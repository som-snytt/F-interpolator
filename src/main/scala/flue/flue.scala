
package flue

import scala.language.experimental.macros
import scala.PartialFunction.cond
import scala.reflect.macros.{ WhiteboxContext => Context }
import scala.reflect.macros.contexts.{ Context => MacroContext }
//import scala.tools.reflect.MacroImplementations
import scala.collection.mutable.{ ListBuffer, Stack }
import scala.util.matching.Regex.Match
import scala.tools.reflect.StdContextTags

import java.lang.reflect.InvocationTargetException
import java.util.{ Formatter, IllegalFormatException }

object FlueImpl {
  @inline private def truly(body: => Unit): Boolean = { body ; true }
  @inline private def falsely(body: => Unit): Boolean = !truly(body)

  /** Extensions to the standard f-interpolator.
   */
  def flue(c: Context)(args: c.Expr[Any]*): c.Expr[String] = {
    val myc = c.asInstanceOf[MacroContext]
    val myargs = args.asInstanceOf[Seq[myc.Expr[Any]]]
    _flue(myc)(myargs: _*).asInstanceOf[c.Expr[String]]
  }
  // Forward the call to preserve param names.
  // Downcast for StdContextTags.
  private def _flue(c: MacroContext)(args: c.Expr[Any]*): c.Expr[String] = {
    import c.universe.{ Match => _, _ }
    import definitions._
    def fail(msg: String) = c.abort(c.enclosingPosition, msg)

    // part match {
    // case r"""%$index(\d+\$)?$flags([-#+ 0,(\<]*)?$width(\d+)?$precision(\.\d+)?$time([tT])?$op([%a-zA-Z])"""
    // => Conversion(index map (_.toInt), flags, width map (_.toInt), precision map (_.drop(1).toInt), time, op)
    // match on (cc)? to notice missing cc's.
    val fpat = """%(?:(\d+)\$)?([-#+ 0,(\<]+)?(\d+)?(\.\d+)?((?:[tT])?[%a-zA-Z])?""".r

    val tagOfFormattable = typeTag[java.util.Formattable]

    // None if subtypes none
    def checkType0(arg: Tree, variants: Type*): Option[Type] =
      variants find (arg.tpe <:< _)
    // None if conforming to none
    def checkType1(arg: Tree, variants: Type*): Option[Type] =
      checkType0(arg, variants: _*) orElse (
        variants find (c.inferImplicitView(arg, arg.tpe, _) != EmptyTree)
      )
    // require variants.nonEmpty
    def checkType(arg: Tree, variants: Type*): Option[Type] =
      checkType1(arg, variants: _*) orElse Some(variants(0))

    val stdContextTags = new { val tc: c.type = c } with StdContextTags
    import stdContextTags._

    // don't the types have to be in widening order?
    // byte and short always wide to int, so it'll always choose int?
    def conversionType(ch: Char, arg: Tree): Option[Type] = {
      ch match {
        case 'b' | 'B' =>
          if (arg.tpe <:< NullTpe) Some(NullTpe) else Some(BooleanTpe)
        case 'h' | 'H' =>
          Some(AnyTpe)
        case 's' | 'S' =>
          Some(AnyTpe)
        case 'c' | 'C' =>
          checkType(arg, CharTpe, ByteTpe, ShortTpe, IntTpe)
        case 'd' | 'o' | 'x' | 'X' =>
          checkType(arg, IntTpe, LongTpe, ByteTpe, ShortTpe, tagOfBigInt.tpe)
        case 'e' | 'E' | 'g' | 'G' | 'f' | 'a' | 'A'  =>
          checkType(arg, DoubleTpe, FloatTpe, tagOfBigDecimal.tpe)
        case 't' | 'T' =>
          checkType(arg, LongTpe, tagOfCalendar.tpe, tagOfDate.tpe)
        case _ => None
      }
    }
    class Conversion(m: Match, pos: Position) {
      private def maybeStr(i: Int) = Option(m.group(i))
      private def maybeInt(i: Int) = maybeStr(i) map (_.toInt)
      val index: Option[Int]     = maybeInt(1)
      val flags: Option[String]  = maybeStr(2)
      val width: Option[Int]     = maybeInt(3)
      val precision: Option[Int] = maybeStr(4) map (_.drop(1).toInt)
      val op: String             = maybeStr(5) getOrElse ???

      def indexed:   Boolean = index.nonEmpty || hasFlag('<')
      def isLiteral: Boolean = false
      def verify:    Boolean = {
        def goodFlags = flags map (_ forall (okFlags contains _)) getOrElse true
        def goodIndex = {
          if (index.nonEmpty && hasFlag('<')) {
            c.warning(groupPos(1), "Argument index ignored if '<' flag is present")
          }
          if (index.getOrElse(0) > args.size && !hasFlag('<')) {
            c.error(groupPos(1), "Argument index out of range")
            false
          } else true
        }
        goodFlags && goodIndex
      }

      def accepts(arg: Tree): Option[Type] = conversionType(op(0), arg)

      protected def okFlags: String = ""
      val allFlags = "-#+ 0,(<"
      def hasFlag(f: Char) = flags.fold(false)(_ contains f)
      def hasAnyFlag(fs: String) = fs exists (hasFlag)
      def cc: Char = if ("tT" contains op(0)) op(1) else op(0)

      def badFlag(f: Char, msg: String) = c.error(flagPos(f), msg)
      def flagPos(f: Char) = pos withPoint (pos.point + m.start(2) + flags.get.indexOf(f))
      def groupPos(i: Int) = pos withPoint (pos.point + m.start(i))

      def noPrecision = precision.isEmpty || falsely {
        c.error(groupPos(4), "precision not allowed")
      }
      def noWidth = width.isEmpty || falsely {
        c.error(groupPos(3), "width not allowed")
      }
      def noFlags = flags.isEmpty || falsely {
        c.error(groupPos(2), "flags not allowed")
      }
    }
    trait GeneralXn extends Conversion {
      override def accepts(arg: Tree) = (
        if (flags.fold(false)(_ contains '#') && (op == "s")) checkType(arg, tagOfFormattable.tpe)
        else super.accepts(arg)
      )
      override protected def okFlags = cc match {
        case 's' | 'S' => "-#"
        case _         => "-"
      }
    }
    trait LiteralXn extends Conversion {
      override val isLiteral = true
      override def verify = op match {
        case "%" => super.verify && noPrecision
        case "n" => noFlags && noWidth && noPrecision
      }
      override protected val okFlags = "-"
    }
    trait CharacterXn extends Conversion {
      override def verify = {
        val bad = ((flags getOrElse "") filterNot (_ == '-')).headOption
        cond(bad) {
          case Some(f) => badFlag(f, "Only '-' allowed for c conversion") ; false
          case None    => true
        }
      }
    }
    trait IntegralXn extends Conversion {
      override def verify = {
        def d_# = {
          val e = cc == 'd' && hasFlag('#')
          if (e) badFlag('#', "# not allowed for d conversion")
          e
        }
        def x_comma = {
          val e = cc != 'd' && hasFlag(',')
          if (e) badFlag(',', "',' only allowed for d conversion of integral types")
          e
        }
        super.verify && noPrecision && !d_# && !x_comma
      }
      override def accepts(arg: Tree) = {
        def isBigInt = checkType0(arg, tagOfBigInt.tpe).nonEmpty
        def bad_+ = {
          val e = cond(cc) { case 'o' | 'x' | 'X' if hasAnyFlag("+ (") => !isBigInt }
          if (e) {
            val badf = ("+ (" filter (hasFlag)).head
            badFlag(badf, s"only use '$badf' for BigInt conversions to o, x, X")
          }
          e
        }
        if (bad_+) None
        else super.accepts(arg)
      }
      override protected val okFlags = allFlags
    }
    trait FloatingPointXn extends Conversion {
      override def verify = cc match {
        case 'a' | 'A' =>
          def aBadFlag = (",(" filter (hasFlag)).headOption
          noPrecision && aBadFlag.isEmpty && falsely {
            badFlag(aBadFlag.get, s"'$aBadFlag' not allowed for a, A")
          }
        case _ => true
      }
    }
    trait DateTimeXn extends Conversion {
      def goodCC =
        if ("HIklMSLNpzZsQBbhAaCYyjmdeRTrDFc" contains cc) true
        else {
          badFlag(cc, s"'$cc' doesn't seem to be a date or time conversion")
          false
        }
      def no_# = (
        if (hasFlag('#')) {
          badFlag('#', "'#' disallowed for dates and times")
          false
        } else true
      )
      override def verify = goodCC && noPrecision && no_#
    }
    object Converter {
      def apply(m: Match, p: Position): Option[Conversion] = {
        def nocc = {
          c.error(p, s"Missing conversion operator in '${m.matched}'")
          "_"
        }
        val cc = (Option(m group 5) getOrElse nocc)(0)
        val cv = cc match {
          case 'b' | 'B' | 'h' | 'H' | 's' | 'S' =>
            new Conversion(m, p) with GeneralXn
          case 'c' | 'C' =>
            new Conversion(m, p) with CharacterXn
          case 'd' | 'o' | 'x' | 'X' =>
            new Conversion(m, p) with IntegralXn
          case 'e' | 'E' | 'f' | 'g' | 'G' | 'a' | 'A' =>
            new Conversion(m, p) with FloatingPointXn
          case 't' | 'T' =>
            new Conversion(m, p) with DateTimeXn
          case '%' | 'n' =>
            new Conversion(m, p) with LiteralXn
          case _ => null
        }
        Option(cv) filter (_.verify)
      }
    }

    /** As for standard f-interpolator, with more verification drudgery.
     *  Every part except the first must begin with a conversion for
     *  the arg that preceded it. If the conversion is missing, "%s"
     *  is inserted.
     *
     *  In any other position, the only permissible conversions are
     *  the literals (%% and %n).
     *
     *  A conversion specifier has the form:
     *
     *  [index$][flags][width][.precision]conversion
     *
     *  Notes:
     *  "Duke's Birthday: %tm %2$<te, %<tY".format(now, again)
     *  should complain about conflict between arg index and < flag.
     *  "%5%".format()
     *  should warn that width is ignored, output is not "%"*5
     */
    def interpolate(parts: List[Tree]): Tree = {
      def badlyInvoked = (parts.length != args.length + 1) && truly {
        def because(s: String) = s"too $s arguments for interpolated string"
        val (p, msg) =
          if (parts.length == 0) (c.prefix.tree.pos, "there are no parts")
          else if (args.length + 1 < parts.length)
            (if (args.isEmpty) c.enclosingPosition else args.last.tree.pos, because("few"))
          else (args(parts.length-1).tree.pos, because("many"))
        c.abort(p, msg)
      }
      def interpolated = {
        val bldr     = new StringBuilder
        val evals    = ListBuffer[ValDef]()
        val ids      = ListBuffer[Ident]()
        val argStack = Stack(args: _*)

        // create a tmp val and add it to the ids passed to format
        def defval(value: Tree, tpe: Type): Unit = {
          val freshName = TermName(c.freshName("arg$"))
          evals += {
            val t = ValDef(Modifiers(), freshName, TypeTree(tpe) setPos value.pos.focus, value)
            t setPos value.pos
            t
          }
          ids += Ident(freshName)
        }

        // Append the current part to the string builder.
        def copyPart(part: Tree, first: Boolean): Unit = {
          def errorAt(idx: Int, msg: String) = c.error(part.pos withPoint (part.pos.point + idx), msg)
          def nonEscapedPercent(idx: Int) =
            errorAt(idx, "conversions must follow a splice; use %% for literal %, %n for newline")

          val s0 = part match {
            case Literal(Constant(x: String)) => x
            case _ => throw new IllegalArgumentException("internal error: argument parts must be a list of string literals")
          }
          val s  = StringContext.treatEscapes(s0)
          val ms = fpat findAllMatchIn s
          // a conversion for the arg is required
          if (!first) {
            val arg = argStack.pop()
            def s_%(): Unit = {
              bldr append "%s"
              defval(arg.tree, AnyTpe)
            }
            def accept(op: Conversion) = op.accepts(arg.tree) match {
              case Some(tpe) => defval(arg.tree, tpe)
              case None =>
            }
            if (ms.hasNext) {
              val m = ms.next
              Converter(m, part.pos) match {
                case Some(op) if op.isLiteral || op.indexed => s_%()
                case Some(op) => accept(op)
                case None     =>
              }
            } else {
              s_%()
            }
          }
          // any remaining conversions must be either literals or indexed
          while (ms.hasNext) {
            val m = ms.next
            Converter(m, part.pos) match {
              case Some(op) if first && op.hasFlag('<')   => op.badFlag('<', "No last arg")
              case Some(op) if op.isLiteral || op.indexed => // OK
              case Some(op) => nonEscapedPercent(m.start)
              case None     =>
            }
          }
          bldr append s
        }

        copyPart(parts.head, first = true)
        for (part <- parts.tail) copyPart(part, first = false)

        val fstring = bldr.toString
        val expr =
          Apply(
            Select(
              Literal(Constant(fstring)),
              TermName("format")),
            List(ids: _* )
          )
        val where = c.macroApplication.pos
        Block(evals.toList, atPos(where.focus)(expr)) setPos where.makeTransparent
        //q"..evals, ${Literal(Constant(fstring))}.format(..$ids)"
      }
      if (badlyInvoked) EmptyTree else interpolated
    }
    /* Convert enhanced conversions to something format likes.
     * %q for quotes, %p for position, %Pf for file, %Pn line number,
     * %Pc column %Po offset.
     */
    def downConvert(parts: List[Tree]): List[Tree] = {
      def fixup(t: Tree): Tree = {
        val Literal(Constant(s: String)) = t
        val r = "(?<!%)%(p|q|Pf|Po|Pn|Pc)".r
        def p = c.macroApplication.pos
        def f(m: Match): String = m group 1 match {
          case "p"  => p.toString
          case "q"  => "\""
          case "Pf" => p.source.file.name
          case "Po" => p.point.toString
          case "Pn" => p.line.toString
          case "Pc" => p.column.toString
        }
        val z = r.replaceAllIn(s, f _)
        Literal(Constant(z)) setPos t.pos
      }
      parts map (x => fixup(x))
    }
    def check(f: Tree) = {
      val lcs = f match {
        //case q"{ $_ ; $lcs.format($_) }"             => lcs
        case Block(_, Apply(Select(lcs, _), _)) => lcs
        case _ => fail(f"Unexpected tree from f%n${showRaw(f)}")
      }
      //val u = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
      //val d = definitions.asInstanceOf[u.DefinitionsClass]
      def defaultOf(x: c.Expr[Any]): Any = x.tree.tpe.typeSymbol match {
        case UnitClass    => ()
        case BooleanClass => false
        case FloatClass   => 0.0f
        case DoubleClass  => 0.0d
        case ByteClass    => 0.toByte
        case ShortClass   => 0.toShort
        case IntClass     => 0
        case LongClass    => 0L
        case CharClass    => 0.toChar
        case _              => new AnyRef
      }
      // try the format string with arbitrary default values
      def attemptApplyingDefaults(s: String) = {
        s.format(args map defaultOf : _*)
        true
      }
      val Literal(Constant(s: String)) = lcs
      def msg(e: Throwable) = s"Bad format: ${e.getClass.getName}: ${e.getMessage}"
      try {
        if (!attemptFormatParse(s) && !attemptApplyingDefaults(s))
          c.warning(c.macroApplication.pos, s"Could not verify format `$s`")
      } catch {
        case e: IllegalFormatException => c.error(c.macroApplication.pos, msg(e))
      }
    }
    val t = c.prefix.tree match {
      case q"$_($_(..$tparts))" =>
        val parts = downConvert(tparts)
        val f = interpolate(parts)
        check(f)
        f
      case other =>
        c.error(c.prefix.tree.pos, s"Unexpected prefix ${showRaw(other)}")
        c.macroApplication
    }
    c.Expr[String](t)
  }

  /** Attempt to verify the format string by invoking
   *  the private `parse` method, available on OpenJDK.
   */
  private def attemptFormatParse(s: String): Boolean = {
    import scala.reflect.runtime._
    import universe._
    val im = currentMirror reflect (new Formatter)
    im.symbol.asClass.typeSignature member TermName("parse") match {
      case NoSymbol => false
      case mp =>
        val mm = im reflectMethod mp.asMethod
        try {
          mm(s)
          true
        } catch {
          case e: InvocationTargetException if e.getCause.isInstanceOf[IllegalFormatException] =>
            throw e.getCause
          case e: InvocationTargetException => false
        }
    }
  }
}
