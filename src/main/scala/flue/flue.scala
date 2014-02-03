
package flue

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.contexts.{ Context => MacroContext }
import scala.util.matching.Regex.Match

import java.lang.reflect.InvocationTargetException
import java.util.{ Formatter, IllegalFormatException }

class Fextensions(val c: Context) {
  import c.universe.{ Match => _, _ }

  def fail(msg: String) = c.abort(c.enclosingPosition, msg)

  /** Extensions to the standard f-interpolator.
   */
  def impl(args: c.Expr[Any]*): c.Expr[String] = {

    val t = c.prefix.tree match {
      case q"$_($_(..$tparts))" =>
        val parts = downConvert(tparts)
        val f = interpolate(parts, args: _*)
        check(f, args: _*)
        f
      case other =>
        c.error(c.prefix.tree.pos, s"Unexpected prefix ${showRaw(other)}")
        c.macroApplication
    }
    c.Expr[String](t)
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
  def interpolate(parts: List[Tree], args: Expr[Any]*): Tree = {
    val myc = c.asInstanceOf[MacroContext]
    val v = new Verify(myc)
    val myparts = parts.asInstanceOf[List[v.c.universe.Tree]]
    val myargs = (args map (_.tree)).toList.asInstanceOf[List[v.c.universe.Tree]]
    val p = c.macroApplication.pos.asInstanceOf[reflect.internal.util.Position]
    v.f(myparts, myargs, p).asInstanceOf[Tree]
  }
  def check(f: Tree, args: Expr[Any]*) = {
    val lcs = f match {
      //case q"{ ..$_ ; $lcs.format($_) }" => lcs
      case Block(_, Apply(Select(lcs, _), _)) => lcs
      case _ => fail(f"Unexpected tree from f%n${showRaw(f)}")
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

  // try the format string with arbitrary default values
  // TODO formattable, etc
  def attemptApplyingDefaults(s: String, args: Expr[Any]*) = {
    val u = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
    val d = definitions.asInstanceOf[u.DefinitionsClass]
    import d._
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
    s.format(args map defaultOf : _*)
    true
  }

  /** Attempt to verify the format string by invoking
   *  the private `parse` method, available on OpenJDK.
   */
  def attemptFormatParse(s: String): Boolean = {
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
