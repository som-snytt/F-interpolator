
package flue

import scala.language.experimental.macros
import scala.reflect.macros.{ WhiteboxContext => Context }
import scala.tools.reflect.MacroImplementations

import java.lang.reflect.InvocationTargetException
import java.util.{ Formatter, IllegalFormatException }

object FlueImpl {
  /** Extensions to the standard f-interpolator.
   */
  def flue(c: Context)(args: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._
    def fail(msg: String) = c.abort(c.enclosingPosition, msg)
    def interpolate(parts: List[Tree]): c.universe.Tree = {
      val f = new { val c0 = c } with MacroImplementations {
        override val c = c0.asInstanceOf[scala.reflect.macros.contexts.Context]
      }
      val tparts = parts.asInstanceOf[List[f.c.universe.Tree]]
      val targs = (args.toList map (_.tree)).asInstanceOf[List[f.c.universe.Tree]]
      val p = c.macroApplication.pos.asInstanceOf[f.c.universe.Position]
      val t = f.macro_StringInterpolation_f(tparts, targs, p)
      t.asInstanceOf[Tree]
    }
    /* Convert enhanced conversions to something format likes.
     * %q for quotes, %p for position, %Pf for file, %Pn line number,
     * %Pc column %Po offset.
     */
    def downConvert(parts: List[Tree]): List[Tree] = {
      import scala.util.matching.Regex.Match
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
      val u = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
      val d = definitions.asInstanceOf[u.DefinitionsClass]
      def defaultOf(x: c.Expr[Any]): Any = x.tree.tpe.typeSymbol match {
        case d.UnitClass    => ()
        case d.BooleanClass => false
        case d.FloatClass   => 0.0f
        case d.DoubleClass  => 0.0d
        case d.ByteClass    => 0.toByte
        case d.ShortClass   => 0.toShort
        case d.IntClass     => 0
        case d.LongClass    => 0L
        case d.CharClass    => 0.toChar
        case _              => new AnyRef
      }
      // try the format string with arbitrary default values
      def attemptApplyingDefaults(s: String) = {
        s.format(args map defaultOf)
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
