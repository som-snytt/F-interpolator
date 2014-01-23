
package object flue {
  import scala.language.experimental.macros

  implicit class Flue(val sc: StringContext) {
    def F(args: Any*): String = macro FlueImpl.flue
  }
}
package flue {

  import org.junit.Test
  import org.junit.Assert._

  class FlueTest {
    @Test def sample(): Unit = {
      //F"${0x1234}%.2x"  // DNC
      //FlueTest.scala:15: Bad format: java.util.IllegalFormatPrecisionException: 2
    }
    @Test def pos(): Unit = {
      val res = F"Position[ %p ]"
      assertTrue(res contains "line-")
      assertTrue(res contains "FlueTest.scala")
    }
    @Test def file(): Unit = {
      val res = F"File[ %Pf ]"
      assertTrue(res contains "FlueTest.scala")
    }
    @Test def column(): Unit = {
      val res = F"Col[ %Pc ]"
      assertEquals("Col[ 17 ]", res)
    }
    @Test def quoted(): Unit = {
      val res = F"hello, so-called %qworld%q."
      assertEquals("""hello, so-called "world".""", res)
    }
    @Test def mixed(): Unit = {
      val x = "Scala"
      val y = "opera house"
      val res = F"%q$x%q is a wonderful $y."
      assertEquals(""""Scala" is a wonderful opera house.""", res)
    }
    @Test def verified(): Unit = {
      assertEquals("She is 4 feet tall.", F"She is ${4}%d feet tall.")
    }
  }
}
