
package object flue {
  import scala.language.experimental.macros

  implicit class Flue(val sc: StringContext) {
    def F(args: Any*): String = macro Fextensions.impl
  }
}
package flue {

  import org.junit.Test
  import org.junit.Assert._

  class FlueRegressionTest {
    @Test def simple(): Unit = {
      assertEquals("123", F"123")
      assertEquals("%", F"%%")
      assertEquals("123", F"${123}%d")
      assertEquals("       0X4", F"${4}%#10X")
      assertEquals("10,000", F"${10000}%,d")
      assertEquals("7 7 9", F"${7}%d %<d ${9}%d")
      assertEquals("7 7 9", F"${7}%d %1$$d ${9}%d")
    }
    @Test def DNC_stringy(): Unit = {
      //F"${"hi"}%+s"
    }
    @Test def DNC_integral(): Unit = {
      //F"${4}%2.2d"
//[error] FlueTest.scala:21: precision not allowed
//[error]       F"${4}%2.2d"
//[error]               ^
      //F"${4}%-#10d"
//[error] FlueTest.scala:25: # not allowed for d conversion
//[error]       F"${4}%-#10d"
//[error]               ^
      //F"${4}%#+10X"
//[error] FlueTest.scala:30: only use '+' for BigInt conversions to o, x, X
//[error]       F"${4}%#+10X"
//[error]               ^
      //F"${4}%,10000x"
//[error] FlueTest.scala:35: ',' only allowed for d conversion of integral types
//[error]       F"${4}%,10000x"
//[error]              ^
      //F"%<d ${9}%d"
//[error] /home/apm/projects/F-interpolator/src/test/scala/flue/FlueTest.scala:40: No last arg
//[error]       F"%<d ${9}%d"
//[error]          ^
      //F"${8}%d ${9}%3$$d"
//[error] FlueTest.scala:45: Argument index out of range
//[error]       F"${8}%d ${9}%3$$d"
//[error]                     ^
    }
  }
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
