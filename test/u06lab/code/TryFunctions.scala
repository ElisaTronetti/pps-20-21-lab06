package u06lab.code

import org.junit.jupiter.api.{Assertions, Test}
import Assertions._

class TryFunctions {

  @Test def testSum() {
    val f: Functions = FunctionsImpl
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001)
    assertEquals(0.0, f.sum(List()))
  }

  @Test def testConcat() {
    val f: Functions = FunctionsImpl
    assertEquals("abc", f.concat(Seq("a", "b", "c")))
    assertEquals("", f.concat(Seq()))
  }

  @Test def testMax() {
    val f: Functions = FunctionsImpl
    assertEquals(3, f.max(List(-10, 3, -5, 0)))
    assertEquals(Integer.MIN_VALUE, f.max(List()))
  }

}