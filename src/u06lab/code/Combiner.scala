package u06lab.code

/**
  * 1) Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly.
 */
/*
trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

//basic implementation
object FunctionsImpl extends Functions {

  override def sum(a: List[Double]): Double = a.foldLeft(0.0)(_+_)

  override def concat(a: Seq[String]): String = a.foldLeft("")(_+_)

  override def max(a: List[Int]): Int = a.foldLeft(Int.MinValue)(_.max(_))
}
*/


/*
  * 2) To apply DRY principle at the best,
  * note the three methods in Functions do something similar.
  * Use the following approach:
  * - find three implementations of Combiner that tell (for sum,concat and max) how
  *   to combine two elements, and what to return when the input list is empty
  * - implement in FunctionsImpl a single method combiner that, other than
  *   the collection of A, takes a Combiner as input
  * - implement the three methods by simply calling combiner
  *
  * When all works, note we completely avoided duplications..
 */

trait Combiner[A] {
  def unit: A //what to return if the data structure is empty
  def combine(a: A, b: A): A //how to combine the elements if the data structure is not empty
}

object ImplicitCombiner {
  implicit object sumCombiner extends Combiner[Double] {
    override def unit: Double = 0.0
    override def combine(a: Double, b: Double): Double = a + b
  }

  implicit object concatCombiner extends Combiner[String] {
    override def unit: String = ""
    override def combine(a: String, b: String): String = a + b
  }

  implicit object maxCombiner extends Combiner[Int]{
    override def unit: Int = Int.MinValue
    override def combine(a: Int, b: Int): Int = a.max(b)
  }
}

/**
 * using implicit combiner
 */
trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int
}

object FunctionsImpl extends Functions {
  import u06lab.code.ImplicitCombiner._

  override def sum(a: List[Double]): Double = combine(a)

  override def concat(a: Seq[String]): String = combine(a)

  override def max(a: List[Int]): Int = combine(a)

  //actually use strategy, private method that actually takes the correct combiner and applies combine if possible,
  //otherwise do combine
  private def combine[A](elems: Seq[A])(implicit  combiner: Combiner[A]): A = elems.foldLeft(combiner.unit)(combiner.combine)
}