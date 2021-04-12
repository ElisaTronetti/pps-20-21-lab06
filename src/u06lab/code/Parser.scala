package u06lab.code

abstract class Parser[T] {
  def parse(t: T): Boolean  // is the token accepted?
  def end(): Boolean        // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean = (seq forall parse) & end()
}

class BasicParser(chars: Set[Char]) extends Parser[Char] {
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end(): Boolean = true
}

trait NonEmpty[T] extends Parser[T]{
  private[this] var empty = true
  abstract override def parse(t: T): Boolean = {empty = false; super.parse(t)}
  abstract override def end(): Boolean = !empty && {empty = true; super.end()}
}

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]{
  private[this] var first: Option[T] = None

  abstract override def parse(t: T): Boolean = first match {
    case Some(x) if t==x => false
    case _ => first = Some(t); super.parse(t)
  }
  abstract override def end(): Boolean = super.end()
}

class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]

//implicit conversion if found a string return a parser with the string converted to chars
object ConvertToChar {
  implicit class StringParser(str: String) {
    def charParser(): Parser[Char] = new BasicParser(Set.from(str.toCharArray))
  }
}