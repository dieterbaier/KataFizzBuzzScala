package dieterbaier.kata

import org.scalatest.FlatSpec
import java.io.PrintStream
import java.io.OutputStream
import java.io.ByteArrayOutputStream
import java.util.regex.Pattern
import scala.util.matching.Regex

class FizzBuzzTest extends FlatSpec {

  behavior of "Printed Numbers from 1 to 100"
  it must "have been printing 100 lines" in { checkFor(System.getProperty("line.separator"), 1, 100, 100) }
  it must "have converted 34 numbers to 'Fizz'" in { checkFor("\\bFizz\\b", 1, 100, 34) }
  it must "have converted 22 numbers to 'Buzz'" in { checkFor("\\bBuzz\\b", 1, 100, 22) }
  it must "have converted 6 numbers to 'FizzBuzz'" in { checkFor("\\bFizzBuzz\\b", 1, 100, 6) }
  it must "have converted 38 numbers to its string-representation" in { checkFor("\\d+", 1, 100, 38) }

  "A number containing a 3" must " be converted to Fizz" in {
    assert(convert(13) === "Fizz")
  }

  "A number devisable by 3" must " be converted to 'Fizz'" in {
    assert(convert(3) === "Fizz")
  }
  "A number containing a 5" must " be converted to Buzz" in {
    assert(convert(51) === "Buzz")
  }

  "A number devisable by 5" must " be converted to 'Buzz'" in {
    assert(convert(5) === "Buzz")
  }

  "A number devisable by 3 and 5" must " be converted to 'FizzBuzz'" in {
    assert(convert(15) === "FizzBuzz")
  }

  "A number not devisable by 3 and 5" must " must only be converted to its string representation" in {
    assert(convert(4) === "4")
  }

  private def checkFor(pattern: String, start: Int, end: Int, expectedCount: Int) {
    val baos = new ByteArrayOutputStream()
    System.setOut(new PrintStream(baos))
    printLines(start, end, System.out)
    assert(count(pattern, baos.toString()) === expectedCount)
  }

  private def count(pattern: String, text: String): Int = {
    val parser = pattern.r.findAllMatchIn(text)
    var counter = 0
    while (parser.hasNext) {
      parser.next
      counter = counter.+(1)
    }
    return counter
  }

  def printLines(start: Int, end: Int, stream: PrintStream) {
    for (a <- start to end)
      stream.println(convert(a));
  }

  private def convert(number: Int): String = {
    if (isFizzBuzzCandidate(number))
      return "FizzBuzz"
    val numberAsString = number.toString()
    if (isBuzzCandidate(number, numberAsString))
      return ("Buzz")
    if (isFizzCandidate(number, numberAsString))
      return "Fizz"
    return numberAsString;
  }

  private def isFizzBuzzCandidate(number: Int): Boolean = {
    return isNumberDevisableByDigit(number, 3) && isNumberDevisableByDigit(number, 5)
  }

  private def isBuzzCandidate(number: Int, numberAsString: String): Boolean = {
    isNumberDevisableByDigit(number, 5) || numberAsString.contains("5")
  }

  private def isFizzCandidate(number: Int, numberAsString: String): Boolean = {
    isNumberDevisableByDigit(number, 3) || numberAsString.contains("3")
  }

  private def isNumberDevisableByDigit(number: Int, digit: Int): Boolean = {
    number % digit == 0
  }
}
