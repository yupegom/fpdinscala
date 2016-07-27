package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }
  
  test("set of roots to polynomial"){
    val set = Polynomial.computeSolutions(Var(1), Var(2), Var(1), Var(-1))
    println(s"Result: ${set()}")
    assert(set().isEmpty)

    val set1 = Polynomial.computeSolutions(Var(1), Var(3), Var(1), Polynomial.computeDelta(Var(1), Var(3), Var(1)))
    println(s"Result1: ${set1()}")

    val set2 = Polynomial.computeSolutions(Var(1), Var(1), Var(1), Polynomial.computeDelta(Var(1), Var(1), Var(1)))
    println(s"Result2: ${set2().toString}")
  }

  test("Plus 2 2 = 4"){
    val plus = Calculator.computeValues(Map("a" -> Signal(Plus(Literal(2),Literal(2)))))
    assert(plus("a")() == 4.0)
  }

  test("a=2, b=a+2 = 4"){
    val namedValues: Map[String, Signal[Expr]] = Map("a" -> Signal(Literal(2)), "b" -> Signal(Plus(Ref("a"), Literal(2))))
    val plus = Calculator.computeValues(namedValues)
    assert(plus("b")() == 4.0)
  }

  test("a=b+1, b=2*a = NaN"){
    val namedValues: Map[String, Signal[Expr]] = 
      Map("a" -> Signal(Plus(Ref("b"), Literal(2))), "b" -> Signal(Times(Ref("a"), Literal(2))))
    val plus = Calculator.computeValues(namedValues)
    assert(plus("b")().isNaN)
  }
}
