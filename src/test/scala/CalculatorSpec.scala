import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.io.Source

class CalculatorSpec extends AnyFlatSpec with should.Matchers {

  val fileName = "C:\\CalculatorCommands\\operations.txt"

  "Calculator" should "execute all the commands and return some value" in {
    val calc = new Calculator
    calc.runCalc(fileName)
    calc.opStack.last shouldBe 20
    calc.opStack.tail.popAll()
  }

  "Calculator" should "create seq from file with commands" in {
    val calc = new Calculator
    calc.linesFromFile(fileName) shouldBe Seq("sum 5", "subtract 3", "multiply 10")
  }
  it should "sum values" in {
    val calc = new Calculator
    val sq = Seq("sum 8")
    calc.linesToCmd(sq).foreach(calc.execute)
    calc.opStack.last shouldBe 8
    calc.opStack.tail.popAll()
  }
  it should "subtract values" in{
    val calc = new Calculator
    val sq = Seq("subtract 8")
    calc.linesToCmd(sq).foreach(calc.execute)
    calc.opStack.last shouldBe -8
    calc.opStack.tail.popAll()
  }
  it should "multiply values" in{
    val calc = new Calculator
    val sq = Seq("sum 1", "multiply 8")
    calc.linesToCmd(sq).foreach(calc.execute)
    calc.opStack.last shouldBe 8
    calc.opStack.tail.popAll()
  }

  it should "divide values" in{
    val calc = new Calculator
    val sq = Seq("sum 16", "divide 8")
    calc.linesToCmd(sq).foreach(calc.execute)
    calc.opStack.last shouldBe 2
    calc.opStack.tail.popAll()
  }
  it should "not divide by 0" in{
    val calc = new Calculator
    val sq = Seq("sum 16", "divide 0")
    calc.linesToCmd(sq).foreach(calc.execute)
    calc.opStack.last shouldBe 16
    calc.opStack.tail.popAll()
  }
}
