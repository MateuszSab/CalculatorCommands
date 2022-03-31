import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.io.Source

class CalculatorSpec extends AnyFlatSpec with should.Matchers {

  val fileName = "C:\\CalculatorCommands\\operations.txt"
  val lines = Source.fromFile(fileName).getLines.toSeq
  val commands = lines.map(Commands.from)

  "Calculator " should "execute all the commands and return some value" in {
    commands.foreach {
      cmd =>
        Calculator.execute(cmd)
    }
    Calculator.opStack.last shouldBe -5
  }


}
