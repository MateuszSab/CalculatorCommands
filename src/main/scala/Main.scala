import scala.collection.mutable
import scala.io.Source

object Main extends App {
  val fileName = "C:\\CalculatorCommands\\operations.txt"
  val lines = Source.fromFile(fileName).getLines.toSeq
  val commands = lines.map(Commands.from)
  val current = commands.foldLeft(Calculator) { (c, cmd) =>
    Calculator.execute(cmd)
    println(Calculator.opStack.head)
    Calculator
  }
}
object Commands{
  val acceptedComands = Map(
    "sum" -> Sum.from _,
    "subtract" -> Subtract.from _,
    "multiply" -> Multiply.from _,
    "divide" -> Divide.from _
  )
  def from(l: String)={
    val name = l.split("\\s+").head.toLowerCase()
    val fromMethod = (acceptedComands(name))
    fromMethod(l)
  }
}
object Calculator{
  val opStack = mutable.Stack(0)
  def execute(cmd: Product) = cmd match{
    case c: Sum => opStack.addOne( opStack.pop() + c.x)
    case a: Subtract => opStack.addOne(opStack.pop() - a.x)
    case m: Multiply => opStack.addOne(opStack.pop() * m.x)
    case d: Divide => opStack.addOne(opStack.pop() / d.x)
    case n: Negation => opStack.addOne(-opStack.head)
  }
}

case class Sum(x: Int)

object Sum {
  def from(s: String) = {
    val Array(_, x) = s.split("\\s+")
    Sum(x.toInt)
  }
}

case class Subtract(x: Int)

object Subtract {
  def from(s: String) = {
    val Array(_, x) = s.split("\\s+")
    Subtract(x.toInt)
  }
}

case class Multiply(x: Int)

object Multiply {
  def from(s: String) = {
    val Array(_, x) = s.split("\\s+")
    Multiply(x.toInt)
  }
}


case class Divide(x: Int)

object Divide {
  def from(s: String) = {
    val Array(_, x) = s.split("\\s+")
    Divide(x.toInt)
  }
}

case class Negation(n: Int)

object Negation {
  def from(s: String) = {
    val Array(_, x) = s.split("\\s+")
    Negation(x.toInt)
  }
}

case class Print(n: Int)

object Print {
  def from(s: String) = {
    val Array(_, x) = s.split("\\s+")
    Print(x.toInt)
  }
}
