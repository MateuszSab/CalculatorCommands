import scala.collection.mutable
import scala.io.Source

object Main extends App {
  val fileName = "C:\\CalculatorCommands\\operations.txt"
  val calc = new Calculator
  calc.runCalc(fileName)
//    println(cmd)
//    println(Calculator.opStack.head)
//    Calculator

  println("ostatni element " + calc.opStack.head)
}
object Commands{
  val acceptedComands = Map(
    "sum" -> Sum.from _,
    "subtract" -> Subtract.from _,
    "multiply" -> Multiply.from _,
    "divide" -> Divide.from _,
    "negation" -> Negation.from _,
    "print" -> Print.from _
  )
  def from(l: String)={
    val name = l.split("\\s+").head.toLowerCase()
    val fromMethod = (acceptedComands(name))
    fromMethod(l)
  }
}

class Calculator{
  def runCalc(name: String)={
    linesToCmd(linesFromFile(name)).foreach{(cmd)=>
      execute(cmd)
    }
  }
  def linesFromFile(name: String) =  {
    Source.fromFile(name).getLines.toSeq
  }
  def linesToCmd(ss: Seq[String]) = {
    ss.map(Commands.from)
  }
  val opStack = mutable.Stack(0)
  def execute(cmd: Product) = cmd match{
    case c: Sum => opStack.addOne( opStack.last + c.x)
    case a: Subtract => opStack.addOne(opStack.last - a.x)
    case m: Multiply => opStack.addOne(opStack.last * m.x)
    case d: Divide => opStack.addOne(opStack.last / d.x)
    case n: Negation => opStack.addOne(-opStack.last)
    case p: Print => println(opStack.last)
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
    if (x.toInt != 0) {
      Divide(x.toInt)
    }else{
      println("Cannot divide by 0")
      Divide(1)
    }
  }
}

case class Negation()

object Negation {
  def from(s: String) = {
    val Array(_) = s.split("\\s+")
    Negation()
  }
}

case class Print()

object Print {
  def from(s: String) = {
    val Array(_) = s.split("\\s+")
    Print()
  }
}
