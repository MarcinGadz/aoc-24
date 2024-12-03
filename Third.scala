//> using scala 3.3.0

import scala.util.Try

object Third {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("You have to provide 1 argument - input puzzle")
      System.exit(1)
    }
    val puzzle = args(0)
    val sum = "mul\\(\\d+,\\d+\\)".r
      .findAllIn(puzzle)
      .flatMap { case s"mul($num1,$num2)" =>
        Try(num1.toInt * num2.toInt).toOption
      }
      .sum

    println(sum2)
  }
}
