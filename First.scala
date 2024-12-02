//> using scala 3.3.0

import scala.io.Source

object First {
 def main(args: Array[String]) = {
  if (args.length != 1) {
    println("You have to provide 1 argument - name of the file with input data")
    System.exit(1)
  }
  val fileName = args(0)
  println(s"reading file $fileName")
  val content = Source.fromFile(fileName).getLines()
  val (list1, list2) = content.flatMap{line => line.trim().split("\\s+") match
    case Array(col1, col2) if col1.toIntOption.isDefined && col2.toIntOption.isDefined => Some(col1.toInt -> col2.toInt)
    case _ => None
  }.toList.unzip
  val distance = list1.sorted.zip(list2.sorted).map((v1, v2) => Math.abs(v1 - v2)).sum
  println(s"Result: $distance")
}
}