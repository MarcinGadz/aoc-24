import scala.io.Source
//> using scala 3.3.0

object Second {
  private def isSafe(arr: Array[Int]): Boolean = {
    val safe1 = arr.sliding(2).forall(chunk => chunk(0) - chunk(1) >= 1 && chunk(0) - chunk(1) <= 3 )
    val safe2 = arr.sliding(2).forall(chunk => chunk(1) - chunk(0) >= 1 && chunk(1) - chunk(0) <= 3 )
    safe1 || safe2
  }
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println(
        "You have to provide 1 argument - name of the file with input data"
      )
      System.exit(1)
    }
    val fileName = args(0)
    println(s"reading file $fileName")
    val result = Source
      .fromFile(fileName)
      .getLines()
      .map(_.trim.split("\\s+").flatMap(_.toIntOption))
      .count(isSafe(_))
    println(result)
  }
}
