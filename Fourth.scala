//> using scala 3.3.0

import scala.util.Try
import scala.io.Source

object Fourth {
  type Matrix = Array[Array[Char]]
  private val pattern = "MAS".r
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("You have to provide 1 argument - file with input puzzle")
      System.exit(1)
    }
    val fileName = args(0)
    println(s"reading file $fileName")
    val input = Source
      .fromFile(fileName)
      .getLines()
      .map(_.toCharArray())
    val inputLinesStandard = input.toArray
    val parts = getSubMatrixes(inputLinesStandard)

    val resultD = parts
      .map(part => getCountD(part) + getCountD(part.map(_.reverse)))
      .filter(psum => psum == 2)
      .sum / 2
    println(resultD)
  }

  private def findCountInString(str: String): Int = pattern.findAllIn(str).length
  private def getSubMatrixes(matrix: Matrix): List[Matrix] = {
    val rows = matrix.length
    val cols = matrix(0).length
    (for {
      i <- 0 to rows - 3
      j <- 0 to cols - 3
    } yield Array(
      matrix(i).slice(j, j + 3),
      matrix(i + 1).slice(j, j + 3),
      matrix(i + 2).slice(j, j + 3)
    )).toList
  }

  private def getCountD(matrix: Matrix) = {
    var diagonals = List[Array[Char]]()
    val rows = matrix.length
    val cols = matrix(0).length
    // LTR
    for (startRow <- 0 until rows) {
      val diagonal = (0 until math.min(rows - startRow, cols))
        .map(k => matrix(startRow + k)(k))
        .toArray
      diagonals = diagonals :+ diagonal
    }
    for (startCol <- 1 until cols) {
      val diagonal = (0 until math.min(rows, cols - startCol))
        .map(k => matrix(k)(startCol + k))
        .toArray
      diagonals = diagonals :+ diagonal
    }
    diagonals.map(row =>findCountInString(row.mkString)).sum
      + diagonals
        .map(_.reverse)
        .map(row => findCountInString(row.mkString))
        .sum
  }
}
