//> using scala 3.3.0

import scala.util.Try
import scala.io.Source

object Fourth {
  private val pattern = "XMAS".r
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
    val resultHV = getCountHV(inputLinesStandard)
    val resultD = getCountD(inputLinesStandard)
    println(resultHV + resultD)
  }

  private def getCountHV(matrix: Array[Array[Char]]): Int = {
    val horizontal =
      matrix.map(row => pattern.findAllIn(row.mkString).length).sum + matrix.map(row => pattern.findAllIn(row.reverse.mkString).length).sum
    val vertical =
      matrix.transpose.map(row => pattern.findAllIn(row.mkString).length).sum +  matrix.transpose.map(row => pattern.findAllIn(row.reverse.mkString).length).sum
    horizontal + vertical 
  }
  private def getCountD(matrix: Array[Array[Char]]) = {
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
      // RTL
      val matrixMirror = matrix.map(_.reverse)
      val rowsM = matrix.length
      val colsM = matrix(0).length
      for (startRow <- 0 until rowsM) {
        val diagonal = (0 until math.min(rowsM - startRow, colsM))
          .map(k => matrixMirror(startRow + k)(k))
          .toArray
        diagonals = diagonals :+ diagonal
      }
      for (startCol <- 1 until colsM) {
        val diagonal = (0 until math.min(rowsM, colsM - startCol))
          .map(k => matrixMirror(k)(startCol + k))
          .toArray
        diagonals = diagonals :+ diagonal
      }
      diagonals.map(arr => println(arr.mkString))
      diagonals.map(row => pattern.findAllIn(row.mkString).length).sum
       + diagonals.map(_.reverse).map(row => pattern.findAllIn(row.mkString).length).sum
  }
}

//should be 18