object Sudoku {


  def checkRow(line: Array[Char]): Boolean = {
    val filtered = line.filter(_ != '.')
    println (s"filterd=${filtered.deep.mkString(" ")}")
    if (filtered.length == 0 ) true else
    filtered.distinct.length == filtered.length
  }

  def checkGrid(grid: Array[Array[Char]], x: Int, y: Int): Boolean = {
    println (s"x=$x, y=$y is checking")
    val chars = grid(x).slice(y, y + 3) ++ grid(x + 1).slice(y, y + 3) ++ grid(x + 2).slice(y, y + 3)
    println(chars.deep.mkString("\n"))
    checkRow(chars)
  }


  def sudoku2(grid: Array[Array[Char]]): Boolean = {
    grid.forall(checkRow) &&
    grid.transpose.forall(checkRow) &&
    checkGrid(grid, 0, 0) && checkGrid(grid, 0, 3) && checkGrid(grid, 0, 6) &&
    checkGrid(grid, 3, 0) && checkGrid(grid, 3, 3) && checkGrid(grid, 3, 6) &&
    checkGrid(grid, 6, 0) && checkGrid(grid, 6, 3) && checkGrid(grid, 6, 6)

  }


  def main(args: Array[String]): Unit = {
    println(sudoku2(null))
  }


}
