val input = io.Source.fromResource("day08.txt").mkString

val lines = input.split("\n").toList

val width = 50
val height = 6

val y = transform(lines, Set()).toList.sorted
y.size

val msg = (0 until (height * width)).map(k => if (y.contains(k)) "#" else ".").grouped(width)

println(msg.mkString("\n"))
// EFEYKFRFIJ

def transform(lines: Seq[String], display: Set[Int]): Set[Int] = {

  if (lines.isEmpty) return display

  val splitted = lines.head.split(" ")

  if (splitted.contains("rect")) {
    val a = splitted.last.split("x")
    transform(lines.tail, mkRect(display, a.head.toInt, a.last.toInt))
  } else {
    val by = splitted.last.toInt
    val id = splitted(2).tail.tail.toInt

    if (splitted.contains("column"))
      transform(lines.tail, rotateCol(display, id, by))
    else
      transform(lines.tail, rotateRow(display, id, by))
  }
}

def mkRect(display: Set[Int], col: Int, row: Int) = {
  display ++ (for (i <- 0 until col; j <- 0 until (row * width) by width) yield i + j).filter(_ < width * height).toSet
}

def rotateCol(display: Set[Int], col: Int, by: Int) = {
  val colElems = display.filter(_ % width == col)
  display.diff(colElems) ++ colElems.map(_ + by * width).map(_ % (height * width))
}

def rotateRow(display: Set[Int], row: Int, by: Int) = {
  val rowElems = display.filter(e => e >= row * width && e < (row + 1) * width)
  val shouldJump = (x: Int) => x > (width * (row + 1)) - 1
  val jump = (x: Int) => row * width + x % ((row + 1) * width)
  val transformed = rowElems.map(_ + by).map(e => if (shouldJump(e)) jump(e) else e)
  display.diff(rowElems) ++ transformed
}