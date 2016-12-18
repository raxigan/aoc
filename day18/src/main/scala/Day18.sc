val input = io.Source
  .fromInputStream(getClass.getResourceAsStream("input.txt")).mkString
val rowsNo = 400000

//val rowsNo = 40
//val input = ".^^.^.^^^^"
//val rowsNo = 10

allTileRows(input, new StringBuilder(input)).count(_ == '.')

def allTileRows(prev: String, acc: StringBuilder, currNo: Int = 1): String = {

  def prevTiles(ix: Int) = {
    def tailForIx = (x: Int) => if (x == -1 || x == prev.length) '.' else prev(x)

    (tailForIx(ix - 1), prev(ix), tailForIx(ix + 1))
  }

  def newTiles(p: (Char, Char, Char)) = if (p._1 == p._3) '.' else '^'

  val newTileRow = prev.indices.map(i => newTiles(prevTiles(i))).mkString
  if (currNo == rowsNo) acc.toString()
  else allTileRows(newTileRow, acc.append(newTileRow), currNo + 1)
}