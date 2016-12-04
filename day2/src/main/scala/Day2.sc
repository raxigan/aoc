//val input = "ULL\nRRDDD\nLURDL\nUUUUD"

val input = io.Source
  .fromInputStream(getClass.getResourceAsStream("input.txt")).mkString

val keypad1 = (1 to 9).toList
val keypad2 = List(0, 0, 1, 0, 0) :::
  List(0, 2, 3, 4, 0) :::
  (5 to 9).toList :::
  List(0, "A", "B", "C", 0) :::
  List(0, 0, "D", 0, 0)

val instrLines: List[String] = input.split("\n").toList
val kp1Sqrt = Math.sqrt(keypad1.size).toInt

val cantUp = (0 until kp1Sqrt).toList
val cantDown = ((keypad1.size - kp1Sqrt) until keypad1.last).toList
val cantLeft = keypad1.filter(_ % kp1Sqrt == 1).map(_ - 1)
val cantRight = keypad1.filter(_ % kp1Sqrt == 0).map(_ - 1)

val cantUp2 = List(2, 6, 8, 10, 14)
val cantDown2 = List(10, 14, 16, 18, 22)
val cantLeft2 = List(2, 6, 10, 16, 22)
val cantRight2 = List(2, 8, 14, 18, 22)


cantUp2.map(keypad2(_))
cantDown2.map(keypad2(_))
cantLeft2.map(keypad2(_))
cantRight2.map(keypad2(_))

def bathroomCode(keypad: List[Any], startPos: Int, cantUp: List[Int], cantDown: List[Int],
                 cantLeft: List[Int], cantRight: List[Int]): String = {

  val sqrt = Math.sqrt(keypad.size).toInt

  def code(currPos: Int, instr: List[String]): List[Int] = {
    if (instr.isEmpty) List()
    else {
      val newPos = pos(currPos, instr.head)
      newPos :: code(newPos, instr.tail)
    }
  }

  def pos(currIndex: Int, letter: String): Int = {
    if (letter.isEmpty) currIndex else
      letter.head match {
        case 'U' => pos(up(currIndex), letter.tail)
        case 'D' => pos(down(currIndex), letter.tail)
        case 'L' => pos(left(currIndex), letter.tail)
        case 'R' => pos(right(currIndex), letter.tail)
      }
  }

  def up(pos: Int) = move(pos, cantUp, x => x - sqrt)

  def down(pos: Int) = move(pos, cantDown, x => x + sqrt)

  def left(pos: Int) = move(pos, cantLeft, x => x - 1)

  def right(pos: Int) = move(pos, cantRight, x => x + 1)

  def move(currIndex: Int, cant: Seq[Int], f: Int => Int) = {
    if (cant.contains(currIndex)) currIndex else {
      f(currIndex)
    }
  }

  code(startPos, instrLines).map(keypad(_)).mkString
}

bathroomCode(keypad1, 4, cantUp, cantDown, cantLeft, cantRight)
bathroomCode(keypad2, 10, cantUp2, cantDown2, cantLeft2, cantRight2)
