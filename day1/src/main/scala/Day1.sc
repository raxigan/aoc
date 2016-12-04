val text = io.Source
  .fromInputStream(getClass.getResourceAsStream("input.txt")).mkString

val k:Array[String] = text.split(",")

val degree = 0

def getPos(pos: (Int, Int), d: Int, r: Array[String]): (Int, Int) = {
  if (r.isEmpty) pos
  else {
    val k = r.head
    val dd = List(0, 90, 180, 270)
    val newIndex = dd.indexOf(d) + (if(k.head == 'L') -1 else 1)
    val newD = dd(if(newIndex == -1) 3 else newIndex % 4)
    newD match {
      case 0 => getPos((pos._1, pos._2 + k.tail.toInt), newD, r.tail): (Int, Int)
      case 90 => getPos((pos._1+ k.tail.toInt, pos._2), newD, r.tail): (Int, Int)
      case 180 => getPos((pos._1, pos._2- k.tail.toInt), newD, r.tail): (Int, Int)
      case 270 => getPos((pos._1- k.tail.toInt, pos._2), newD, r.tail): (Int, Int)
    }
  }
}

val pos = getPos((0,0), 0, k.map(_.trim))
Math.abs(pos._1) + Math.abs(pos._2)

val pos1 = getPos((0,0), 0, Array("R2", "L3"))
val pos2 = getPos((0,0), 0, Array("R2", "R2", "R2"))
val pos3 = getPos((0,0), 0, Array("R5", "L5", "R5", "R3"))

Math.abs(pos1._1) + Math.abs(pos1._2)
Math.abs(pos2._1) + Math.abs(pos2._2)
Math.abs(pos3._1) + Math.abs(pos3._2)

