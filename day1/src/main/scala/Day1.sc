val text = io.Source
  .fromInputStream(getClass.getResourceAsStream("input.txt")).mkString

val commands = text.split(",").map(k => k.trim)


def visited(pos: List[(Int, Int)] = List((0, 0)), direction: Int = 0, spec: Array[String]): List[(Int, Int)] = {

  if (spec.isEmpty) pos
  else {
    val cmd = (spec.head.head, spec.head.tail.toInt)
    val newIndex = direction + (if (cmd._1 == 'L') -90 else 90)
    val newD = if (newIndex == -90) 270 else newIndex % 360
    val p = pos.last

    val newPos = newD match {
      case 0 => (p._2 to p._2 + cmd._2).map(i => (p._1, i))
      case 90 => (p._1 to p._1 + cmd._2).map(i => (i, p._2))
      case 180 => (p._2 - cmd._2 to p._2).map(i => (p._1, i)).reverse
      case 270 => (p._1 - cmd._2 to p._1).map(i => (i, p._2)).reverse
    }

    visited(pos ::: newPos.toList.tail, newD, spec.tail)
  }
}


def blockAway(sp: (Int, Int)) = Math.abs(sp._1) + Math.abs(sp._2)

def placesVisitedTwice(u: List[(Int, Int)]) = u.diff(u.distinct).distinct

val allVisitedPlaces = visited(spec = commands)
val answer1 = blockAway(allVisitedPlaces.last)
val answer2 = placesVisitedTwice(allVisitedPlaces).map(blockAway).head

