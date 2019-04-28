val input = io.Source.fromResource("day15.txt").mkString

//val input = "Disc #1 has 5 positions; at time=0, it is at position 4.\n" +
//  "Disc #2 has 2 positions; at time=0, it is at position 1."

val lines = input.split("\n").toList :+
  "Disc #7 has 11 positions; at time=0, it is at position 0."

val times = 100000000

def posAtTime(spec: String) = {
  val chunks = spec.split(" ")
  val posNo = chunks(3).toInt
  val currPos = chunks.last.init.toInt

  (0 to times).map(k => (currPos + k) % posNo).toArray
}

def isPathOkAtTime(time: Int, discPosAtTime: Seq[Array[Int]]): Boolean = {
  discPosAtTime.indices.map(d => discPosAtTime(d)(time + d)).forall(_ == 0)
}

val discPosAtTime = lines.map(posAtTime)
val timeToPressButton = (0 to times - 10).find(k => isPathOkAtTime(k, discPosAtTime))