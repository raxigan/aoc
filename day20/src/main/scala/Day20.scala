/**
  * Created by mateusz on 20/12/16.
  */
object Day20 extends App {
  val input = io.Source
    .fromInputStream(getClass.getResourceAsStream("input.txt")).mkString

  //  val input = "5-8\n0-2\n4-7\n"
  //  val allIps = 0 to 9

  val lines = input.split("\n")
  val ranges = lines.map(k => (k.split("-")(0).toLong, k.split("-")(1).toLong)).sortBy(_._1).toList

  val allowedIPs = getIp(ranges)

  val answer1 = allowedIPs.head
  val answer2 = allowedIPs.size

  println(s"$answer1 $answer2")

  def getIp(rangesLeft: List[(Long, Long)], minPos: Long = 0, allowed: List[Long] = List()): List[Long] = {
    val highest = Math.max(minPos, rangesLeft.head._2)

    if (rangesLeft.length == 1) allowed
    else getIp(rangesLeft.tail, highest, allowed ::: (highest + 1 until rangesLeft.tail.head._1).toList)
  }
}
