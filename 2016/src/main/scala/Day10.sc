val input = io.Source.fromResource("day10.txt").mkString

val lines = input.split("\n")

val initialBots: Map[String, Seq[Int]] = (for (
  line <- lines;
  spl = line.split(" ").toList;
  if line.startsWith("value")
) yield (spl(4) + spl(5), spl(1).toInt)).groupBy(_._1).mapValues(_.map(_._2))

val instructions = lines.filterNot(_.startsWith("value")).toList

val bots = moveChips(instructions, initialBots)

bots("output0").head * bots("output1").head * bots("output2").head

def moveChips(lines: Seq[String], bots: Map[String, Seq[Int]]): Map[String, Seq[Int]] = {

  if (lines.isEmpty) return bots

  val splitted = lines.head.split(" ")
  val bot = splitted.head + splitted(1)
  val lowerTaker = splitted(5) + splitted(6)
  val higherTaker = splitted(10) + splitted(11)

  if (bots.get(bot).isEmpty || bots(bot).size != 2) return moveChips(lines.tail :+ lines.head, bots)

  val lower = bots(bot).min
  val higher = bots(bot).max

  val giveHigher = giveValue(higherTaker, higher, giveValue(lowerTaker, lower, emptyBot(bot, bots)))
  moveChips(lines.tail, bots ++ giveHigher)
}

def giveValue(bot: String, value: Int, bots: Map[String, Seq[Int]]) = {
  val curr = bots.getOrElse(bot, List()).toList
  bots + (bot -> (value :: curr))
}

def emptyBot(bot: String, bots: Map[String, Seq[Int]]) = {
  bots + (bot -> List())
}