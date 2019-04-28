val input = io.Source.fromResource("day06.txt").mkString

//val input = "eedadn\ndrvtee\neandsr\nraavrd\n" +
//  "atevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada" +
//  "\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"

val lines = input.split("\n").toList
val longestLineSize = lines.map(_.length).max

val u = for(
  i <- 0 until longestLineSize
) yield lines.map(_(i))

val msg = u.map(posChars => posChars.maxBy(x => posChars.count(_ == x))).mkString
val msg2 = u.map(posChars => posChars.minBy(x => posChars.count(_ == x))).mkString