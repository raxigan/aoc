/**
  * Created by mateusz on 13/12/16.
  */
object Day13 extends App {
  val input = 1350
  val height = 39
  val width = 31

//    val input = 10
//    val height = 6
//    val width = 9

  val area = for {
    y <- 0 to height
    x <- 0 to width
  } yield (x, y)

  def count(x: Int, y: Int) = x * x + 3 * x + 2 * x * y + y + y * y

  val row0 = ("X" :: (0 to width).map(_.toString).toList).map(_.padTo(2, " ").mkString).mkString(" ")

  val l = area
    .map(p => count(p._1, p._2))
    .map(_ + input)
    .map(Integer.toBinaryString)
    .map(_.count(k => k == '1'))
    .map(k => if (k % 2 == 0) " ." else " #")
    .grouped(width + 1)

  val maze = row0 :: l.zipWithIndex.map(k => k._2.toString.padTo(2, " ").mkString + " " + k._1.mkString(" ")).toList
  maze.foreach(println)

}
