
Integer.toString(7, 2)
Integer.toBinaryString(7)


val input = 1350

val area = for {
  x <- 0 to 31
  y <- 0 to 39
} yield (x, y)

def count(x: Int, y: Int) = x * x + 3 * x + 2 * x * y + y + y * y

val l = area
  .map(p => count(p._1, p._2))
  .map(_ + input)
  .map(Integer.toBinaryString(_))
  .map(_.count(k => k == '1'))
  .map(k => if (k % 2 == 0) '.' else '#')
  .grouped(31).mkString("\n")

l.foreach(k => println(k.mkString("\n")))

for(i <- l) {
  println(l.mkString("\n"))
}