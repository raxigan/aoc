val input = io.Source.fromResource("day03.txt").mkString

def isTriangle(s1: Int, s2: Int, s3: Int): Boolean = {
  (s1 + s2 > s3) && (s2 + s3 > s1) && (s1 + s3 > s2)
}

val lines = input.split("\n")

val triangleSpecs = lines.map(k => k.trim.split(" +").map(_.toInt).toList).toList

triangleSpecs.count(s => isTriangle(s(0), s(1), s(2)))

val threeRowChunks = triangleSpecs.grouped(3).toList

val triangleSpecs2 = threeRowChunks.flatMap(l => get3Triangles(l))
triangleSpecs2.count(s => isTriangle(s(0), s(1), s(2)))

def get3Triangles(rows: List[List[Int]]): List[Seq[Int]] = {
  val r = 0 to 2
  List(r.map(rows(_)(0)), r.map(rows(_)(1)), r.map(rows(_)(2)))
}

