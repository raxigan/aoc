val input = io.Source
  .fromInputStream(getClass.getResourceAsStream("input.txt")).mkString

def isTriangle(s1: Int, s2: Int, s3: Int): Boolean = {
  (s1 + s2 > s3) && (s2 + s3 > s1) && (s1 + s3 > s2)
}

val lines = input.split("\n")

val triangleSpecs = lines.map(k => k.trim.split(" +").map(_.toInt).toList).toList

triangleSpecs.count(s => isTriangle(s(0), s(1), s(2)))

val threeRowChunks = triangleSpecs.grouped(3).toList

val triangleSpecs2 = threeRowChunks.flatMap(l => get3Triangles(l))
triangleSpecs2.count(s => isTriangle(s(0), s(1), s(2)))

def get3Triangles(rows: List[List[Int]]): List[List[Int]] = {
  List(List(rows(0)(0), rows(1)(0), rows(2)(0)),
    List(rows(0)(1), rows(1)(1), rows(2)(1)),
    List(rows(0)(2), rows(1)(2), rows(2)(2))
  )
}

