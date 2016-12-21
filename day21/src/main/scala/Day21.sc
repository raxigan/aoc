val input = io.Source
  .fromInputStream(getClass.getResourceAsStream("input.txt")).mkString

val operations = input.split("\n").toList

val unscrambled = "abcdefgh"
val scrambled = "fbgdceah"

val answer1 = password(unscrambled, operations)
val answer2 = scrambled
  .permutations.toList
  .find(k => password(k, operations) == scrambled)
  .get


def password(p: String, o: Seq[String]): String = {
  if (o.isEmpty) p
  else {
    val s = o.head.split(" ")

    val newPw =
      if (s.head == "swap" && s(1) == "position") {
        swapPos(p, s(2).toInt, s.last.toInt)
      } else if (s(1) == "letter") {
        swapLet(p, s(2).head, s.last.head)
      } else if (s.head == "rotate" && s.last.contains("step")) {
        rotate(p, s(1) == "right", s(2).toInt)
      } else if (s.head == "rotate" && s(1) == "based") {
        val idx = p.indexOf(s.last.head)
        rotate(p, true, idx + (if (idx >= 4) 2 else 1))
      } else if (s.head == "reverse") {
        reverse(p, s(2).toInt, s.last.toInt)
      } else {
        movePos(p, s(2).toInt, s.last.toInt)
      }

    password(newPw, o.tail)
  }
}


def swapPos(pw: String, x: Int, y: Int) = {
  pw.indices
    .map(k => if (k == x) -1 else k)
    .map(k => if (k == y) x else k)
    .map(k => if (k == -1) y else k)
    .map(pw(_))
    .mkString
}

def swapLet(pw: String, x: Char, y: Char) = {
  pw.map(c => if (c == x) '!' else c)
    .map(c => if (c == y) x else c)
    .map(c => if (c == '!') y else c)
}

def rotate(pw: String, right: Boolean, steps: Int) = {
  pw.indices
    .map(_ + (if (right) -steps else steps))
    .map(_ % pw.length)
    .map(k => if (k < 0) pw.length + k else k)
    .map(pw(_)).mkString
}

def reverse(pw: String, x: Int, y: Int) = {
  pw.substring(0, x) +
    pw.substring(x, y + 1).reverse +
    pw.substring(y + 1, pw.length)
}

def movePos(pw: String, x: Int, y: Int) = {
  val s = pw.indices
    .filterNot(_ == x)
    .map(pw(_)).splitAt(y)

  s._1.mkString + pw(x).toString + s._2.mkString
}
