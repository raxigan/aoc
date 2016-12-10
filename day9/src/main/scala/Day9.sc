val input = io.Source
  .fromInputStream(getClass.getResourceAsStream("input.txt")).mkString

//check("ADVENT")
//check("A(1x5)BC")
//check("(3x3)XYZ")
//check("A(2x2)BCD(2x2)EFG")
//check("(6x1)(1x3)A")
//check("X(8x2)(3x3)ABCY")

// 773345 - wrong
// 122055 - to low
// 235244 - to low
// 348433 - wrong???

check("(3x3)XYZ")

//check(input)

def check(text: String, transformed: String = "", startFrom:Int = 0): String = {

  println(text)

  if (text.isEmpty) return transformed

  if (text.head == '(') {
    val s = text.split("\\)")
    val spec = s.head
    val splt = spec.tail.split("x")

    val charsNo = splt.head.toInt
    val multiplier = splt.last.toInt

    val l = text.indexOf(")") + 1
    val tToMlt = text.substring(l, l + charsNo)
    val g = tToMlt.lastIndexOf(")")
    val u = tToMlt.substring(g+1, tToMlt.size)
    val t = u * multiplier
//    check(text.substring(l + charsNo, text.length), transformed + t)
    val newText = text + t
    println(newText.substring(l, newText.length) + " " + text)
    check(newText.substring(l, newText.length), transformed + t, text.length)
  }
  else {
    val taken = text.substring(startFrom, text.size).takeWhile(_ != '(')
    check(text.substring(taken.length, text.length), transformed + taken)
  }
}