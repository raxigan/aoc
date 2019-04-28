val input = io.Source.fromResource("day12.txt").mkString

//val input = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"
val lines = input.split("\n")
//val regs = ('a' to 'd').map(c => (c -> 0)).toMap
val regs2 = ('a' to 'd').map(c => c -> 0).toMap + ('c' -> 1)

code(0, regs2)('a')

def code(lineNo: Int, regs: Map[Char, Int]): Map[Char, Int] = {

  def value(arg: String): Int = {
    if (arg.length == 1 && regs.keySet.contains(arg(0))) regs(arg(0))
    else arg.toInt
  }

  if (lineNo > lines.size - 1) return regs
  val spl = lines(lineNo).split(" ")

  spl(0) match {
    case "cpy" => code(lineNo + 1, regs + (spl(2)(0) -> value(spl(1))))
    case "inc" => code(lineNo + 1, regs + (spl(1)(0) -> (regs(spl(1)(0)) + 1)))
    case "dec" => code(lineNo + 1, regs + (spl(1)(0) -> (regs(spl(1)(0)) - 1)))
    case "jnz" => code(lineNo + (if (value(spl(1)) != 0) spl(2).toInt else 1), regs)
  }

}
