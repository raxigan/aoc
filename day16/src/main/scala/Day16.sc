//val input = "10111100110001111"
//val toFill = 35651584
val input = "10000"
val toFill = 20

def process(a: String) = a + '0' + a.reverse.map(c => if (c == '0') '1' else '0')
def fill(txt: String): String = if (txt.length < toFill) fill(process(txt)) else txt.substring(0, toFill)

def checksum(txt: String): String = {
  if (txt.length % 2 != 0) txt
  else checksum(txt.indices
    .filter(_ % 2 == 0)
    .map(k => if (txt(k) == txt(k + 1)) '1' else '0')
    .mkString)
}

checksum(fill(input))