val input = io.Source.fromResource("day07.txt").mkString

val lines = input.split("\n")

supportsTLS("abba[mnop]qrst")
supportsTLS("abcd[bddb]xyyx")
supportsTLS("aaaa[qwer]tyui")
supportsTLS("ioxxoj[asdfgh]zxcvbn")

supportsSSL("aba[bab]xyz")
supportsSSL("xyx[xyx]xyx")
supportsSSL("aaa[kek]eke")
supportsSSL("zazbz[bzb]cdb")

val ipsSupportingTLS = lines.count(supportsTLS)
val ipsSupportingSSL = lines.count(supportsSSL)

def supportsTLS(line: String): Boolean = {
  val splitted = line.split("[\\[\\]]").toList
  val w = splitted.indices.exists(k => k % 2 == 0 && hasAbba(splitted(k)))
  val z = splitted.indices.exists(k => k % 2 == 1 && hasAbba(splitted(k)))
  w && !z
}

def supportsSSL(line: String): Boolean = {
  val splitted = line.split("[\\[\\]]").toList
  val hypernet = splitted.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).mkString
  splitted.indices.exists(k => k % 2 == 0 && hastAbaAndBab(splitted(k), 1, hypernet))
}

def hasAbba(ln: String, ix: Int = 1): Boolean = {
  if (ix + 2 >= ln.length) return false
  if (isAbba(ln, ix)) true
  else hasAbba(ln, ix + 1)
}

def hastAbaAndBab(ln: String, ix: Int = 1, hypernet: String): Boolean = {
  if (ix + 1 >= ln.length) return false
  if (isAbba(ln, ix, 1) && hypernet.contains(toBab(ln.substring(ix - 1, ix + 2)))) true
  else hastAbaAndBab(ln, ix + 1, hypernet)
}

def toBab(aba: String) = {
  aba.tail + aba(1)
}

def isAbba(ln: String, ix: Int, cons: Int = 2) = {
  val let = ln.substring(ix - 1, ix + cons + 1)
  let == let.reverse && let(0) != let(1)
}