import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter.printHexBinary

//val input = "abc"
val input = "ahsbgdzn"

def md5(text: String) = {
  val md5bytes = MessageDigest.getInstance("MD5").digest(text.getBytes())
  printHexBinary(md5bytes).toLowerCase
}

def hashOfHash(text: String, ix: Int = 0): String = {
  if (ix == 2017) text
  else hashOfHash(md5(text), ix + 1)
}

def get3Cons(md5: String): Option[(Char, Int)] = {
  md5.zipWithIndex.find(k => k._2 > 0 && k._2 < md5.length - 1 && k._1 == md5(k._2 - 1) && k._1 == md5(k._2 + 1))
}

def contains5(md5: String, c: Char) = {
  md5.contains(c.toString * 5)
}

//val hashes = (0 to 46728).map(i => md5(input + i))
val hashes = (0 to 28859).map(i => hashOfHash(input + i))
println(hashes(22551))
println(hashes(200))
val s = hashes.zipWithIndex.filter(k => k._2 < 25859 && isKey(k._1, k._2)).toList
s(63)

def isKey(md5: String, id: Int): Boolean = {
  val cons = get3Cons(md5)

  if (cons.isEmpty) false
  else {
    (id + 1 until 1000 + id)
      .map(i => hashes(i)).exists(k => contains5(k, cons.get._1))
  }
}


