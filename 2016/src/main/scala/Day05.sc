import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter.printHexBinary

val input = "wtnhxymk"
//val input = "abc"

def md5(text: String) = {
  val md5bytes = MessageDigest.getInstance("MD5").digest(text.getBytes())
  printHexBinary(md5bytes)
}

def password(index: Int = 0, pw: String = ""): String = {
  if (pw.length == 8) pw
  else {
    val hex = md5(input + index)
    if (hex.startsWith("00000")) password(index + 1, pw + hex(5))
    else password(index + 1, pw)
  }
}

def password2(index: Int = 0, pw: Map[Int, Char] = Map()): String = {
  if (pw.size == 8) {
    pw.keySet.toList.sorted.map(pw(_)).mkString.toLowerCase
  }
  else {
    val hex = md5(input + index)
    if (hex.startsWith("00000") && ('0' to '7').contains(hex(5))) {
      val pos = hex(5).asDigit
      password2(index + 1, pw + (pos -> pw.getOrElse(pos, hex(6))))
    }
    else password2(index + 1, pw)
  }
}

//password()
password2()