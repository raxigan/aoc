val input = io.Source
  .fromInputStream(getClass.getResourceAsStream("input.txt")).mkString

val lines = input.split("\n")

isRoom("aaaaa-bbb-z-y-x-123[abxyz]")
isRoom("a-b-c-d-e-f-g-h-987[abcde]")
isRoom("not-a-real-room-404[oarel]")
isRoom("totally-real-room-200[decoy]")

val roomLines = lines.filter(isRoom)
val sectorIdsSum = roomLines.map(k => k.split("-").last.split("\\[")(0).toInt).toList.sum

def isRoom(line: String): Boolean = {

  val checksum = line.split("\\[")(1).init

  line.split("-")
    .init.flatten
    .groupBy(_.hashCode)
    .values.map(_.mkString)
    .toList.sortBy(k => (-k.size, k))
    .map(_ (0)).mkString.take(5) == checksum
}