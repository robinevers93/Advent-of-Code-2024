package day3

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Solution extends ZIOAppDefault {

  private def findAllRegexInString(input: String): Int = {
    val regex = """mul\((\d+),(\d+)\)""".r
    val matches = regex.findAllMatchIn(input).toList
    matches.map(m => m.group(1).toInt * m.group(2).toInt).sum
  }

  private def removeSubstrings(input: String): String = {
    input
      .replaceAll("""don't\(\).*?do\(\)""", "")
      .replaceFirst("""don't\(\).*$""", "")
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input: List[String] = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day3/input.txt")
    val string = input.mkString("")
    val part1 = findAllRegexInString(string)
    println(s"Part 1: $part1")
    val part2 = findAllRegexInString(removeSubstrings(string))
    println(s"Part 2: $part2")

    ZIO.unit
  }
}
