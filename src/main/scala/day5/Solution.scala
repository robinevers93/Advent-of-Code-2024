package day5

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Solution extends ZIOAppDefault {

  private def readRules(string: String): (Int, Int) = {
    val split = string.split('|')
    (split.head.toInt, split(1).toInt)
  }

  private val rules: List[(Int, Int)] =
    general.ReadInTestData.getPuzzleInput(readRules)("src/test/scala/day5/input-rules.txt")

  private val updates: List[List[Int]] =
    general.ReadInTestData.getPuzzleInput(_.split(',').toList.map(_.toInt))("src/test/scala/day5/input.txt")

  private val ordering = new Ordering[Int] {
    override def compare(x: Int, y: Int): Int = {
      if (rules.contains((x, y))) -1
      else if (rules.contains((y, x))) 1
      else 0
    }
  }

  private def isOrdered(update: List[Int]): Boolean =
    update.sorted(ordering) == update

  private def getMiddleNumber(range: List[Int]): Int =
    range(range.length / 2)

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {

    val part1 = updates.filter(isOrdered).map(getMiddleNumber).sum
    println(s"Part 1: $part1")
    val part2 = updates.filterNot(isOrdered).map(_.sorted(ordering)).map(getMiddleNumber).sum
    println(s"Part 2: $part2")

    ZIO.unit
  }
}
