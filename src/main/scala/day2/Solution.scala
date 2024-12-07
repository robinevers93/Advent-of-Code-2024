package day2

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Solution extends ZIOAppDefault {

  private def readLevels(input: String): List[Int] =
    input.split(" ").toList.map(_.toInt)

  private def getIncrements(input: List[Int]): List[Int] =
    input.zip(input.tail).map { case (a, b) => b - a }

  private def levelIsSafe(list: List[Int]): Boolean =
    (list.forall(_ > 0) && list.forall(_ < 4)) || (list.forall(_ < 0) && list.forall(_ > -4))

  private def dropIndex(input: List[Int], index: Int): List[Int] =
    input.take(index) ++ input.drop(index + 1)

  private def hasSafeLevel(input: List[Int]): Boolean =
    Range(0, input.size).exists(i => levelIsSafe(getIncrements(dropIndex(input, i))))

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = general.ReadInTestData.getPuzzleInput(readLevels)("src/test/scala/day2/input.txt")
    val safeLevels = input.zipWithIndex.filter { case (x, _) => hasSafeLevel(x) }
    println(s"Safe levels: ${safeLevels.size}")

    ZIO.unit
  }
}
