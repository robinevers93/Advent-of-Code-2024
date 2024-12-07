package day7

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Solution extends ZIOAppDefault {

  private case class Equation(total: Long, numbers: List[Long])

  private def readLine(string: String): Equation = {
    val split = string.split(": ")
    Equation(split.head.toLong, split(1).split(' ').toList.map(_.toLong))
  }

  private def makeEquationWork(equation: Equation, part2: Boolean): Long = {
    val numbers = equation.numbers
    val total = equation.total
    val remainder = numbers.tail
    if remainder
        .foldLeft(List(numbers.head))((acc: List[Long], number: Long) => {
          val addedToAcc = acc.map(a => a + number)
          val multipliedWithAcc = acc.map(a => a * number)
          val concatenatedWithAcc = if part2 then acc.map(a => s"${a.toString}${number.toString}".toLong) else List.empty
          addedToAcc ++ multipliedWithAcc ++ concatenatedWithAcc
        })
        .count(_ == total) > 0
    then total
    else 0
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {

    val input: List[String] =
      general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day7/input.txt")

    val part1 = input.map(readLine).map(makeEquationWork(_, false)).sum
    println(s"Part 1: $part1")
    val part2 = input.map(readLine).map(makeEquationWork(_, true)).sum
    println(s"Part 2: $part2")

    ZIO.unit
  }
}
