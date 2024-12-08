package day8

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Solution extends ZIOAppDefault {

  private case class Coordinate(x: Int, y: Int) {
    def isLeftAboveOf(other: Coordinate): Boolean = x <= other.x & y <= other.y
    def isLeftBelowOf(other: Coordinate): Boolean = x <= other.x & y > other.y
    def isRightAboveOf(other: Coordinate): Boolean = x > other.x & y <= other.y

    def getCoordinateLeftBelow(d: (Int, Int)): Coordinate = Coordinate(x - d._1, y + d._2)
    def getCoordinateLeftAbove(d: (Int, Int)): Coordinate = Coordinate(x - d._1, y - d._2)
    def getCoordinateRightBelow(d: (Int, Int)): Coordinate = Coordinate(x + d._1, y + d._2)
    def getCoordinateRightAbove(d: (Int, Int)): Coordinate = Coordinate(x + d._1, y - d._2)

    def dx(other: Coordinate): Int = math.abs(x - other.x)
    def dy(other: Coordinate): Int = math.abs(y - other.y)
  }

  private type Grid = Map[Coordinate, Char]

  private def createGrid(string: List[List[Char]]): Grid = {
    string.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (char, x) =>
        Coordinate(x, y) -> char
      }
    }.toMap
  }

  private def getPointsOnLine(node1: Coordinate, node2: Coordinate, distances: List[(Int, Int)]): Set[Coordinate] =
    distances
      .map { distance =>
        if node1.isLeftAboveOf(node2) then Set(node1.getCoordinateLeftAbove(distance), node2.getCoordinateRightBelow(distance))
        else if node1.isLeftBelowOf(node2) then Set(node1.getCoordinateLeftBelow(distance), node2.getCoordinateRightAbove(distance))
        else if node1.isRightAboveOf(node2) then Set(node1.getCoordinateRightAbove(distance), node2.getCoordinateLeftBelow(distance))
        else Set(node1.getCoordinateRightBelow(distance), node2.getCoordinateLeftAbove(distance))
      }
      .reduce(_ ++ _)

  private def isWithinGrid(gridSize: (Int, Int), point: Coordinate): Boolean =
    point.x <= gridSize._1 & point.y <= gridSize._2 & point.x >= 0 & point.y >= 0

  private def findAntinodes(node1: Coordinate, node2: Coordinate, part2: Boolean): Set[Coordinate] = {
    val ints = if part2 then (0 to 50).toList else List(1)
    val distances = ints.map(n => (n * node1.dx(node2), n * node1.dy(node2)))
    getPointsOnLine(node1, node2, distances)
  }

  private def findAntinodes(grid: Grid, char: Char, gridSize: (Int, Int), sol2: Boolean): Set[Coordinate] =
    grid
      .filter { case (_, c) => c == char }
      .keySet
      .toList
      .combinations(2)
      .flatMap { case List(node1, node2) => findAntinodes(node1, node2, sol2).filter(isWithinGrid(gridSize, _)) }
      .toSet

  private val lowercaseCharacters = ('a' to 'z').toList
  private val uppercaseCharacters = ('A' to 'Z').toList
  private val numbers = ('0' to '9').toList

  private val allCharacters = lowercaseCharacters ++ uppercaseCharacters ++ numbers

  private def findAntinodes(grid: Grid, gridSize: (Int, Int), part2: Boolean): Set[Coordinate] =
    allCharacters.map(c => findAntinodes(grid, c, gridSize, part2)).reduce(_ ++ _)

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {

    val input: List[List[Char]] =
      general.ReadInTestData.getPuzzleInput(_.toList)("src/test/scala/day8/input.txt")

    val grid = createGrid(input)
    val gridSize = (input.head.length - 1, input.length - 1)

    val part1 = findAntinodes(grid, gridSize, false).size
    println(s"Part 1: $part1")

    val part2 = findAntinodes(grid, gridSize, true).size
    println(s"Part 2: $part2")

    ZIO.unit
  }
}
