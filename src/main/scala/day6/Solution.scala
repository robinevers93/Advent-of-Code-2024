package day6

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.annotation.tailrec

object Solution extends ZIOAppDefault {

  private type Grid = Map[(Int, Int), Char]

  private def moveUp(oldGrid: Grid, currentPosition: ((Int, Int), Char)): (Grid, Option[((Int, Int), Char)]) =
    currentPosition._1 match {
      case (x, 0) => (oldGrid.updated((x, 0), 'X'), None) // walked off grid
      case (x, y) if oldGrid(x, y - 1) == '#' => (oldGrid.updated((x, y), '+'), Some((x, y), '>')) // found an obstacle
      case (x, y) => (oldGrid.updated((x, y), '^'), Some((x, y - 1), '^')) // normal move
    }

  private def moveDown(oldGrid: Grid, currentPosition: ((Int, Int), Char), maxY: Int): (Grid, Option[((Int, Int), Char)]) =
    currentPosition._1 match {
      case (x, y) if y == maxY => (oldGrid.updated((x, maxY), 'X'), None) // walked off grid
      case (x, y) if oldGrid(x, y + 1) == '#' => (oldGrid.updated((x, y), '+'), Some((x, y), '<')) // found an obstacle
      case (x, y) => (oldGrid.updated((x, y), 'v'), Some((x, y + 1), 'v')) // normal move
    }

  private def moveLeft(oldGrid: Grid, currentPosition: ((Int, Int), Char)): (Grid, Option[((Int, Int), Char)]) =
    currentPosition._1 match {
      case (0, y) => (oldGrid.updated((0, y), 'X'), None) // walked off grid
      case (x, y) if oldGrid(x - 1, y) == '#' => (oldGrid.updated((x, y), '+'), Some((x, y), '^')) // found an obstacle
      case (x, y) => (oldGrid.updated((x, y), '<'), Some((x - 1, y), '<')) // normal move
    }

  private def moveRight(oldGrid: Grid, currentPosition: ((Int, Int), Char), maxX: Int): (Grid, Option[((Int, Int), Char)]) =
    currentPosition._1 match {
      case (x, y) if x == maxX => (oldGrid.updated((x, y), 'X'), None) // walked off grid
      case (x, y) if oldGrid(x + 1, y) == '#' => (oldGrid.updated((x, y), '+'), Some((x, y), 'v')) // found an obstacle
      case (x, y) => (oldGrid.updated((x, y), '>'), Some((x + 1, y), '>')) // normal move
    }

  private def getStartingPosition(grid: Grid): ((Int, Int), Char) =
    grid.toList.find(coordinate => List('^', 'v', '<', '>').contains(coordinate._2)).get

  private def countInGrid(grid: Grid, value: Char): Int =
    grid.values.count(_ == value)

  private def addObstacleToGrid(grid: Grid, x: Int, y: Int): Grid =
    grid.updated((x, y), '#')

  private def doesGridHaveLoop(grid: Grid, maxX: Int, maxY: Int, startingPosition: ((Int, Int), Char)): Boolean = {

    @tailrec
    def gridHasLoop(
      oldGrid: Grid,
      maxX: Int,
      maxY: Int,
      currentPosition: ((Int, Int), Char),
      visitedLeft: Set[(Int, Int)],
      visitedRight: Set[(Int, Int)],
      visitedUp: Set[(Int, Int)],
      visitedDown: Set[(Int, Int)]
    ): Boolean = {
      val foundLoop = currentPosition._2 match {
        case '^' => visitedUp.contains(currentPosition._1)
        case 'v' => visitedDown.contains(currentPosition._1)
        case '<' => visitedLeft.contains(currentPosition._1)
        case '>' => visitedRight.contains(currentPosition._1)
      }

      if (!foundLoop) {
        val (newGrid, newPosition) = currentPosition._2 match {
          case '^' => moveUp(oldGrid, currentPosition)
          case 'v' => moveDown(oldGrid, currentPosition, maxY)
          case '<' => moveLeft(oldGrid, currentPosition)
          case '>' => moveRight(oldGrid, currentPosition, maxX)
        }

        val newVisitedLeft = currentPosition._2 match {
          case '<' => visitedLeft + currentPosition._1
          case _ => visitedLeft
        }

        val newVisitedRight = currentPosition._2 match {
          case '>' => visitedRight + currentPosition._1
          case _ => visitedRight
        }

        val newVisitedUp = currentPosition._2 match {
          case '^' => visitedUp + currentPosition._1
          case _ => visitedUp
        }

        val newVisitedDown = currentPosition._2 match {
          case 'v' => visitedDown + currentPosition._1
          case _ => visitedDown
        }

        if (newPosition.isDefined)
          gridHasLoop(newGrid, maxX, maxY, newPosition.get, newVisitedLeft, newVisitedRight, newVisitedUp, newVisitedDown)
        else false
      } else true
    }

    val visitedLeft: Set[(Int, Int)] = Set.empty
    val visitedRight: Set[(Int, Int)] = Set.empty
    val visitedUp: Set[(Int, Int)] = Set.empty
    val visitedDown: Set[(Int, Int)] = Set.empty

    gridHasLoop(grid, maxX, maxY, startingPosition, visitedLeft, visitedRight, visitedUp, visitedDown)
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {

    val input: List[List[Char]] =
      general.ReadInTestData.getPuzzleInput(_.toCharArray.toList)("src/test/scala/day6/input.txt")

    val grid: Grid = input.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (value, x) =>
        (x, y) -> value
      }
    }.toMap

    val maxY = grid.keys.map(_._2).max
    val maxX = grid.keys.map(_._1).max
    val startingPosition = getStartingPosition(grid)

    val allGrids: Long =
      Range(0, maxY + 1).toList
        .map { y =>
          Range(0, maxX + 1).count { x =>
            doesGridHaveLoop(addObstacleToGrid(grid, x, y), maxX, maxY, startingPosition)
          }
        }
        .reduce(_ + _)

    println(s"Part 2: $allGrids")

    ZIO.unit
  }
}
