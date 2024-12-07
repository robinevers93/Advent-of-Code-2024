package day4

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

//Absolutely gross solution, for part 1, but it works
object Solution extends ZIOAppDefault {

  private def findAllSubstrings(input: List[Char]): Int = {
    val regex = """XMAS""".r
    val matches = regex.findAllMatchIn(input.mkString).toList
    val reverseRegex = """SAMX""".r
    val matches2 = reverseRegex.findAllMatchIn(input.mkString).toList
    println(s"There are ${matches.size} matches and ${matches2.size} reverse matches")
    matches.size + matches2.size
  }

  private def findSubstringsInMatrix(matrix: List[List[Char]]): Int = {
    matrix.map(findAllSubstrings).sum
  }

  private def rotateMatrix(matrix: List[List[Char]]): List[List[Char]] = {
    val n = matrix.size
    val m = matrix.head.size
    (0 until m).map { i =>
      (0 until n).map { j =>
        matrix(j)(i)
      }.toList
    }.toList
  }

  private def getDiagonalsOfMatrix(matrix: List[List[Char]]): List[List[Char]] = {
    val n = matrix.size
    val m = matrix.head.size

    val diagonals1 = (0 until n).map { i =>
      (0 until i + 1).map { j =>
        matrix(i - j)(j)
      }.toList
    }.toList

    val diagonals2 =
      (1 until m).map { i =>
        (0 until n - i).map { j =>
          matrix(n - j - 1)(i + j)
        }.toList
      }.toList

    val diagonals3 = (0 until n).map { i =>
      (0 until i + 1).map { j =>
        matrix.reverse(i - j)(j)
      }.toList
    }.toList

    val diagonals4 =
      (1 until m).map { i =>
        (0 until n - i).map { j =>
          matrix.reverse(n - j - 1)(i + j)
        }.toList
      }.toList

    diagonals1 ++ diagonals2 ++ diagonals3 ++ diagonals4

  }

  private def getSubstringInAllDirections(matrix: List[List[Char]]): Int = {
    val x = findSubstringsInMatrix(matrix)
    val y = findSubstringsInMatrix(rotateMatrix(matrix))
    val z = findSubstringsInMatrix(getDiagonalsOfMatrix(matrix))
    println(s"regular matches: $x, rotated matches: $y, diagonal matches: $z")
    x + y + z
  }

  private def subMatrixHasXmasShape(subMatrix: List[List[Char]]): Boolean = {
    val regex1 = """S.S.A.M.M"""
    val matches1 = subMatrix.flatten.mkString.matches(regex1)
    val regex2 = """M.M.A.S.S"""
    val matches2 = subMatrix.flatten.mkString.matches(regex2)
    val regex3 = """M.S.A.M.S"""
    val matches3 = subMatrix.flatten.mkString.matches(regex3)
    val regex4 = """S.M.A.S.M"""
    val matches4 = subMatrix.flatten.mkString.matches(regex4)
    val combined = matches1 || matches2 || matches3 || matches4
    combined
  }
  private def findSubMatrixOccurrences(fullMatrix: List[List[Char]]): Int = {
    val n = fullMatrix.size
    val m = fullMatrix.head.size
    (0 until n - 2).map { i =>
      (0 until m - 2).count { j =>
        val subMatrixToCheck = fullMatrix.slice(i, i + 3).map(_.slice(j, j + 3))
        subMatrixHasXmasShape(subMatrixToCheck)
      }
    }.sum
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input: List[List[Char]] = general.ReadInTestData.getPuzzleInput(_.toList)("src/test/scala/day4/input.txt")
    val part1 = getSubstringInAllDirections(input)
    println(s"Part 1: $part1")
    val part2 = findSubMatrixOccurrences(input)
    println(s"Part 2: $part2")

    ZIO.unit
  }
}
