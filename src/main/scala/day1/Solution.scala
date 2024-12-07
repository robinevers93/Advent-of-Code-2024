package day1

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Solution extends ZIOAppDefault {

  private def splitString(input: String): (Int, Int) =
    (input.split(" {3}")(0).toInt, input.split(" {3}")(1).toInt)

  private def combineLists(input: List[(Int, Int)]): (List[Int], List[Int]) =
    (input.map(_._1), input.map(_._2))

  private def addDistances(list1: List[Int], list2: List[Int]): List[Int] =
    list1.zip(list2).map { case (a, b) => Math.abs(a - b) }

  private def getSimilarityScore(int: Int, list: List[Int]): Int =
    list.count(_ == int) * int

  private def addSimilarityScores(list1: List[Int], list2: List[Int]): List[Int] =
    list1.map(getSimilarityScore(_, list2))

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    val input = general.ReadInTestData.getPuzzleInput(splitString)("src/test/scala/day1/input.txt")
    val (list1, list2) = combineLists(input)
    val sortedList1 = list1.sorted
    val sortedList2 = list2.sorted
    val totalDistance = addDistances(sortedList1, sortedList2).sum
    println(s"Total distance: $totalDistance")

    val similarityScore = addSimilarityScores(list1, list2).sum
    println(s"Similarity score: $similarityScore")

    ZIO.unit
  }
}
