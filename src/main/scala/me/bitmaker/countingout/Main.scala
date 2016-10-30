package me.bitmaker.countingout

import scala.annotation.tailrec


/**
  * Created by bitmaker on 10/29/16.
  */
object Main {
  def main(args: Array[String]): Unit = {
    val n = args(0).toInt
    val k = args(1).toInt
    val survivor = countOut(n, k)
    println(s"The last survivor is at position $survivor")
  }


  /**
    * Solve the counting-out game (Eeny, meeny, miny, moe)
    * @param n The number of people that are playing (conceptually, in a circle)
    * @param k The distance between people who are "out"
    * @return The position in the circle of the last person out
    */
  def countOut(n: Int, k: Int): Int = {
    require(n >= k, "N must be >= K")

    k match {
      case 1 => n
      case 2 => algorithmicCountOut(n)
      case _ =>
        val people: Vector[Int] = Range(1, n + 1).toVector // count people from 1
        recursiveCountOut(people, 0, k)
    }
  }

  /**
    * Recursively remove one person at a time from the solution, until one person is left
    * @param people A Vector of people remaining, where the value in the list represents the starting position
    *               Vector was chosen since this algorithm frequently removes elements from the middle of the list.
    * @param position The index to the location to start counting from
    * @param k The number of places to count each iteration
    * @return The position of the last person remaining
    */
  @tailrec
  def recursiveCountOut(people: Vector[Int], position: Int, k: Int): Int = {
    people.length match {
      case 1 =>
        people(0) // termination case
      case otherwise =>
        val nextPerson = (position + k - 1) % people.length
        val peopleMinusOne = people.patch(nextPerson, Vector.empty[Int], 1)
        recursiveCountOut(peopleMinusOne, nextPerson , k)
    }
  }


  /**
    * Copied from https://en.wikipedia.org/wiki/Josephus_problem
    * @param n the number of people standing in the circle
    * @return the safe position who will survive the execution
    *   f(N) = 2L + 1 where N =2^M + L and 0 <= L < 2^M
    */
  def algorithmicCountOut(n: Int): Int = {
    // find value of L for the equation
    val valueOfL: Int = n - Integer.highestOneBit(n)
    val safePosition: Int = 2 * valueOfL  + 1

    safePosition
  }
}
