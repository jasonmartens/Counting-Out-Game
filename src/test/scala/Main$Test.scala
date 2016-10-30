import org.scalatest.{FlatSpec, Matchers}
import me.bitmaker.countingout.Main._

import scala.util.Random

/**
  * Created by bitmaker on 10/29/16.
  */
class Main$Test extends FlatSpec with Matchers {
  "The Counting-Out Game" should "return the position of the last person out" in {
    countOut(2, 1) shouldBe 2

    // i i i i i
    // i i o
    // o     i i
    //   i   i o
    //   i   i
    //   o
    countOut(5, 3) shouldBe 4
  }

  it should "not allow k > n" in {
    assertThrows[RuntimeException] {
      countOut(1, 2)
    }
  }

  it should "allow n == k" in {
    // i i i
    // i i o
    // i i
    // o
    countOut(3, 3) shouldBe 2
  }

  it should "work with n = 3 and k = 2" in {
    // i i i
    // i o
    // o   i
    countOut(3, 2) shouldBe 3
  }

  it should "work with a very large n and k = 2" in {
    countOut(123123, 2) shouldBe 115175
  }

  "The algorithmic solution" should "equal the brute-force solution" in {
    Range(1, 10).map(_ => Random.nextInt(5000)).foreach{ n =>
      println(s"Testing n == $n")
      recursiveCountOut(Range(1, n + 1).toVector, n, 2) shouldBe algorithmicCountOut(n)
    }
  }
}
