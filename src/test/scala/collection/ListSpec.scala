package collection


import com.ntonyo.github.collection.{List, Nil}
import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {
  "A List" should "correct accumulate sum in foldRight" in {
    val acc = List(1,2,3,4,5,6,7).foldRight(0)(_ + _)
    acc should be (28)
  }

  "A List" should "return z for Nil foldRight" in {
    val sum = (l: Int, r: Int) => l + r
    val z = 1
    val acc = Nil.foldRight(z)(sum)
    acc should be (1)
  }

  "A list" should "return correct value through index" in {
    val list = List(1,2,3,4)
    list(0) should be (1)
    list(1) should be (2)
    list(2) should be (3)
    list(3) should be (4)
  }

  "A List" should "correct map" in {
    val list = List(1,2,3).map(_.toString)
    list(0) should be ("1")
    list(1) should be ("2")
    list(2) should be ("3")
  }

  it should "thrown IndexOutOfBoundsException if i > list size" in {
    a [IndexOutOfBoundsException] should be thrownBy {
      val list = List(1,2,3)
      list(4)
    }
  }
}
