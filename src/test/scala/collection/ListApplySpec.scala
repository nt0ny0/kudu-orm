package collection
import com.ntonyo.github.collection.{List, Nil, ::}
import org.scalatest._

class ListApplySpec extends  FlatSpec {
  "A List apply" should "return Nil on empty args" in {
    val list = List()
    assert {
      list match {
        case Nil => true
        case ::(_, _, _) => false
      }
    }
  }
  ""
  "A List apply" should "return :: on not empty args" in {
    val list = List(1)
    val list2 = List("1", "two")
    assert {
      list match {
        case Nil => false
        case ::(_, _, _) => true
      }
    }
    assert {
      list2 match {
        case Nil => false
        case ::(_, _, _) => true
      }
    }
  }
}
