package com.ntonyo.github.collection

sealed trait List[+A] {
  val size: Int
}

case object Nil extends List[Nothing] {
  override val size: Int = 0
}
case class ::[A](head: A, tail: List[A], size: Int) extends List[A]

object List {
  def apply[A](source: A*):List[A] = {

    if(source.isEmpty){
      Nil
    }
    else {
      ::(source.head, apply(source.tail:_*), source.size)
    }
  }

  implicit class RichList[A](source: List[A]) {
    def foldRight[B](z: B)(f:(A, B) => B): B = {
      def move (source: List[A]):B = {
        source match {
          case Nil => z
          case ::(x, xs, _) => f(x, move(xs))
        }
      }
      move(source)
    }

    def apply(i: Int): A = {
      if(i > source.size) {
        throw new IndexOutOfBoundsException
      }
      @annotation.tailrec
      def move (i: Int, source: List[A]): A = {
        val ::(head, tail, _) = source
        if (i == 0) {
          head
        }
        else move(i-1, tail)
      }
      move(i, source)
    }

    def map[B](f: A => B) : List[B] = {
      def move(source: List[A]): List[B] = {
        source match {
          case Nil => Nil
          case ::(x, xs, _) => ::(f(x), move(xs), source.size)
        }
      }
      move(source)
    }
  }
}
