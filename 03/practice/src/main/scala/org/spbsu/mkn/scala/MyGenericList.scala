package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import scala.runtime.Nothing$




sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[T2](f: T => T2): MyGenericList[T2] = this match {
    case MyNil => MyNil
    case _ => tail match {
      case MyNil => new MyGenericListClass[T2](f(head), MyNil)
      case l => f(head) :: l.map[T2](f)
    }
  }
  def ::[T1 >: T](elem: T1): MyGenericList[T1] = new MyGenericListClass(elem, this)

}

case class MyGenericListClass[+T](head : T, tail : MyGenericList[T] = MyNil) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T] = n match {
    case 0          => this
    case 1          => tail
    case _ if n > 1 => tail.drop(n - 1)
    case _ if n < 0  => throw new UnsupportedOperationException("operation is undefined")
  }
  override def take(n: Int): MyGenericList[T] = n match {
    case 0          => MyNil
    case 1 => head :: MyNil
    case _ if n > 1  => head :: tail.take(n - 1)
    case _ if n < 0  => throw new UnsupportedOperationException("operation is undefined")
  }
}

case object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = {
    seq.length match {
      case i if i <= 0 => MyNil
      case _           => seq.head :: fromSeq(seq.tail)
    }
  }
  def sum(list: MyGenericList[Int]): Int    = list match {
    case MyNil => MyNil.undef
    case _     => foldLeft[Int, Int](((a : Int, b : Int) => a + b), 0, list)
  }

  def size[T](list: MyGenericList[T]): Int = list match {
    case MyNil => 0
    case _     => foldLeft[T, Int](((a, b) => a + 1), 0, list)
  }

  // extra task: implement sum using foldLeft
  def foldLeft[TL, TV](f:(TV, TL)=>TV, init:TV, list: MyGenericList[TL]) : TV = list match {
    case MyNil => init
    case l : MyGenericList[TL] => foldLeft[TL, TV](f, f(init, l.head), l.tail)
  }


}

case object MyNil extends MyGenericList[Nothing] {
  override def head = undef
  override def tail = undef
  override def drop(n: Int) = undef
  override def take(n: Int) = undef
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
}

