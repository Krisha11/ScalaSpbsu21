package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.runtime.Nothing$

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = MyIntList(elem, this)
}

case class MyIntList(head : Int, tail : IntList = IntNil) extends IntList {
  override def drop(n: Int): IntList = n match {
    case 0          => this
    case 1          => tail
    case _ if n > 1 => tail.drop(n - 1)
    case _ if n < 0  => throw new UnsupportedOperationException("operation is undefined")
  }
  override def take(n: Int): IntList = n match {
    case 0          => IntNil
    case 1 => head :: IntNil
    case _ if n > 1  => head :: tail.take(n - 1)
    case _ if n < 0  => throw new UnsupportedOperationException("operation is undefined")
  }
  override def map(f: Int => Int): IntList = f(head)::tail.map(f)
}


case object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = {
    seq.length match {
      case i if i <= 0 => IntNil
      case _           => seq.head :: fromSeq(seq.tail)
    }
  }
  def sum(intList: IntList): Int      = intList match {
    case IntNil => throw new UnsupportedOperationException("operation is undefined")
    case _      => foldLeft(((a, b) => a + b), 0, intList)
  }

  def size(intList: IntList): Int     = foldLeft(((a, b) => a + 1), 0, intList)

  // extra task: implement sum using foldLeft
   def foldLeft(f:(Int, Int)=>Int, init:Int, intList: IntList)/*(???)*/: Int = intList match {
     case IntNil => init
     case l => foldLeft(f, f(init, l.head), l.tail)
   }

}

case object IntNil extends IntList {
  override def head: Int = throw new UnsupportedOperationException("operation is undefined")
  override def tail: IntList = IntNil
  override def drop(n: Int): IntList = throw new UnsupportedOperationException("operation is undefined")
  override def take(n: Int): IntList = throw new UnsupportedOperationException("operation is undefined")
  override def map(f: Int => Int): IntList = IntNil
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def sum(intList: IntList): Int      = throw new UnsupportedOperationException("operation is undefined")
  def size(intList: IntList): Int     = 0
}

