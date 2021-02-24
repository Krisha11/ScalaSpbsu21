package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.runtime.Nothing$

sealed trait IntList extends Any{
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = {
    val tmp = this
    new IntList {
      override def head = elem
      override def tail:IntList = tmp
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


      override def equals(obj: Any): Boolean = {
        obj match {
          case IntNil      => this.hashCode() == IntNil.hashCode()
          case obj:IntList => (head == obj.head && tail.equals(obj.tail))
          case _           => false
        }
      }
    }
  }

   override def equals(obj: Any): Boolean = {
     obj match {
       case IntNil      => this.hashCode() == IntNil.hashCode()
       case obj:IntList => (head == obj.head && tail.equals(obj.tail))
       case _           => false
     }
   }

}

object IntList {
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

object IntNil extends IntList {
  override def head: Int = throw new UnsupportedOperationException("operation is undefined")
  override def tail: IntList = IntNil
  override def drop(n: Int): IntList = throw new UnsupportedOperationException("operation is undefined")
  override def take(n: Int): IntList = throw new UnsupportedOperationException("operation is undefined")
  override def map(f: Int => Int): IntList = IntNil
  override def equals(obj: Any): Boolean = {
    obj.hashCode() == IntNil.hashCode()
  }
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def sum(intList: IntList): Int      = throw new UnsupportedOperationException("operation is undefined")
  def size(intList: IntList): Int     = 0
}

