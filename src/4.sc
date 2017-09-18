//hide std library `Option` and `Either`, since we are writing our own in this chapter
package fpinscala


import scala.{Either => _, Option => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(val) => Some(f(val))

  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => None
    case Some(val) => val
  }


  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(val) => this
  }

  def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a) Some(a) else None)
}


case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]