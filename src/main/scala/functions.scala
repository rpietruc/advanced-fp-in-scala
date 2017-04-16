package lambdaconf.functions

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
  // Domain: {Vegetables, Fruits, Meat, Dairy, Eggs}
  // Codomain: {Love, Like, Neutral, Dislike, Hate}
}

object exercise2 {
  val compareStrings: (Char => Char) => (String, String) => Boolean = ???
}

object exercise3 {
  type Error = String
  type Parser[A] = String => Either[Error, (String, A)]

  // function combinator
  def or[A](left: Parser[A], right: Parser[A]): Parser[A] =
    (input: String) => left(input) match {
      case Left(error) => right(input)
      case x => x
    }

  case object identity {
    def apply[A](value: A): A = value
  }
  //identify.apply type = ( A: Type => a: A = A )
  // takes types and values
  identity(3)   // 3
  identity("3") // "3"

  def gimmeAListUniversal[A]: List[A] = ??? //we choose a type
  def gimmeAListExistential: List[A] forSome { type A } =
    1 :: 2 :: Nil

  gimmeAListExistential.length
}

object exercise4 {
  def snd[A, B](v: (A, B)): B = v._2
}
