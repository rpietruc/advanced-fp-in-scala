package lambdaconf.types

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {

  object mysolution {
    case class Person(age: Int, name: String)
    case class Address(street: String, zip: String, state: String, country: String)

    type LocatedPerson = (Person, Address)

    case class LocatedPerson2(person: Person, address: Address)

    final case class Line[A](p1: A, p2: A, p3: A, p4: A)
    final case class CheckersBoard(value: Line[Line[Option[CheckerPiece]]])

    sealed trait CheckerPiece
    case object RedPiece extends CheckerPiece
    case object BlackPiece extends CheckerPiece
    case object CrownedRedPiece extends CheckerPiece
    case object CrownedBlackPiece extends CheckerPiece
  }

  object solution1 {
    final case class CheckersBoard[A](matrix: List[List[A]])
  }
  object solution2 {
    type BoardRow[A] = (A, A, A, A, A, A, A, A)
    final case class CheckersBoard[A](matrix: BoardRow[BoardRow[A]])
  }
  object solution3 {
    type BoardRow[A, B] = (A, B, A, B, A, B, A, B)
    final case class CheckersBoard[A, B](matrix: BoardRow[BoardRow[A, B], BoardRow[B, A]])
  }
  sealed trait CheckerPiece
  final case object WhitePiece extends CheckerPiece
  final case object BlackPiece extends CheckerPiece
}

object exercise2 {
  trait Show[A] {
    def show(a: A): String
  }

  object Show {
    //to be able to write Show[Int].show(1)
    def apply[A](implicit S: Show[A]): Show[A] = S

    implicit val showInt: Show[Int] = new Show[Int] {
      def show(a: Int): String = a.toString
    }
    implicit val showString: Show[String] = new Show[String] {
      def show(a: String): String = a
    }
    implicit def showList[A](implicit S: Show[A]) = new Show[List[A]] {
      def show(l: List[A]): String = l.map(S.show(_)).mkString
    }
    //<- to companion object
  }
  Show[Int].show(1)

  //to be able to write e.g. 1.show
  implicit class ShowSyntax[A: Show](value: A) {
    def show: String = Show[A].show(value)
  }
  //1.show

  final case class Box[A](value: A)
  object Box {
    //-> to companion object of user class
    implicit def showBox[A](implicit S: Show[A]) = new Show[Box[A]] {
      def show(b: Box[A]): String = S.show(b.value)
    }
  }

  type RectangleLike = {
    def x: Int
    def y: Int
  }
  case class Square()
}

object exercise3 {
  // 1. scala.collection.List: * => *
  // 2. F[_, _]: [*, *] => *
  // 3. Option: * => * (_[_])
  // 4. Int: *
  // 5. T[_[_], _]: [* => *, *] => * (FreeMonad: Free[F, A])
  // T : [* => *, *] => *
  // T : (* => *) => * => *
  // T[List, Int]

  def magicAlgorithm[T[_[_], _], A](input: T[Option, A]): T[Option, A] =
    ???

  (_ + 1): (Int => Int)

  // _[_]: * => *
  // T[_[_]] (* => *) => *
  trait Foo[T[_[_]]] {
  }
  // Foo: ((* => *) => *) => *
  // always limited to return single star

  trait Traversable[F[_]] { // F: * => * (kind)
    def foldLeft[A, Z](fa: F[A], initial: Z)(f: (Z, A) => Z): Z = ???
  }
  // (Kind of) Traversable: (* => *) => *
  object Traversable {
    implicit val TraversableList: Traversable[List] = ??? // * => *

    // partially aplied type
    //val TraversableMap: Traversable[Map] = ??? // ! (*, *) = > *

    // Type alias, partially applied
    object solution1 {
      implicit def TraversableMap[A]: Traversable[({ type MapA[B] = Map[A, B] })#MapA] = {
        type MapA[B] = Map[A, B]
        new Traversable[({ type MapA[B] = Map[A, B] })#MapA] { // * => *
          def foldLeft[B, Z](fa: Map[A, B], initial: Z)(f: (Z, A) => Z): Z = ???
        }
      }
    }
    object solution2 {
      implicit def TraversableMap2[A]: Traversable[Map[A, ?]] = {
        new Traversable[Map[A, ?]] { // * => *
          def foldLeft[B, Z](fa: Map[A, B], initial: Z)(f: (Z, A) => Z): Z = ???
        }
      }
    }
  }

  val baz: Traversable[List] = ???
  val foo: Foo[Traversable] = ???
}

object exercise4 {
  trait StackModule {
    type Stack[_]

    def newStack[A]: Stack[A]

    def push[A](v: A, s: Stack[A]): Stack[A]

    def pop[A](s: Stack[A]): Option[(A, Stack[A])]
  }

  trait SmartList[A] { self =>
    type A0

    val list : List[A0]

    val f0 : A0 => A

    def run: List[A] = list.map(f0)

    def map[B](f: A => B): SmartList[B] =
      new SmartList[B] {
        type A0 = self.A0

        val list = self.list

        val f0 = self.f0.andThen(f)
      }
  }
  object SmartList {
    def apply[A](l: List[A]): SmartList[A] = new SmartList[A] {
      type A0 = A

      val f0 = (a: A) => a

      val list = l
    }
  }

  trait FileSystem {
    // ???
  }
}

object exercise5 {
  //dummy example (e.g. example of option)
  sealed trait Example[F[_]] {
    def value: F[String]
  }

  val ExampleOption: Example[Option] = new Example[Option] {
    def value: Option[String] = Some("Hello World")
  }

  val ExampleList: Example[List] = new Example[List] {
    def value: List[String] = ExampleOption.value.toList
  }

  /*
  def foo1[B](v: B): Example[({type Apply[A] = Either[A, B]})#Apply] = new Example[({type Apply[A] = Either[A, B]})#Apply] { // <-- ???
    def value: Either[String, B] = Right(v)
  }
  def foo2[B](v: B): Example[Either[?, B]] = new Example[Either[?, B]] {
    def value: Either[String, B] = Right(v)
  }
  */

  object solution1 {
    type EitherAInt[A] = Either[A, Int] //alias
    val ExampleEither: Example[EitherAInt] = new Example[EitherAInt] {
      def value: EitherAInt[String] = Right(2)
    }
    type MapToInt[A] = Map[A, Int]
    val MapExample: Example[MapToInt] = new Example[MapToInt] {
      def value: MapToInt[String] = ExampleList.value.zipWithIndex.toMap
    }
  }

  object solution2 {
    val ExampleEither: Example[({type EitherAInt[A] = Either[A, Int]})#EitherAInt] = new Example[({type EitherAInt[A] = Either[A, Int]})#EitherAInt] {
      def value: Either[String, Int] = Right(2)
    }
  }

  object solution3 {
    val ExampleEither: Example[Either[?, Int]] = new Example[Either[?, Int]] {
      def value: Either[String, Int] = Right(2)
    }
    val MapExample: Example[Map[?, Int]] = new Example[Map[?, Int]] {
      def value: Map[String, Int] = ExampleList.value.zipWithIndex.toMap
    }
  }
}
