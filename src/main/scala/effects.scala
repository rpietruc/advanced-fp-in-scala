package lambdaconf.effects

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
  final case class IO[A](unsafePerformIO: () => A) {
    self =>

    def pure[A](a: A): IO[A] = IO { () =>
      a
    }
    def map[B](f: A => B): IO[B] = IO { () =>
      f(self.unsafePerformIO())
    }
    def flatMap[B](f: A => IO[B]): IO[B] = IO { () =>
      f(self.unsafePerformIO()).unsafePerformIO()
    }
    def attempt: IO[Either[Throwable, A]] = IO { () =>
      try  {
        Right(self.unsafePerformIO())
      } catch {
        case t: Throwable => Left(t)
      }
    }
  }

  implicit val IOMonad: Monad[IO] = ???
}

object exercise2 {
  import exercise1._

  //turn effects into description
  object Console {
    def putStrLen(s: String): IO[Unit] = IO { () =>
      println(s)
    }
    def getStrLen: IO[String] = IO { () =>
      readLine()
    }
  }
  object ConsoleExample {
    import Console._
    val main = for {
      _ <- putStrLen("name")
      name <- getStrLen
      _ <- putStrLen(name)
    } yield ()
  }

  trait Op[In, Out, A] {
    def in: In
    def out: Out => A //operations
  }
  type ReadOnlyOp[Out, A] = Op[Unit, Out, A]
  type WriteOnlyOp[In, A] = Op[In, Unit, A]

  trait ConsoleOp[In, Out, A] extends Op[In, Out, A]
  case class ReadLine[A](handle: String => A) extends ConsoleOp[Unit, String, A] {
    def in = readLine()
    def out = handle
  }
  case class WriteLine[A](in: String, a: A) extends ConsoleOp[String, Unit, A] {
    def out = _ => a
  }

  // def readLine: ConsoleIO[String] = Free.liftF(
  //   ReadLine(line => line)
  // )
  // def write(line: String): ConsoleIO[Unit] = Free.liftF(
  //   WriteLine(line, ())
  // )

  //Free monad gives sequencial context
  sealed trait Free[F[_], A] { self =>
    def map[B](f0: A => B): Free[F, B] = new Map[F, B] {
      type A0 = A
      def fa0 = self
      def f = f0
    }
    def flatMap[B](f0: A => Free[F, B]): Free[F, B] = new FlatMap[F, B] {
      type A0 = A
      def fa0 = self
      def f = f0
    }
  }

  sealed trait ConsoleF[A]
  type ConsoleIO[A] = Free[ConsoleF, A]

  //def interpret[A](mockSpec: mockSpec[F])

  //def interpretIO
  //def interpretS[A](program: ConsoleIO[A]): Free[SocketF, A] = ???

  final case class Effect[F[_], A](op: F[A]) extends Free[F, A]

  final case class Pure[F[_], A](a: A) extends Free[F, A]
  abstract class Map[F[_], A] extends Free[F, A] {
    type A0
    def fa0: Free[F, A0]
    def f: A0 => A
  }

  abstract class FlatMap[F[_], A] extends Free[F, A] {
    type A0
    def fa0: Free[F, A0]
    def f: A0 => Free[F, A]
  }

  val program: Free[ConsoleF, Unit] = ???

  object Free {
    def pure[F[_], A](a: A): Free[F, A] = Pure[F, A](a)
    def liftF[F[_], A](op: F[A]): Free[F, A] = Effect(op)
  }
}

object exercise3 {

  object solution1 {
    case class StateMachine[S](initial: S, update: S => S)
    def gimmeStateMachine: StateMachine[S] forSome { type S } = ???
  }

  //existential function
  object solution2 {
    trait StateMachine[A] {
      type State
      def initial: State
    }
  }

  object solution3 {
    case class StateMachine[S, A](initial: S, update: S => (S, A))
    def gimmeStateMachine: StateMachine[S, Int] forSome { type S } = ???
  }

  object solution4 {
    trait StateMachine[S, A] {
      def initial: S
      def update(old: S): (S, A)
    }

    trait HandleStateMachine[A, Z] {
      def apply[S](sm: StateMachine[S, A]): Z
    }

    trait ExistentialStateMachine[A] {
      def apply[Z](sm: HandleStateMachine[A, Z]): Z
    }

    val sm: ExistentialStateMachine[Int] = ???
    // sm.apply[List[Int]](new HandleStateMachine[Int, List[Int]]) {
    //   def apply[S](sm: StateMachine[S, Int]): List[Int] = {
    //     val initial: S = sm.initial
    //
    //   }
    // }
  }

  // append(append(a, b), c) == append(a, append(b, c))
  trait Semigroup[A] {
    def append(a1: A, a2: A): A
  }

  object Semigroup {
    def apply[A](implicit S: Semigroup[A]): Semigroup[A] = S
    implicit val stringSemigroup: Semigroup[String] = new Semigroup[String] {
      def append(a1: String, a2: String): String = a1 + a2
    }
    // ???
    // ???
    // implicit def numericSemigroup[A: Numeric]: Semigroup[A] = new Semigroup[A] {
    //   def append(a1: A, a2: A): A = implicitly(Numeric)(a1.plus(a2))
    // }
  }

  trait Partitionable[F[_]] {
    def partition[A, B, C](fa: F[A], p: A => Either[B, C]): (F[B], F[C])
  }
  object Partitionable {
    implicit val partitionableList: Partitionable[List] = new Partitionable[List] {
      def partition[A, B, C](fa: List[A], p: A => Either[B, C]): (List[B], List[C]) = {
        val ps = fa.map(p)
        (ps.collect { case Left(l) => l },
         ps.collect { case Right(r) => r })
        //foldLeft() ???
      }
    }
  }
  // implicit class SemigroupSyntax[A](a: A) extends AnyVal {
  //   def <> (that: A)(implicit S: Semigroup[A]): A = S.append(a, that)
  // }

  //covariant endofuntor === functor
  //identity law: map(fa)(id) == fa
  //map(fa)(f.compose(g)) == map(map(fa)(g), f)
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  // object Functor {
  //   implicit val optionFunctor: Functor[Option] = new Functor[Option] {
  //     def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
  //       case Some(a) => Some(f(a))
  //       case None => None
  //     }
  //   }
  // }
  //combine 2 futures from any may fail
  trait Apply[F[_]] extends Functor[F] {
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    // def ap[A, B](fab: F[A=> B], fa: F[A]): F[B] =
    //   map(zip(fab, fa))(t => t._1(t._2))
  }
  // object Apply {
  //   implicit val optionApply: Apply[Option] = new Apply[Option] {
  //     def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
  //       case (Some(a), Some(b)) => Some((a, b))
  //       case _ => None
  //     }
  //   }
  // }
  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](a: A): F[A]
  }
  // runtime dependent sequential computation
  trait Monad[F[_]] extends Applicative[F] {
    def join[A](ffa: F[F[A]]): F[A]
    // ensure the order of computation
    // runtime value (A) dictates behaviour of computation (F[B])
    // monads are powerful
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      join(map(fa)(f))
    //monad is an essence of imperative programming
  }
  object Monad {
    implicit val optionMonad: Monad[Option] = new Monad[Option] {
      def pure[A](a: A): Option[A] = Some(a)

      def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
      def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
        case (Some(a), Some(b)) => Some((a, b))
        case _ => None
      }
      def join[A](ffa: Option[Option[A]]): Option[A] = ffa match {
        case Some(Some(a)) => Some(a)
        case _ => None
      }
    }
  }
  implicit def StateMonad[S]: Monad[State[S, ?]] = ???

  final case class State[S, A](run: S => (S, A)) {
    def evalState(s: S): A = ???
  }
  // object State {
  //   implicit def stateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
  //     def pure[A](a: A): State[S, A] = State(s => (s, a))
  //     def get: State[S, S] = State(s => (s, s))

  //     def modify(f: S => S): State[S, S] = State(
  //       s => {
  //         val s2 = f(s)
  //         (s2, s2)
  //       })

  //     def zip[A, B](l: State[S, A], r: State[S, B]): State[S, (A, B)] = State[S, (A, B)] {
  //       s =>
  //       val (s2, a) = l.run(s)
  //       val (s3, b) = r.run(s2)
  //       (s3, (a, b))
  //     }
  //     def map[A, B](fa: State[A])(f: A => B): State[B] = fa match {
  //     }
  //     def join[A](ffa: State[State[A]]): State[A] = ffa match {
  //       case Some(Some(a)) => Some(a)
  //       case _ => None
  //     }
  //   }
  // }
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }
  object Monoid {
    // implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    //   val empty = ""
    // }
  }
  trait SerialString[A] {
    def serialize(a: A): String
    def deserialize(v: String): Either[String, A]
  }
  // implicit laws: round trip
  // def roundtriplaw[A: SerialString](a: A) =
  //   a.serialize.deserialize[A] must beRight(a)

}

object exercise4 {
  import exercise3._

  def sort[A: Order](list: List[A]): List[A] =
    (??? : State[List[A], List[A]]).evalState(list)
}
