package lambdaconf.effects

import matryoshka._
import monocle._
import scalaz._
import Scalaz._
import lambdaconf.effects.exercise2.Free

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

  object Console {
    val putStrLn : String => IO[Unit] =
      line => IO(unsafePerformIO = () => println(line))
    val getStrLn : IO[String] =
      IO(unsafePerformIO = () => readLine())
  }

  final case class IO[A](unsafePerformIO: () => A)
  implicit val IOMonad: Monad[IO] = new Monad[IO] {
    def point[A](a: => A): IO[A] = IO(() => a)
    def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      IO(() => f(fa.unsafePerformIO()).unsafePerformIO())
  }

  import Console._

  val program : IO[String] =
    for {
      _ <- putStrLn("Hello! What is your name?")
      n <- getStrLn
      _ <- putStrLn("Hello, " + n + ", good to meet you!")
    } yield n

  final case class State[S, A](run: S => (S, A))

  object State {
    def gets[S]: State[S, S] = State(s => (s, s))

    def puts[S](s2: S): State[S, Unit] = State(s => (s2, ()))

    def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
  }

  implicit def StateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
    def point[A](a: => A): State[S, A] =
      State(s => (s, a))

    def bind[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      State(s => {
        val (s2, a) = fa.run(s)

        f(a).run(s2)
      })
  }

  // case class Box[A](value: A)
  // implicit val BoxFunctor = new Functor[Box] {
  //   def map[A, B](fa: Box[A])(f: A => B): Box[B] =
  //     Box(f(fa.value))
  // }
  // implicit class FunctorSyntax[F[_], A](self: F[A]) {
  //   def map[B](f: A => B)(implicit F: Functor[F]): F[B] =
  //     F.map(self)(f)
  // }
  // val box = Box(1)
  // box.map(a => a.toString)

  import State._

  val program2 : State[Int, Int] =
    for {
      i <- gets[Int]
      _ <- puts(i + 1)
      _ <- modify[Int](_ + 2)
      i <- gets[Int]
    } yield i

  program2.run(10)

  // implicit val ListTraverse = new Traverse[List] {
  //   def sequence[G[_]: Applicative, A](fga: List[G[A]]): G[List[A]] =
  //     fga match {
  //       case Nil => Applicative[G].point(List.empty[A])
  //       case a :: as => (a |@| sequence[G, A](as))(_ :: _)
  //     }
  // }

  trait MonadIO[F[_]] {
    def capture[A](effect: => A): F[A]

    def attempt[A](fa: F[A]): F[Either[String, A]]
  }
  implicit val MonadIOIO = new MonadIO[IO] {
    def capture[A](effect: => A): IO[A] =
      IO(() => effect)

    def attempt[A](fa: IO[A]): IO[Either[String, A]] =
      IO(() => {
        try { Right(fa.unsafePerformIO()) }
        catch {
          case e : Throwable => Left(e.toString())
        }
      })
  }

  sealed trait Level
  case object Fine extends Level
  case object Debug extends Level

  trait MonadLog[F[_]] {
    def log(level: Level, line: String): F[Unit]
  }
  implicit val MonadLogMonadIO = new MonadLog[IO] {
    def log(level: Level, line: String): IO[Unit] =
      IO(() => println(level.toString() + ": " + line))
  }

  def doLog[F[_]: MonadLog]: F[Unit] = ???

  def doIO[F[_]: MonadIO]: F[Unit] = ???

  def program[F[_]: MonadIO: MonadLog]: F[Unit] = ???

  def main = program[IO].unsafePerformIO()

  trait Empty[F[_]] {
    def empty[A]: F[A]
  }
  implicit val ListEmpty = new Empty[List] {
    def empty[A]: List[A] = Nil
  }
  trait Consable[F[_]] {
    def cons[A](a: A, as: F[A]): F[A]
  }
  implicit val ListConsable = new Consable[List] {
    def cons[A](a: A, as: List[A]): List[A] = a :: as
  }
  trait Unconsable[F[_]] {
    def uncons[A](fa: F[A]): Option[(A, F[A])]
  }
  implicit val ListUnconsable = new Unconsable[List] {
    def uncons[A](fa: List[A]): Option[(A, List[A])] =
      fa match {
        case Nil => None
        case a :: as => Some((a, as))
      }
  }

  def build1[F[_]: Consable: Empty]: F[Int] = {
    val C = implicitly[Consable[F]]
    val E = implicitly[Empty[F]]

    C.cons(1, E.empty[Int])
  }

  build1[List] : List[Int]

  case class OptionT[F[_], A](run: F[Option[A]])
  object OptionT {
    def some[F[_]: Applicative, A](a: A): OptionT[F, A] =
      OptionT(Option(a).point[F])

    def none[F[_]: Applicative, A]: OptionT[F, A] =
      OptionT(Option.empty[A].point[F])

    def lift[F[_]: Applicative, A](fa: F[A]): OptionT[F, A] =
      OptionT(fa.map(Option(_)))
  }

  implicit def OptionTMonad[F[_]: Monad] = new Monad[OptionT[F, ?]] {
    def point[A](a: => A): OptionT[F, A] =
      OptionT(Option(a).point[F])

    def bind[A, B](fa: OptionT[F, A])(f: A => OptionT[F,B]): OptionT[F, B] =
      OptionT(fa.run.flatMap {
        case None => Option.empty[B].point[F]
        case Some(a) => f(a).run
      })
  }

  type OptionalIO[A] = OptionT[IO, A]

  val o : OptionT[IO, Int] = OptionT.some(1)
}



object exercise2 {

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


object myexercise2 {
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

  case class ReadLine[A](line: String => A) extends ConsoleF[A]
  case class WriteLine[A](line: String, next: A) extends ConsoleF[A]
  case class RandomNumber[A](num: Double => A) extends ConsoleF[A]

  sealed trait Free[F[_], A]
  final case class Point[F[_], A](value: A) extends Free[F, A]
  final case class Effect[F[_], A](effect: F[A]) extends Free[F, A]
  final case class Bind[A0, F[_], A](m: Free[F, A0], f: A0 => Free[F, A]) extends Free[F, A]

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Effect(fa)

  def foldFree[F[_], M[_]: Monad, A](fa: Free[F, A])(f: F ~> M): M[A] =
    fa match {
      case Point(a) => a.point[M]
      case Effect(fa) => f(fa)
      case b : Bind[a0, F, A] =>
        foldFree(b.m)(f).flatMap(a0 =>
          foldFree[F, M, A](b.f(a0))(f)
        )
    }

  implicit def MonadFree[F[_]]: Monad[Free[F, ?]] = new Monad[Free[F, ?]] {
    def point[A](a: => A): Free[F,A] =
      Point[F, A](a)

    def bind[A, B](fa: Free[F,A])(f: A => Free[F,B]): Free[F,B] =
      Bind[A, F, B](fa, f)
  }

  type Console[A] = Free[ConsoleF, A]

  def getStrLn: Console[String] = liftF(ReadLine(a => a))
  def putStrLn(line: String): Console[Unit] = liftF(WriteLine(line, ()))
  def getRandom: Console[Double] = liftF(RandomNumber(a => a))

  val getNumber: Console[Int] =
    for {
      _ <- putStrLn("Guess a number between 0 and 10: ")
      g <- getStrLn
      i <- scala.util.Try(g.toInt).toOption.fold(getNumber)(i => i.point[Console])
    } yield i

  val loop: Console[Unit] =
    for {
      i <- getNumber
      r <- getRandom
      val r2 = (r * 10.5).round.toInt
      _ <- if (i == r2) putStrLn("You're a WINNER!")
           else putStrLn("No, sorry. Try again.").flatMap(_ => loop)
    } yield ()

  val program : Console[String] =
    for {
      _ <- putStrLn("Hello! What is your name?")
      n <- getStrLn
      _ <- if (n == "John") putStrLn("Hello, instructor!")
           else putStrLn("Hello, attendee, " + n + "!")
    } yield n

  val consoleFToIO: ConsoleF ~> IO = new NaturalTransformation[ConsoleF, IO] {
    def apply[A](v: ConsoleF[A]): IO[A] = v match {
      case ReadLine(f) => IO(() => f(readLine()))
      case WriteLine(l, a) => IO(() => { println(l); a })
      case RandomNumber(f) => IO(() => f(scala.util.Random.nextDouble()))
    }
  }

  // foldFree(program)(consoleFToIO).unsafePerformIO()

  sealed trait StateF[S, A]
  case class Gets[S, A](next: S => A) extends StateF[S, A]
  case class Puts[S, A](s: S, next: A) extends StateF[S, A]

  type SafeState[S, A] = Free[StateF[S, ?], A]

  type Trampoline[A] = Free[Function0, A]
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
