package dev.habla

type Parser[A] = String => List[(A, String)]

// Introduces parser combinators and implements some of them
object ParserCombinators1:
  
  def result[A](a: A): Parser[A] = s => List((a, s))

  def zero[A]: Parser[A] = _ => Nil
  
  val item: Parser[Char] = _ match
    case "" => List.empty
    case s => List((s.head, s.tail))

  def seq[A, B](p: Parser[A], q: Parser[B]): Parser[(A, B)] = inp => {
    for {
      (a, inp2) <- p(inp)
      (b, inp3) <- q(inp2)
    } yield ((a, b), inp3)
  }

  def bind[A, B](p: Parser[A], f: A => Parser[B]): Parser[B] = inp => {
    for {
      (a, inp2) <- p(inp)
      (b, inp3) <- f(a)(inp2)
    } yield (b, inp3)
  }

  def sat(p: Char => Boolean): Parser[Char] =
    bind(item, c => if (p(c)) result(c) else zero)

  def char(c: Char): Parser[Char] = sat(_ == c)

  def digit: Parser[Char] = sat(c => '0' <= c && c <= '9')

  def lower: Parser[Char] = sat(c => 'a' <= c && c <= 'z')

  def upper: Parser[Char] = sat(c => 'A' <= c && c <= 'Z')

  def [A](p: Parser[A]) ++ (q: Parser[A]): Parser[A] = { inp =>
    p(inp) ++ q(inp)
  }

  def letter: Parser[Char] = lower ++ upper

  def alphanum: Parser[Char] = letter ++ digit

  def word: Parser[String] =
    result("") ++ bind(letter, { c => 
      bind(word, { cs => 
        result(s"$c$cs")
      })
    })

end ParserCombinators1

// Supply type classes and exploit for-comprehension notation
object ParserCombinators2:

  trait Monad[M[_]]:
    def result[A](a: A): M[A]
    def [A, B](ma: M[A]) flatMap (f: A => M[B]): M[B] 
    def [A, B](ma: M[A]) map (f: A => B): M[B] = ma flatMap (a => result(f(a)))

  object Monad:
    def apply[M[_]](using m: Monad[M]): Monad[M] = m

  trait MonadPlus[M[_]] extends Monad[M]:
    def zero[A]: M[A]
    def [A](m: M[A]) ++ (n: M[A]): M[A]
    def [A](ma: M[A]) withFilter (p: A => Boolean): M[A] = 
      ma.flatMap(a => if (p(a)) result(a) else zero)

  given mparser as MonadPlus[Parser]:
    def result[A](a: A): Parser[A] = s => List((a, s))
    def [A, B](p: Parser[A]) flatMap (f: A => Parser[B]): Parser[B] = inp => {
      for {
        (a, inp2) <- p(inp)
        (b, inp3) <- f(a)(inp2)
      } yield (b, inp3)
    }
    def zero[A]: Parser[A] = _ => List()
    def [A](p: Parser[A]) ++ (q: Parser[A]): Parser[A] = inp => p(inp) ++ q(inp)

  import ParserCombinators1._
  import mparser.result

  def sat(p: Char => Boolean): Parser[Char] =
    for { c <- item if (p(c)) } yield c

  def char(c: Char): Parser[Char] = sat(_ == c)

  def string(s: String): Parser[String] =
    s match
    case "" => result("")
    case _ =>
      for {
        c  <- char(s.head)
        cs <- string(s.tail)
      } yield s"$c$cs"

  def word: Parser[String] =
    (for {
      c <- letter
      cs <- word
    } yield s"$c$cs") ++ result("")

  def many[A](p: Parser[A]): Parser[List[A]] =
    (for {
      a <- p
      as <- many(p)
    } yield a :: as) ++ result(List.empty)

  val ident: Parser[String] =
    for {
      c  <- lower
      cs <- many(alphanum)
    } yield s"""$c${cs.mkString("")}"""

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      a <- p
      as <- many(p)
    } yield a :: as

  def nat: Parser[Int] =
    for {
      ds <- many1(digit)
    } yield ds.mkString("").toInt

  def int: Parser[Int] =
    (for {
      _ <- char('-')
      n <- nat
    } yield -n) ++ nat

end ParserCombinators2

