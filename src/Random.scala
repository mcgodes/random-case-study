package random

object RandomTest extends App {
  val a = Random.double
  val b = Random.double
  val a2 = a.map(_*2)
  println(a)
  println(a2)
  println(b)
}


sealed trait Random[A] {
  def run(rng: scala.util.Random): A = this match {
    case Primitive(f) => f(rng)
    case FlatMap(r, f) => f(r.run(rng)).run(rng)
    case Map(r, f) => f(r.run(rng))
  }

  def flatMap[B](f: A => Random[B]): Random[B] = FlatMap(this, f)

  def map[B](f: A => B): Random[B] = Map(this, f)

  def zip[B](that: Random[B]): Random[(A,B)] = {
    for {
      a <- this
      b <- that
    } yield (a,b)
  }
}

object Random {
  val double: Random[Double] = Primitive(rng => rng.nextDouble)

  val int: Random[Int] = Primitive(rng => rng.nextInt)

  /** Generate a value from a normal or Gaussian distribution. */
  val normal: Random[Double] = Primitive(rng => rng.nextGaussian())

  /** Create a Random value that always generates `in`. */
  def always[A](in: A): Random[A] = Primitive(rng => in)

  val point: Random[Point] = (Random.double zip Random.double).map { (pt: (Double, Double)) =>
    val (x, y) = pt
    Point(x, y)
  }

  def vec(x: Random[Double], y: Random[Double]): Random[Vec] = {
    x.zip(y).map { case (a,b)  => Vec(a, b) }
  }

  def circle(radius: Random[Double]): Random[Circle] = {
    radius.zip(Random.point).map { case (r, p) => Circle(p, r) }
  }

  def randomWalk(point: Random[Vec], noise: Random[Vec]): Random[Vec] = {
    point.zip(noise).map {
      case (p, n) => Vec(p.x + n.x, p.y + n.y)
    }
  }
}

case class Point(x: Double, y: Double)
case class Circle(origin: Point, radius: Double)

final case class Primitive[A](f: scala.util.Random => A) extends Random[A]
final case class FlatMap[A,B](random: Random[A], f: A => Random[B]) extends Random[B]
final case class Map[A,B](random: Random[A], f: A => B) extends Random[B]