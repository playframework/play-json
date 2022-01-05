/*
 * Copyright (C) 2009-2021 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.functional

trait Applicative[M[_]] extends DeprecatedApplicative[M] {
  def pure[A](f: => A): M[A]
  def map[A, B](m: M[A], f: A => B): M[B]
  def apply[A, B](mf: M[A => B], ma: M[A]): M[B]
}

sealed trait DeprecatedApplicative[M[_]] { self: Applicative[M] =>

  @deprecated("Use `pure` with `f:=>A` parameter", "2.7.0")
  def pure[A](value: A): M[A] = pure(f = value)
}

object Applicative {

  implicit val applicativeOption: Applicative[Option] = new Applicative[Option] {
    def pure[A](f: => A): Option[A] = Option(f)

    def map[A, B](m: Option[A], f: A => B): Option[B] = m.map(f)

    def apply[A, B](mf: Option[A => B], ma: Option[A]): Option[B] = mf.flatMap(f => ma.map(f))
  }
}

class ApplicativeOps[M[_], A](ma: M[A])(implicit
    a: Applicative[M]
) {
  def ~>[B](mb: M[B]): M[B]      = a(a(a.pure(((_: A) => (b: B) => b): A => B => B), ma), mb)
  def <~[B](mb: M[B]): M[A]      = a(a(a.pure(((a: A) => (_: B) => a): A => B => A), ma), mb)
  def andKeep[B](mb: M[B]): M[B] = ~>(mb)
  def keepAnd[B](mb: M[B]): M[A] = <~(mb)

  def <~>[B, C](mb: M[B])(implicit
      witness: <:<[A, B => C]
  ): M[C] = apply(mb)

  def apply[B, C](mb: M[B])(implicit
      witness: <:<[A, B => C]
  ): M[C] = a(a.map(ma, witness), mb)
}
