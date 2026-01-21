package lumens.internal

import scala.util.Failure
import scala.util.Success
import scala.util.Try

trait MonadError[F[_]]:
    def unit[T](t: T): F[T]
    def map[T, T2](fa: F[T])(f: T => T2): F[T2]
    def flatMap[T, T2](fa: F[T])(f: T => F[T2]): F[T2]
    def error[T](t: Throwable): F[T]
    def ensure[T](fa: F[T], e: => F[Unit]): F[T]
    def eval[T](t: => T): F[T]     = map(unit(()))(_ => t)
    def blocking[T](t: => T): F[T] = eval(t)
end MonadError

type Identity[T] = T

object IdentityMonadError extends MonadError[Identity]:
    override def unit[T](t: T): Identity[T]                                          = t
    override def map[T, T2](fa: Identity[T])(f: T => T2): Identity[T2]               = f(fa)
    override def flatMap[T, T2](fa: Identity[T])(f: T => Identity[T2]): Identity[T2] = f(fa)
    override def error[T](t: Throwable): Identity[T]                                 = throw t
    override def eval[T](t: => T): Identity[T]                                       = t
    override def ensure[T](f: Identity[T], e: => Identity[Unit]): Identity[T]        =
        try f
        finally e
