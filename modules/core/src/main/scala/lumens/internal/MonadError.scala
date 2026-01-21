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

    protected def handleWrappedError[T](rt: F[T])(h: PartialFunction[Throwable, F[T]]): F[T]
    def handleError[T](rt: => F[T])(h: PartialFunction[Throwable, F[T]]): F[T] =
        try handleWrappedError(rt)(h)
        catch
            case e: Throwable if h.isDefinedAt(e) => h(e)
            case e: Throwable                     => error(e)

    def eval[T](t: => T): F[T]                      = map(unit(()))(_ => t)
    def suspend[T](t: => F[T]): F[T]                = flatten(eval(t))
    def flatten[T](ffa: F[F[T]]): F[T]              = flatMap[F[T], T](ffa)(identity)
    def flatTap[T, U](fa: F[T])(f: T => F[U]): F[T] = flatMap(fa)(t => map(f(t))(_ => t))
    def fromTry[T](t: Try[T]): F[T]                 = t match
        case Success(v) => unit(v)
        case Failure(e) => error(e)
    def blocking[T](t: => T): F[T]                  = eval(t)
end MonadError

type Identity[T] = T

object IdentityMonadError extends MonadError[Identity]:
    override def unit[T](t: T): Identity[T]                                          = t
    override def map[T, T2](fa: Identity[T])(f: T => T2): Identity[T2]               = f(fa)
    override def flatMap[T, T2](fa: Identity[T])(f: T => Identity[T2]): Identity[T2] = f(fa)
    override def error[T](t: Throwable): Identity[T]                                 = throw t
    override protected def handleWrappedError[T](rt: Identity[T])(
        h: PartialFunction[Throwable, Identity[T]]
    ): Identity[T] = rt
    override def eval[T](t: => T): Identity[T]                                       = t
    override def ensure[T](f: Identity[T], e: => Identity[Unit]): Identity[T]        =
        try f
        finally e
