package lumens.internal

import scala.Tuple.*

abstract class Zippable[-A, -B]:
    type Out <: Tuple

    def zip(left: A, right: B): Out

    def unzip(out: Out): (Init[Out], Last[Out]) =
        (runtime.Tuples.init(out), runtime.Tuples.last(out)).asInstanceOf[(Init[Out], Last[Out])]

object Zippable extends ZippableLowPriority2:
    given left[A <: Tuple]: (Zippable[A, Unit] { type Out = A }) = (left: A, _) => left

    given right[B <: Tuple]: (Zippable[Unit, B] { type Out = B }) = (_, right: B) => right

    given concat[A <: Tuple, B <: Tuple]: (Zippable[A, B] { type Out = A ++ B }) = (left: A, right: B) => left ++ right

    def apply[A, B](using z: Zippable[A, B]): z.type = z

    def zip[A, B](a: A, b: B)(using z: Zippable[A, B]): z.Out = z.zip(a, b)
end Zippable

trait ZippableLowPriority2 extends ZippableLowPriority1:
    given left0[A]: (Zippable[A, Unit] { type Out = A *: EmptyTuple }) = (left: A, _) => left *: EmptyTuple

    given right0[B]: (Zippable[Unit, B] { type Out = B *: EmptyTuple }) = (_, right: B) => right *: EmptyTuple

trait ZippableLowPriority1 extends ZippableLowPriority0:
    given prepend[A, B <: Tuple]: (Zippable[A, B] { type Out = A *: B }) = (left: A, right: B) => left *: right

    given append[A <: Tuple, B]: (Zippable[A, B] { type Out = A :* B }) = (left: A, right: B) => left :* right

trait ZippableLowPriority0:
    given pair[A, B]: (Zippable[A, B] { type Out = (A, B) }) = (left: A, right: B) => (left, right)
