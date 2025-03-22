package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(h, t) if n > 0 => skip(t)(n-1)
      case _ => s

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2),zip(t1,t2))
      case _ => Nil()

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Nil(), _)  => s2
      case (_, Nil()) => s1
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1,concat(t1,s2))


    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] = {
      @tailrec
      def reverseAcc(s: Sequence[A], acc: Sequence[A]): Sequence[A] = (s, acc) match
        case (Nil(),_) => acc
        case (Cons(h1, Nil()), acc) => Cons(h1, acc)
        case (Cons(h1, t1), Nil()) => reverseAcc(t1, Cons(h1, Nil()))
        case (Cons(h1, t1), acc) => reverseAcc(t1, Cons(h1, acc))
      reverseAcc(s, Nil())
    }

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Nil() => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))


    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] = {
      @tailrec
      def minSaver(s: Sequence[Int], min: Optional[Int]): Optional[Int] = (s, min) match
        case (Cons(h, t), Empty()) => minSaver(t,Just(h))
        case (Nil(), min) => min
        case (Cons(h, t), min) if !isEmpty(min) && h < orElse(min, 0) => minSaver(t, Just(h))
        case (Cons(h, t), min) => minSaver(t, min)

      minSaver(s, Empty())
    }


    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = {
      @tailrec
      def evenIndicesAcc[A](s: Sequence[A], out: Sequence[A], even: Boolean): Sequence[A] = (s, out, even) match
        case (Cons(h,t), out, even) if even => evenIndicesAcc(t, Cons(h, out), false)
        case (Cons(h,t), out, even) if !even => evenIndicesAcc(t, out, true)
        case _ => reverse(out)

      evenIndicesAcc(s, Nil(), true)
    }


    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, t) if h == elem => true
      case Cons(h, t) if h != elem => contains(t)(elem)
      case _ => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] = {
      @tailrec
      def delete(s: Sequence[A], unique: Sequence[A]): Sequence[A] = (s, unique) match
        case (Cons(h,t), unique) if contains(unique)(h) => delete(t, unique)
        case (Cons(h,t), unique) if !contains(unique)(h) => delete(t, Cons(h, unique))
        case _ => reverse(unique)
      delete(s, Nil())
    }

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = {
      @tailrec
      def groupTail(s: Sequence[A], out: Sequence[Sequence[A]]): Sequence[Sequence[A]] = (s, out) match
        case (Nil(), out) => reverse(out)
        case (Cons(h1, Cons(h11, t1)), Cons(Cons(h2, t2), t3)) if h1 == h2 => groupTail(Cons(h11, t1), Cons(Cons(h1, Cons(h2,t2)),t3))
        case (Cons(h1, Cons(h11, t1)), Cons(Cons(h2, t2), t3)) if h1 != h2 => groupTail(Cons(h11, t1), Cons(Cons(h1, Nil()), Cons(Cons(h2, t2), t3)))
        case (Cons(h, t), out) => groupTail(t, Cons(Cons(h,Nil()), out))
      groupTail(s, Nil())
    }

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = {
      @tailrec
      def partitionTail(s: Sequence[A], satisfyPredicate: Sequence[A], unsatisfiedPredicate: Sequence[A]): (Sequence[A], Sequence[A]) = (s, satisfyPredicate, unsatisfiedPredicate) match
        case (Nil(), satisfyPredicate, unsatisfiedPredicate ) => (reverse(satisfyPredicate),reverse(unsatisfiedPredicate))
        case (Cons(h,t), satisfyPredicate, unsatisfiedPredicate) if pred(h) => partitionTail(t, Cons(h,satisfyPredicate), unsatisfiedPredicate)
        case (Cons(h,t), satisfyPredicate, unsatisfiedPredicate) if !pred(h) => partitionTail(t, satisfyPredicate, Cons(h,unsatisfiedPredicate))
      partitionTail(s, Nil(), Nil())
    }

  end Sequence
end Sequences

@main def trySequences =
  import Sequences.*
  val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(sequence)) // 30

  import Sequence.*

  println(sum(map(filter(sequence)(_ >= 20))(_ + 1))) // 21+31 = 52
