package u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.*
import Stream.*
import u03.Sequences.*
import Sequence.*
import u02.Modules.Person
import u02.Modules.Person.{Student, Teacher}


class TestLab03:
  //task 2 test

  @Test def testCoursesOfTeachers() =
    val persons: Sequence[Person] =
      Cons(Teacher("Alice", "Mathematics"),
        Cons(Student("Bob", 2015),
          Cons(Teacher("Carlo", "History"),
            Nil())))
    val courses: Sequence[String] = coursesOfTeachers(persons)
    val expected: Sequence[String] = Cons("Mathematics", Cons("History", Nil()))
    assertEquals(expected, courses)

  @Test def testFoldLeft() =
    val lst: Sequence[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(16, foldLeft(lst)(0)(_ + _))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test def testTotalTaughtCourses() =
    val persons: Sequence[Person] =
      Cons(Teacher("Alice", "Mathematics"),
        Cons(Student("Bob", 2015),
          Cons(Teacher("Carlo", "History"),
            Nil())))
    val courses: Int = totalTaughtCourses(persons)
    val expected: Int = 2
    assertEquals(expected, courses)

  // task 3 tests

  @Test def takeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFill(): Unit =
    val str2 = Stream.fill(5)(4) // {4,4,4,4,4}
    assertEquals(Cons(4, Cons(4, Cons(4, Cons(4, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFibonacci(): Unit =
    val str2 = Stream.toList(Stream.take(fibonacci)(5)) // {4,4,4,4,4}
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), str2)

  @Test def interleaveTest(): Unit =
    val s1: Stream[Int] = Stream.cons(1, Stream.cons(3, Stream.cons(5, Stream.empty())))
    val s2 = Stream.cons(2, Stream.cons(4, Stream.cons(6, Stream.cons(8, Stream.cons(10, Stream.empty())))))
    val str = Stream.toList(Stream.interleave(s1, s2))
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(8, Cons(10, Nil())))))))), str)

  @Test def testCycle(): Unit =
    val repeat = cycle(Cons('a', Cons('b', Cons('c', Nil()))))
    Stream.toList(Stream.take(repeat)(5))