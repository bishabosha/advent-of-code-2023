package day15

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*

// import collection.mutable.ArrayDeque
import collection.immutable.ListMap
import compiletime.ops.int.S
import scala.reflect.ClassTag

type Box = ListMap[String, Int]

object Linear:
  def fill[A: ClassTag](n: Int)(elem: => A): Arr[A] =
    Array.fill(n)(elem)

  def foldOver[T, A](xs: IterableOnce[T], init: Arr[A])(op: (xs: Arr[A], t: T) => Consumed[xs.type]): Arr[A] =
    xs.foldLeft(init)(op.asInstanceOf)

  opaque type Consumed[A <: Singleton] <: A = A
  opaque type Arr[A] = Array[A]
  extension [A](xs: Arr[A])
    // TODO: add a capability parameter that witnesses the consumption, and add a
    // macro that checks that `xs` is linearly used, while providing the capability
    inline def updated(idx: Int, inline op: A => A): Consumed[xs.type] =
      xs(idx) = op(xs(idx))
      xs
    def result(using ClassTag[A]): IArray[A] = IArray.from(xs)

def parse(input: String): IndexedSeq[String] =
  val r"$segments...(,)\n" = input: @unchecked
  segments

def hash(str: String): Int =
  str.foldLeft(0)((acc, c) => (acc + c.toInt) * 17 % 256)

def part1(input: String): Long =
  parse(input).map(hash).sum

def move(acc: Linear.Arr[Box], str: String): Linear.Consumed[acc.type] =
  val r"${Seq(label, op*)}...([=-])" = str: @unchecked
  val idx = hash(label)
  op match
    case Nil => acc.updated(idx, _.removed(label))
    case Seq(focal) => acc.updated(idx, _.updated(label, focal(0).asDigit))

def power(box: Box, idx: Int): Long =
  box.zipWithIndex.map({case ((label, focal), slot) => (1 + idx) * (slot + 1) * focal}).sum

def part2(input: String): Long =
  val box = ListMap.empty[String, Int]
  val boxes = Linear.foldOver(parse(input), Linear.fill(256)(box))(move)
  boxes.result.zipWithIndex.map(power).sum


import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
