package day13

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*

type Mirror = Either[Int, Int]

def parse(input: String): IndexedSeq[IndexedSeq[String]] =
  val r"${r"$imagess...(\n)"}...(\n\n)" = input: @unchecked
  imagess

def findMirrorRow[T](image: IndexedSeq[T], old: Option[Int]): Option[Int] =
  def scan(i: Int, j: Int): Boolean =
    if i < 0 || j >= image.length then true
    else if image(i) == image(j) then scan(i - 1, j + 1)
    else false
  image.lazyZip(image.tail).lazyZip(image.indices)
    .collectFirst:
      case (a, b, i)
        if old.forall(_ != i)
        && a == b
        && scan(i - 1, i + 2)
      => i

def findMirror(image: IndexedSeq[String], old: Option[Mirror]): Option[Mirror] =
  def oldRow = old.collect({ case Left(i) => i })
  def oldCol = old.collect({ case Right(i) => i })
  findMirrorRow(image, oldRow).map(Left(_))
    .orElse(findMirrorRow(image.transpose, oldCol).map(Right(_)))

extension (mirror: Mirror) def summarise: Int =
  mirror.fold(i => (i + 1) * 100, _ + 1)

def summariseBasic(image: IndexedSeq[String]): Int =
  findMirror(image, old = None).get.summarise

def summariseSmudge(image: IndexedSeq[String]): Int =
  val old = findMirror(image, old = None)
  val candidates =
    for y <- image.indices.iterator
        x <- image(y).indices.iterator
        i <- locally:
          val flipped = if image(y)(x) == '.' then '#' else '.'
          val image0 = image.updated(y, image(y).updated(x, flipped))
          findMirror(image0, old)
    yield i
  candidates.next().summarise

def solution(input: String, summary: IndexedSeq[String] => Int): Int =
  parse(input).map(summary).sum

def part1(input: String): Int =
  solution(input, summariseBasic)

def part2(input: String): Int =
  solution(input, summariseSmudge)


import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
