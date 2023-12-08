package day08

import scala.language.experimental.namedTuples
import regexglob.RegexGlobbing.*

type Network = (path: LazyList[Char], nodes: Map[String, (String, String)])

def parse(input: String): Network =
  val r"$lr\n\n${r"$keys = ($ls, $rs)"}...(\n)" = input: @unchecked
  val nodes = keys.lazyZip(ls).lazyZip(rs).map((k, l, r) =>
    k -> (l, r)).toMap
  (path = LazyList.continually(lr).flatten, nodes = nodes)

def part1(input: String): Long =
  val network = parse(input)
  val initial = (key = "AAA", next = network.path, steps = 0L)
  val stepss = LazyList.unfold(initial): state =>
    if state.key == "ZZZ" then None
    else
      val dir = state.next.head
      val (l, r) = network.nodes(state.key)
      val key0 = if dir == 'L' then l else r
      val state0 = (key = key0, next = state.next.tail, steps = state.steps + 1L)
      Some(state.steps + 1L -> state0)
  stepss.last

def part2(input: String): BigInt =
  val network = parse(input)
  val initialKeys = network.nodes.keys.filter(_.endsWith("A")).toList
  val initialSteps = initialKeys.map(_ -> Left(0L).withRight[Long]).toMap
  val initial = (next = network.path, steps = initialSteps)
  val stepss = LazyList.unfold(initial): state =>
    if state.steps.values.forall(_.isRight) then None
    else
      val dir = state.next.head
      val keys0 = state.steps.map: (key, value) =>
        value match
          case Left(steps) =>
            val steps0 = steps + 1L
            val (l, r) = network.nodes(key)
            val key0 = if dir == 'L' then l else r
            if key0.endsWith("Z") then key0 -> Right(steps0)
            else key0 -> Left(steps0)
          case _ => key -> value
      val state0 = (next = state.next.tail, steps = keys0)
      Some(keys0 -> state0)
  val all = stepss.last.values.collect { case Right(steps) => steps }
  def lcm(ns: Iterable[Long]) =
    ns.foldLeft(BigInt(1)): (acc, n) =>
      val g = acc.gcd(n)
      acc * n / g
  lcm(all)

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
