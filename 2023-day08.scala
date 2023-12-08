package day08

import scala.language.experimental.namedTuples
import regexglob.RegexGlobbing.*

type Node = (left: String, right: String)
type Network = (path: LazyList[Char], nodes: Map[String, Node])
type Filter = String => Boolean

def parse(input: String): Network =
  val r"$lr\n\n${r"$keys = ($ls, $rs)"}...(\n)" = input: @unchecked
  val nodes = keys.lazyZip(ls.zip(rs)).map((k, v) => k -> v).toMap
  (path = LazyList.continually(lr).flatten, nodes = nodes)

def search(starts: Seq[String], isEnd: Filter, network: Network): Seq[Int] =
  val initial = (network.path, starts.map(_ -> 0).toMap)
  val states = LazyList.unfold(initial): (path, state) =>
    if state.keys.forall(isEnd) then None
    else
      val next = state.map: (key, steps) =>
        if isEnd(key) then key -> steps
        else
          val node = network.nodes(key)
          val key0 = if path.head == 'L' then node.left else node.right
          key0 -> (steps + 1)
      Some(next -> (path.tail, next))
  states.last.values.toSeq

def lcm(ns: Iterable[Int]): BigInt =
  ns.foldLeft(BigInt(1)): (acc, n) =>
    val g = acc.gcd(n)
    acc * n / g

def part1(input: String): Int =
  val network = parse(input)
  search(Seq("AAA"), _ == "ZZZ", network).last

def part2(input: String): BigInt =
  val network = parse(input)
  val starts = network.nodes.keys.filter(_.endsWith("A")).toList
  lcm(search(starts, _.endsWith("Z"), network))

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
