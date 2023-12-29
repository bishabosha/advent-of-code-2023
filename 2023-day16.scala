package day16

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*

type Mirror = '/' | '\\' | '-' | '|'
type Elem = '.' | Mirror
type World = IndexedSeq[IndexedSeq[Elem]]

type Coord = (char: Elem, x: Int, y: Int)
type Node = (char: Mirror, x: Int, y: Int)
type Nodes = (rows: Map[Int, Seq[Node]], cols: Map[Int, Seq[Node]])

def parse(input: String): World =
  val (r"$rows...(\n)") = input: @unchecked
  rows.map(_.map[Elem] { case e: Elem => e })

def scan(w: World): Nodes =
  val elements =
    for
      (row, y) <- w.zipWithIndex
      case (mirror: Mirror, x) <- row.zipWithIndex
    yield
      (mirror, x, y): Node
  val byRow = elements.groupBy(_.y).view.mapValues(_.sortBy(_.x)).toMap.withDefault(_ => Seq.empty)
  val byCol = elements.groupBy(_.x).view.mapValues(_.sortBy(_.y)).toMap.withDefault(_ => Seq.empty)
  (byRow, byCol)

def part1(input: String): Long =
  val world = parse(input)
  val nodes = scan(world)
  covered(world, nodes, 0, 0, Right).size

enum Direction:
  case Up, Down, Left, Right

import Direction.*

val upTo = collection.mutable.HashMap.empty[(x: Int, y: Int, t1: Int, dir: Direction), Set[Coord]]


def covered(w: World, ns: Nodes, x: Int, y: Int, dir: Direction): Set[Coord] =
  val width = w(0).size
  val height = w.size
  val seen = collection.mutable.HashSet.empty[(Node, Direction)]
  def to(x: Int, y: Int, t1: Int, dir: Direction) =
    upTo.getOrElseUpdate((x, y, t1, dir), {
      dir match
        case Up => (y to t1 by -1).map[Coord](y => (w(y)(x), x, y)).toSet
        case Down => (y to t1).map[Coord](y => (w(y)(x), x, y)).toSet
        case Left => (x to t1 by -1).map[Coord](x => (w(y)(x), x, y)).toSet
        case Right => (x to t1).map[Coord](x => (w(y)(x), x, y)).toSet
    })

  def loop(x: Int, y: Int, dir: Direction): Set[Coord] =
    if x < 0 || x >= width || y < 0 || y >= height then Set.empty
    else dir match
      case Up =>
        val elems = ns.cols(x).view.reverse.dropWhile(_.y > y).toSeq
        elems match
          case m +: _ => // inspect what to do
            val base = to(x, y, m.y, dir)
            val extra =
              if seen.contains((m, dir)) then Set.empty
              else
                seen += ((m, dir))
                m.char match
                  case '/' => loop(x + 1, m.y, Right)
                  case '\\' => loop(x - 1, m.y, Left)
                  case '-' => loop(x - 1, m.y, Left) ++ loop(x + 1, m.y, Right)
                  case '|' => loop(x, m.y - 1, Up)
            base ++ extra
          case _ =>  // continues from x to end or row
            to(x, y, 0, dir)
      case Down =>
        val elems = ns.cols(x).dropWhile(_.y < y)
        elems match
          case m +: _ => // inspect what to do
            val base = to(x, y, m.y, dir)
            val extra =
              if seen.contains((m, dir)) then Set.empty
              else
                seen += ((m, dir))
                m.char match
                  case '/' => loop(x - 1, m.y, Left)
                  case '\\' => loop(x + 1, m.y, Right)
                  case '-' => loop(x - 1, m.y, Left) ++ loop(x + 1, m.y, Right)
                  case '|' => loop(x, m.y + 1, Down)
            base ++ extra
          case _ =>  // continues from x to end or row
            to(x, y, height - 1, dir)
      case Left =>
        val elems = ns.rows(y).view.reverse.dropWhile(_.x > x).toSeq
        elems match
          case m +: _ => // inspect what to do
            val base = to(x, y, m.x, dir)
            val extra =
              if seen.contains((m, dir)) then Set.empty
              else
                seen += ((m, dir))
                m.char match
                  case '/' => loop(m.x, y + 1, Down)
                  case '\\' => loop(m.x, y - 1, Up)
                  case '-' => loop(m.x - 1, y, Left)
                  case '|' => loop(m.x, y - 1, Up) ++ loop(m.x, m.y + 1, Down)
            base ++ extra
          case _ =>  // continues from x to end or row
            to(x, y, 0, dir)
      case Right =>
        val elems = ns.rows(y).dropWhile(_.x < x)
        elems match
          case m +: _ => // inspect what to do
            val base = to(x, y, m.x, dir)
            val extra =
              if seen.contains((m, dir)) then Set.empty
              else
                seen += ((m, dir))
                m.char match
                  case '/' => loop(m.x, y - 1, Up)
                  case '\\' => loop(m.x, y + 1, Down)
                  case '-' => loop(m.x + 1, y, Right)
                  case '|' => loop(m.x, y - 1, Up) ++ loop(m.x, m.y + 1, Down)
            base ++ extra
          case _ =>  // continues from x to end or row
            to(x, y, width - 1, dir)
  end loop
  loop(x, y, dir)

def part2(input: String): Long =
  val world = parse(input)
  val nodes = scan(world)
  val width = world(0).size
  val height = world.size

  val top = (0 until width).map(x => (x, 0, Down))
  val right = (0 until height).map(y => (width - 1, y, Left))
  val bottom = (0 until width).map(x => (x, height - 1, Up))
  val left = (0 until height).map(y => (0, y, Right))

  (top ++ right ++ bottom ++ left).map((x, y, dir) =>
    covered(world, nodes, x, y, dir).size
  ).max

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
