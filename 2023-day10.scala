package day10

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*
import Direction.*

type Coord = (x: Int, y: Int)
type Move = (exitDir: Direction, coord: Coord)
type Segment = (pipe: Char, move: Move)

enum Direction:
  case Up, Down, Left, Right

  def transition(in: Coord): Coord =
    val (x, y) = in
    this match
      case Up => (x, y - 1)
      case Down => (x, y + 1)
      case Left => (x - 1, y)
      case Right => (x + 1, y)

def parseRows(input: String): IndexedSeq[String] =
  val r"$rows...(\n)" = input: @unchecked
  rows

def scanStart(rows: IndexedSeq[String]): Coord =
  rows.iterator.zipWithIndex.collectFirst({
    case (row, y) if row.contains('S') =>
      val x = row.indexOf('S')
      (x, y)
  }).get

def validMove(exitDir: Direction, pipe: Char): Boolean = (exitDir, pipe) match
  case (_, 'S') => true
  case (Down, '|' | 'L' | 'J') => true
  case (Up, '|' | '7' | 'F') => true
  case (Right, '-' | '7' | 'J') => true
  case (Left, '-' | 'F' | 'L') => true
  case _ => false

def charAt(coord: Coord, rows: IndexedSeq[String]): Char =
  val (x, y) = coord
  if y < 0 || y >= rows.length || x < 0 || x >= rows(y).length then
    '.'
  else
    rows(y)(x)

def travel(last: Segment, rows: IndexedSeq[String]): Move =
  // precondition: validMove(last.move)
  val (pipe, (exitDir, coord)) = last
  val nextDir = ((pipe, exitDir): @unchecked) match
    case '|' -> Up => Up
    case '|' -> Down => Down
    case '-' -> Left => Left
    case '-' -> Right => Right
    case 'L' -> Down => Right
    case 'L' -> Left => Up
    case 'J' -> Down => Left
    case 'J' -> Right => Up
    case '7' -> Right => Down
    case '7' -> Up => Left
    case 'F' -> Up => Right
    case 'F' -> Left => Down
  (exitDir = nextDir, coord = nextDir.transition(coord))

def scanConnections(last: Segment, path: Seq[Segment], rows: IndexedSeq[String]): Option[Seq[Segment]] =
  // precondition: validMove(last.move)
  val (scan @ (exitDir, coord)) = travel(last, rows)
  val maybePipe = charAt(coord, rows)
  if validMove(exitDir, maybePipe) then
    if maybePipe == 'S' then
      // infer the pipe for S
      val dir0 = path.head.move.exitDir
      val pipeS = (exitDir, dir0) match
        case (Up, Up) => '|'
        case (Down, Down) => '|'
        case (Left, Left) => '-'
        case (Right, Right) => '-'
        case (Down, Right) => 'L'
        case (Left, Up) => 'L'
        case (Down, Left) => 'J'
        case (Right, Up) => 'J'
        case (Right, Down) => '7'
        case (Up, Left) => '7'
        case (Up, Right) => 'F'
        case (Left, Down) => 'F'
        case _ => throw new Exception(s"invalid pipe: $exitDir => $dir0")
      val next = (pipeS, scan)
      Some(path :+ next)
    else
      val next = (maybePipe, scan)
      scanConnections(next, path :+ next, rows)
  else
    None

def findLoop(input: String, rows: IndexedSeq[String]): Seq[Segment] =
  val start = scanStart(rows)
  val paths = Direction.values.flatMap: exitDir =>
    val coord = exitDir.transition(start)
    val char = charAt(coord, rows)
    if validMove(exitDir, char) then
      val next = (pipe = char, move = (exitDir = exitDir, coord = coord))
      scanConnections(next, Vector(next), rows)
    else
      None
  paths.minBy(_.length)

def part1(input: String): Int =
  val rows = parseRows(input)
  findLoop(input, rows).length / 2

enum State:
  case In, Out
  case Cross(start: Char, wasIn: In.type | Out.type)

  def flip: State = this match
    case In => Out
    case Out => In
    case _ => this

def part2(input: String): Int =
  val rows = parseRows(input)
  val loop = findLoop(input, rows)
  val loopGrid = loop
    .groupBy(_.move.coord.y).view
    .mapValues: es =>
      es.filterNot(_.pipe == '-')
        .map(e => e.move.coord.x -> e.pipe)
        .toMap
    .toMap
  def boundary(x: Int, y: Int) =
    loopGrid.get(y).flatMap(_.get(x))
  def cross(pipe: Char, x: Int, area: Int, begin: Int, state: State) =
    val next = pipe match
      case '|' => state.flip
      case _ => state match
        case State.In => State.Cross(start = pipe, State.In)
        case State.Out => State.Cross(start = pipe, State.Out)
        case State.Cross(before, old) => (before, pipe) match
          case ('L', 'J') => old // U-turn
          case ('F', '7') => old // U-turn
          case ('L', '7') => old.flip // S-bend
          case ('F', 'J') => old.flip // S-bend
    end next
    (
      area = if state == State.In then area + (x - begin) else area,
      begin = if next == State.In then x + 1 else -1,
      state = next
    )
  end cross
  loopGrid.keysIterator
    .map: y =>
      val initial = (area = 0, begin = -1, state = State.Out)
      val (area, _, _) = rows(y).indices.foldLeft(initial): (acc, x) =>
        val (area, begin, state) = acc
        boundary(x, y).map(cross(_, x, area, begin, state)).getOrElse(acc)
      area
    .sum


import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
