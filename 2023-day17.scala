package day17

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*

import scala.collection.mutable.PriorityQueue

type World = IndexedSeq[IndexedSeq[Int]]
type Coord = (x: Int, y: Int)
type Node = (x: Int, y: Int, cooldown: Int, dir: Direction, steps: Int)
type Config = (initial: Seq[Int], minSteps: Int)
type State = (node: Node, heat: Int, estimate: Int)
type Min = Int

def parse(input: String): World =
  val (r"$rows...(\n)") = input: @unchecked
  rows.map(_.map { case e if e.isDigit => e.asDigit })

enum Direction:
  case Up, Down, Left, Right

  def move(x: Int, y: Int, steps: Int): Coord = this match
    case Up => (x, y - steps)
    case Down => (x, y + steps)
    case Left => (x - steps, y)
    case Right => (x + steps, y)

  def left: Direction = this match
    case Up => Left
    case Down => Right
    case Left => Down
    case Right => Up

  def right: Direction = this match
    case Up => Right
    case Down => Left
    case Left => Up
    case Right => Down

  def forward: Direction = this

end Direction

def search(world: World, config: Config): Int =
  val width = world(0).size
  val height = world.size
  val dest = (x = width - 1, y = height - 1)
  val initialSteps = config.minSteps `max` 1
  def pathHeat(x: Int, y: Int, xN: Int, yN: Int, initial: Int): Int =
    if x == xN && y == yN then
      initial
    else
      val x1 = x + (xN - x).sign
      val y1 = y + (yN - y).sign
      pathHeat(x1, y1, xN, yN, initial + world(y1)(x1))
  def isGoal(x: Int, y: Int, steps: Int): Boolean =
    x == dest.x && y == dest.y && canTurn(steps)
  def valid(x: Int, y: Int): Boolean =
    x >= 0 && x < width && y >= 0 && y < height
  def distance(x1: Int, y1: Int, x2: Int, y2: Int): Int =
    math.abs(x1 - x2) + math.abs(y1 - y2)

  def next(x: Int, y: Int, cooldowns: Seq[Int], dir: Direction, oldSteps: Int): Seq[Node] =
    val taken = if oldSteps == 0 then initialSteps else 1
    val (x1, y1) = dir.move(x, y, taken)
    if valid(x1, y1) then
      cooldowns.map(cooldown => (x1, y1, cooldown - taken, dir, oldSteps + taken))
    else
      Seq.empty

  def canTurn(steps: Int): Boolean =
    steps >= config.minSteps

  def moveBasic(x: Int, y: Int, cooldown: Int, dir: Direction, steps: Int): Seq[Node] =
    val forward =
      if cooldown > 0 then
        next(x, y, Seq(cooldown), dir.forward, steps)
      else
        Seq.empty
    val turns =
      if canTurn(steps) then
        next(x, y, config.initial, dir.left, 0) ++ next(x, y, config.initial, dir.right, 0)
      else
        Seq.empty
    forward ++ turns

  val minHeat = collection.mutable.HashMap.empty[Node, Int].withDefault(_ => Int.MaxValue)
  val seenMove = collection.mutable.HashSet.empty[Node]

  given Ordering[State] = Ordering.by((s: State) => s.heat + s.estimate).reverse
  def shortest(queue: PriorityQueue[State], min: Min): Int =
    if queue.isEmpty then
      min
    else
      val ((node @ (x, y, cooldown, dir, steps), heat, estimate)) = queue.dequeue
      if isGoal(x, y, steps) then
        assert(heat < min)
        shortest(queue.filter(s => minHeat(s.node) + s.estimate < heat), heat)
      else if !seenMove.contains(node) then
        seenMove.add(node)
        val next: Seq[State] = moveBasic(x, y, cooldown, dir, steps).flatMap: m1 =>
          val (x1, y1) = (m1.x, m1.y)
          val nextHeat = pathHeat(x, y, x1, y1, initial = heat)
          val estimate = distance(x1, y1, dest.x, dest.y)
          if nextHeat + estimate < minHeat(m1) then
            minHeat(m1) = nextHeat + estimate
            Some((m1, nextHeat, estimate))
          else
            None
        shortest(queue.addAll(next), min)
      else
        shortest(queue, min)

  val start: Seq[Node] =
    config.initial.map(cooldown => (0, 0, cooldown, Direction.Right, 0))
  val initial: PriorityQueue[State] = PriorityQueue(start.map(n => (n, 0, distance(n.x, n.y, dest.x, dest.y)))*)
  shortest(initial, Int.MaxValue)


def part1(input: String): Long =
  search(parse(input), config = (initial = Seq(3), minSteps = 0))

def part2(input: String): Long =
  search(parse(input), config = (initial = 4 to 10, minSteps = 4))

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
