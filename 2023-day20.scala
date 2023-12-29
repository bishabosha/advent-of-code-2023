package day20

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec

enum ModuleKind:
  case Plain, FlipFlop, Conjunction

enum ModuleState:
  case Empty
  case FlipFlop(isOff: Boolean)
  case Conjunction(hiSignals: Set[String], slots: Int)

type ModuleDef = (kind: ModuleKind, outs: Seq[String])

type Mods = Map[String, ModuleDef]
def mods(using Mods): Mods = summon[Mods]

def parse(input: String): Mods =
  val (r"${r"$mods -> $destss...(, )"}...(\n)") = input: @unchecked
  val mods0 = mods.map:
    case s"%$id" => (id = id, kind = ModuleKind.FlipFlop)
    case s"&$id" => (id = id, kind = ModuleKind.Conjunction)
    case id      => (id = id, kind = ModuleKind.Plain)
  mods0.lazyZip(destss).map((mod, dests) =>
    (mod.id -> (mod.kind, dests))
  ).toMap

import ModuleState.*

type State = Map[String, ModuleState]
type Pulse = (from: String, to: String, isLo: Boolean)

def inputs(using Mods): Map[String, Seq[String]] =
  mods.foldLeft(Map.empty): (acc, kv) =>
    val (k, mdef) = kv
    mdef.outs.foldLeft(acc): (acc, out) =>
      acc.updatedWith(out):
        case None => Some(Vector(k))
        case Some(ins) => Some(ins :+ k)

def initialState(inputs: Map[String, Seq[String]])(using Mods): State =
  mods.map: (id, mdef) =>
    id -> mdef.kind.match
      case ModuleKind.Plain => Empty
      case ModuleKind.FlipFlop => FlipFlop(isOff = true)
      case ModuleKind.Conjunction => Conjunction(hiSignals = Set.empty, slots = inputs(id).size)

def updateState(old: ModuleState, from: String, self: String, isLo: Boolean): ModuleState =
  old match
  case Empty => Empty
  case FlipFlop(isOff) => if isLo then FlipFlop(isOff = !isOff) else old
  case Conjunction(hiSignals, inputs) =>
    if isLo then Conjunction(hiSignals = hiSignals - from, inputs)
    else Conjunction(hiSignals = hiSignals + from, inputs)

def pushButton(state: State, breakOut: (Boolean, String) => Boolean)(using Mods): Option[(lo: Int, hi: Int, state: State)] =
  @tailrec
  def nextPulse(queue: Queue[Pulse], lo: Int, hi: Int, state: State): Option[(lo: Int, hi: Int, state: State)] =
    queue.dequeueOption match
      case None => Some((lo, hi, state))
      case Some((pulse, next)) =>
        val (from, to, isLo) = pulse
        val state1 = state.updatedWith(to):
          case Some(old) => Some(updateState(old, from, to, isLo))
          case None => Some(Empty) // e.g. "output", which has no module definition
        inline def append(isLo: Boolean) =
          if breakOut(isLo, to) then None
          else
            val outs = mods.get(to).map(_.outs).getOrElse(Seq.empty)
            val nextPulses = outs.map(out => (from = to, to = out, isLo = isLo))
            val next1 = next.appendedAll(nextPulses)
            if isLo then nextPulse(next1, lo + nextPulses.size, hi, state1)
            else nextPulse(next1, lo, hi + nextPulses.size, state1)
        state1(to) match
          case Empty => append(isLo)
          case FlipFlop(isOff) =>
            if isLo then append(isLo = isOff)
            else nextPulse(next, lo, hi, state1) // ignore
          case Conjunction(hiSignals, slots) =>
            append(isLo = hiSignals.size == slots)
  end nextPulse

  val initialPulses = mods("broadcaster").outs.map(out =>
    (from = "broadcaster", to = out, isLo = true)
  ).to(Queue)
  nextPulse(initialPulses, lo = 1 + mods("broadcaster").outs.size, hi = 0, state = state)
end pushButton

def part1(input: String): Long =
  given Mods = parse(input)

  val initial = (
    lo = 0L,
    hi = 0L,
    state = initialState(inputs),
    pushes = 0,
  )

  val states = Iterator.iterate(initial): s =>
    val (lo, hi, state) = pushButton(s.state, (_, _) => false).get
    (
      lo = lo + s.lo,
      hi = hi + s.hi,
      state = state,
      pushes = s.pushes + 1,
    )

  val (lo = lo, hi = hi) = states.dropWhile(_.pushes < 1000).next()
  lo * hi

def part2(input: String): Long =
  given Mods = parse(input)

  val ins = inputs

  def lcm(a: Long, b: Long): Long = b * a / gcd(a, b)
  def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)

  def firstHigh(mod: String): Long =
    val initial =
      (pushes = 0, state = initialState(ins), found = false)
    val states = Iterator.iterate(initial): s =>
      pushButton(s.state, (isLo, from) => !isLo && from == mod) match
        case None => (pushes = s.pushes + 1, state = s.state, found = true)
        case Some(outs) => (pushes = s.pushes + 1, state = outs.state, found = false)
    states.dropWhile(!_.found).next().pushes

  def minTurns(mod: String): Long =
    mods.get(mod) match
      case None => // terminal, aka forwarder, aka same as parent
        ins(mod).map(minTurns(_)).min
      case Some((kind = ModuleKind.Conjunction)) =>
        ins(mod).map(firstHigh(_)).reduce(lcm)
      case _ => sys.error(s"unexpected module $mod")
  minTurns("rx")

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
