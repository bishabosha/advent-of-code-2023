package day19

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*
import scala.collection.immutable.Queue

type Part = IArray[Int]
type Part2 = IArray[Range]

enum Dest:
  case Named(out: String)
  case Accept, Reject

def parse1(input: String): (flows: Map[String, Part => Dest], parts: Seq[Part]) =
  val r"$flows...(\n)\n\n$parts...(\n)" = input: @unchecked
  val r"$names{$ruless...(,)}" = flows: @unchecked
  def parseDest(out: String): Dest = out match
    case "A" => Dest.Accept
    case "R" => Dest.Reject
    case dest => Dest.Named(dest)
  def make(f: String, cmp: Int, op: Char, dest: String): (Part => Boolean, Dest) =
    val idx = f match
      case "x" => 0
      case "m" => 1
      case "a" => 2
      case "s" => 3
    val op0: Part => Boolean = op match
      case '<' => _(idx) < cmp
      case '>' => _(idx) > cmp
    (op0, parseDest(dest))
  val ruless0 = ruless.map(rules =>
    val ops = rules.map:
      case r"$f<$cmp%d:$dest" => make(f, cmp, '<', dest)
      case r"$f>$cmp%d:$dest" => make(f, cmp, '>', dest)
      case dest => parseDest(dest)
    def find(p: Part): Dest =
      val it = ops.iterator
      while it.hasNext do it.next() match
        case dest: Dest => return dest
        case (op, out) => if op(p) then return out
      ???
    find
  )
  val flows0 = names.lazyZip(ruless0).toMap
  val r"{x=$xs%d,m=$ms%d,a=$as%d,s=$ss%d}" = parts: @unchecked
  val parts0 = xs.lazyZip(ms).lazyZip(as).lazyZip(ss).map(IArray(_,_,_,_))
  (flows0, parts0)

def part1(input: String): Int =
  val (flows, parts) = parse1(input)
  val in = flows("in")
  def accepted(p: Part, flow: Part => Dest): Boolean =
    flow(p) match
    case Dest.Accept => true
    case Dest.Reject => false
    case Dest.Named(out) => accepted(p, flows(out))
  parts.filter(accepted(_, in)).map(_.sum).sum

def parse2(input: String): Map[String, Part2 => Seq[(captured: Part2, dest: Dest)]] =
  val r"$flows...(\n)\n\n$_" = input: @unchecked
  val r"$names{$ruless...(,)}" = flows: @unchecked
  def parseDest(out: String): Dest = out match
    case "A" => Dest.Accept
    case "R" => Dest.Reject
    case dest => Dest.Named(dest)
  def overlaps(r: Range, s: Range): Boolean =
    r.start <= s.end && s.start <= r.end
  def divide(r: Range, s: Range): (intersection: Range, remaining: Range) =
    val start = r.head max s.head
    val end = r.last min s.last
    val intersection = start to end
    val remaining = if r.head < start then r.head until start else (end + 1) to r.last
    (intersection, remaining)
  def make(f: String, cmp: Int, op: Char, dest: String): (con: Part2 => Option[(Part2, Part2)], dest: Dest) =
    val idx = f match
      case "x" => 0
      case "m" => 1
      case "a" => 2
      case "s" => 3
    val valid: Range = op match
      case '<' => 1 until cmp
      case '>' => (cmp + 1) to 4000
    def test(p: Part2) =
      val di = p(idx)
      Option.when(overlaps(di, valid)):
        val (intersection, remaining) = divide(di, valid)
        val p0 = p.updated(idx, intersection)
        val p1 = p.updated(idx, remaining)
        (p0, p1)
    (test, parseDest(dest))
  val ruless0 = ruless.map(rules =>
    val constraints = rules.map:
      case r"$f<$cmp%d:$dest" => make(f, cmp, '<', dest)
      case r"$f>$cmp%d:$dest" => make(f, cmp, '>', dest)
      case dest => parseDest(dest)
    def find(p: Part2): Seq[(captured: Part2, dest: Dest)] =
      val it = constraints.iterator
      val buf = Seq.newBuilder[(captured: Part2, dest: Dest)]
      var curr = p
      while it.hasNext do it.next() match
        case dest: Dest => buf += ((curr, dest))
        case (op, out) =>
          op.value(curr) match
            case Some((intersection, remaining)) =>
              curr = remaining
              buf += ((intersection, out))
            case None => () // fallthrough
      buf.result()
    find
  )
  names.lazyZip(ruless0).toMap

def part2(input: String): Long =
  val flows = parse2(input)
  val allParts: Part2 = IArray.tabulate(4)(i => 1 to 4000)
  type Capture = (captured: Part2, dest: Dest)
  type Work = (captured: Part2, flow: String)
  def combinations(p: Part2): Long =
    p.map(_.size.toLong).product

  def accepted(ps: Queue[Work], acc: Long): Long =
    ps.dequeueOption match
    case None => acc
    case Some((head, rest)) =>
      val (p, flow) = head
      def prepare(explore: Seq[Capture], acc: Long, prep: Seq[Work]): (Long, Seq[Work]) =
        explore match
        case Seq() => (acc, prep)
        case Seq((p0, Dest.Accept), rest*) =>
          prepare(rest, acc + combinations(p0), prep)
        case Seq((p0, Dest.Reject), rest*) =>
          prepare(rest, acc, prep)
        case Seq((p0, Dest.Named(out)), rest*) =>
          prepare(rest, acc, prep :+ ((p0, out)))
      val (acc0, prep) = prepare(flows(flow)(p), acc, Vector.empty)
      accepted(rest.appendedAll(prep), acc0)
  accepted(Queue((allParts, "in")), 0L)

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
