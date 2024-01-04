package day25

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*
import scala.collection.immutable.BitSet

def parse(input: String): AList =
  val r"${r"$keys: $conss...( )"}...(\n)" = input: @unchecked
  keys.lazyZip(conss).map((k, cs) => k -> cs)

type AList = Seq[(String, Seq[String])]

def groups(list: AList, acc: Seq[Set[String]]): Seq[Set[String]] =
  list match
  case Seq((k, vs), rest*) =>
    val conn = Set(k) ++ vs
    val (conns, acc1) = acc.partition(_.intersect(conn).nonEmpty)
    val merged = conns.foldLeft(conn)(_ ++ _)
    groups(rest, acc1 :+ merged)
  case _ => acc

case class Edge(v: Id, w: Id):
  def swap: Edge = copy(v = w, w = v)

case class Vertex(id: Id, nodes: Set[String]):

  def ++ (that: Vertex): Vertex = copy(nodes = nodes ++ that.nodes)

  override def hashCode(): Int = id
  override def equals(that: Any): Boolean = that match
    case that: Vertex => id == that.id
    case _ => false

// type Vertex = (nodes: Set[String])
// type Edge = (Vertex, Vertex)
type Cut = Set[Edge]
extension (cut: Cut)
  def weight: Int = cut.size
type Weight = Map[Edge, Long]
type Vertices = BitSet
type Id = Int

def readGraph(alist: AList): (Graph, Weight) =
  var id = 1
  val seen = collection.mutable.Map.empty[String, Int]
  def idOf(v: String) = seen.getOrElseUpdate(v, { id += 1; id })

  def asVertex(v: String) = v match
    case s"$n@$_" => Vertex(id = idOf(n), nodes = Set(n))
    case n => Vertex(id = idOf(n), nodes = Set(n))
  def asWeight(v: String) = v match
    case s"$_@$w" => w.toLong
    case n => 1L
  def asEdges(k: String, v: String) =
    val t = Edge(idOf(k), idOf(v)) //(asVertex(k), asVertex(v))
    List(t, t.swap)
  def asWeights(k: String, v: String) =
    val t = Edge(idOf(k), idOf(v)) //(asVertex(k), asVertex(v))
    val w = asWeight(v)
    List(t -> w, t.swap -> w)

  val vertices = alist.flatMap((k, vs) => k +: vs).map(asVertex).toSet
  val edges = alist.flatMap((k, vs) => vs.flatMap(v => asEdges(k, v))).toSet
  val weights = alist.flatMap((k, vs) => vs.flatMap(v => asWeights(k, v))).toMap
  Graph(vertices, edges) -> weights

case class Graph(V: Set[Vertex], E: Set[Edge]):
  val v = V.map(_.id).to(BitSet)
  val vs = V.map(v => v.id -> v).toMap
  def nodes(id: Id) = vs(id).nodes

  def cutOfThePhase(t: Id): (graph: Graph, cut: Cut, out: Set[String], in: Set[String]) =
    (graph = this, cut = E.filter({case Edge(t1, y) => t1 == t && v(y)}), out = nodes(t), in = (v - t).flatMap(nodes))

  def shrinkGraph(s: Id, t: Id, w: Weight): (Graph, Weight) =
    // println(s"added(n)=t:${t.show}, added(n-1)=s:${s.show}")
    val v1 = v - s - t

    val (mergeable, replaceable) =
      val candidates: Set[Edge] = E.filter({case Edge(st1, y) =>
        y != s && y != t && (st1 == s || st1 == t)
      })
      candidates.groupMap(_.w)(_.v).partition(_(1).size > 1)

    val mergeableEdges: Cut = mergeable.flatMap((y, sts) => sts.toList.map(Edge(_, y))).toSet
    val replaceableEdges: Cut = replaceable.toSeq.flatMap((y, st1s) => st1s.map(Edge(_, y))).toSet
    // println(s"mergeableEdges: ${mergeableEdges.show(using w)}")
    // println(s"replaceableEdges: ${replaceableEdges.show(using w)}")

    val prunedKeys = (mergeableEdges ++ replaceableEdges + (Edge(s, t))).flatMap(e => Set(e, e.swap))

    val prunedW = w -- prunedKeys
    val prunedEdges = E -- prunedKeys

    val st = vs(s) ++ vs(t)
    val (mergedEdges, mergedWeights) = mergeable.toSet.map((y, st1s) =>
      val e1 = Edge(st.id, y)
      // val weights = st1s.map(st1 => w((st1, y)))
      // println(s"new merged edge: ${e1.show}, weights: ${weights}, from: ${st1s.map(_.show)}")
      (e1, e1 -> st1s.toSeq.map(st1 => w(Edge(st1, y))).sum)
    ).unzip

    val (replacedEdges, replacedWeights) = replaceableEdges.map({case Edge(st1, y) =>
      val e = Edge(st1, y)
      val e1 = Edge(st.id, y)
      (e1, e1 -> w(e))
    }).unzip

    val E1 = prunedEdges ++ (mergedEdges ++ replacedEdges).flatMap(e => Set(e, e.swap))
    val w1 = prunedW ++ (mergedWeights ++ replacedWeights).flatMap(e => Set(e, (e(0).swap, e(1))))
    val V1 = v1.view.map(vs).toSet + st
    // println(s"mergedEdges: ${mergedEdges.show(using w1)}")
    // println(s"replacedEdges: ${replacedEdges.show(using w1)}")
    (Graph(V1, E1), w1)

inline def merge[A, B](onto: Map[A, B], m: IterableOnce[(A, B)], inline combine: (B, B) => B): Map[A, B] =
  m.foldLeft(onto): (m, kv) =>
    val (z, w0) = kv
    m.updatedWith(z):
      case None => Some(w0)
      case Some(w1) => Some(combine(w1, w0))

def cutWeight(cut: Cut, w: Weight): Long =
  cut.foldLeft(0L)(_ + w(_))

def connections(z: Id, explore: Vertices, w: Weight, alist: Map[Id, Map[Id, Set[Edge]]]) =
  alist(z).view.filter((y, _) => explore(y)).mapValues(cutWeight(_, w))

def mostTightlyConnectedVertex(explore: Vertices, frontier: Map[Id, Long]): Id =
  explore.maxBy(y => frontier.getOrElse(y, 0L))

def minimumCutPhase(G: Graph, w: Weight, a: Id) =
  val alist = G.E.groupBy(_.v).view.mapValues(_.groupBy(_.w)).toMap
  var A = List(a)
  var explore = G.v - a
  var frontier = connections(a, explore, w, alist).toMap
  while explore.nonEmpty do
    val z = mostTightlyConnectedVertex(explore, frontier)
    A ::= z
    explore -= z
    frontier = merge(frontier, connections(z, explore, w, alist), _ + _)
  val t :: s :: _ = A: @unchecked
  val (g, w1) = G.shrinkGraph(s, t, w)
  (g, w1, G.cutOfThePhase(t))

def minimumCut(G: Graph, w: Weight, a: Id) =
  var g = G
  var w1 = w
  var i = 0
  var minCut = (graph = G, cut = Set.empty[Edge], out = Set.empty[String], in = Set.empty[String])
  var minCutWeight = Long.MaxValue
  var minCutPhase = 0
  while g.V.size > 1 do
    if i % 100 == 0 then
      println(s"phase: $i, g: ${g.V.size} vertices, ${g.E.size} edges")
    i += 1
    val (g1, w2, cutOfThePhase) = minimumCutPhase(g, w1, a)
    // println(s"new cut: ${cutOfThePhase.cut.show(using w1)}")//", weight: ${w2}, g: ${g1}")
    // println(s"weights:")
    // w2.foreach((k, v) => println(s"${k.show}: ${v}"))
    val weight = cutWeight(cutOfThePhase.cut, w1)
    if weight < minCutWeight then
      minCut = cutOfThePhase
      minCutWeight = weight
      minCutPhase = i
    g = g1
    w1 = w2
  (minCut, minCutWeight, minCutPhase)

def part1(input: String): Long =
  val start = System.currentTimeMillis()
  val raw = parse(input)
  // val all = raw.flatMap((k, vs) => Set(k) ++ vs).toSet
  // val grouped = groups(raw, Seq.empty)
  // assert(grouped.head == all)
  // // val elems = grouped.head.toSeq.size
  // val cons = raw.map(_._2.size).sum
  // // in the input there are 3375 connections,
  // // then selecting 3 possible pairs, there are
  // // 3375 choose 3 = 6,401,532,375 possible configurations,
  // // we need a way to divide the problem.

  // println(s"cons: ${cons}")

  val (G @ _, w) = readGraph(raw)
  println(s"graph ${G.V.size} vertices, ${G.E.size} edges")
  // println(s"weights:")
  // w.foreach((k, v) => println(s"${k.show}: ${v}"))
  val (minCut, weight, i) = minimumCut(G, w, G.V.head.id)
  val end = System.currentTimeMillis()
  println(s"minCut: $weight at idx(${i}) of ${minCut.cut.showRaw(using minCut.graph)}")
  println(s"in: ${minCut.in}, out: ${minCut.out}")
  println(s"took ${end - start} ms")
  minCut.in.size * minCut.out.size

def part2(input: String): Long =
  0

extension (v: Vertex) def show: String = v.nodes.mkString("{", ",", "}")
extension (e: Edge) def show(using g: Graph): String = s"${g.vs(e.v).show}->${g.vs(e.w).show}"
extension (c: Cut) def show(using w: Weight, g: Graph): String =
  c.map(e => s"${e.show}:${w(e)}").mkString("[", ",", "]")
extension (c: Set[Vertex]) def show: String =
  c.map(e => s"${e.show}").mkString("[", ",", "]")
extension (c: Cut) def showRaw(using g: Graph): String =
  c.map(e => s"${e.show}").mkString("[", ",", "]")
// extension (c: Weight) def show: String =
//   c.map((k, v) => s"${k.show}:$v").mkString("[", ",", "]")

import challenges.*

@main def part1: Unit =
  // println(s"the answer is ${part1(inputToday(suffix = "stoerwagner"))}")
  // println(s"the answer is ${part1(inputToday(suffix = "sample"))}")
  println(s"the answer is ${part1(inputToday())}")
  // println(s"the answer is ${part1(inputToday(suffix = "sampleB"))}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
