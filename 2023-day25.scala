package day25

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*

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

case class Vertex(id: String, nodes: Set[String]):

  def ++ (that: Vertex): Vertex = copy(nodes = nodes ++ that.nodes)

  override def hashCode(): Int = id.hashCode()
  override def equals(that: Any): Boolean = that match
    case that: Vertex => id == that.id
    case _ => false

// type Vertex = (nodes: Set[String])
type Edge = (Vertex, Vertex)
type Cut = Set[Edge]
extension (cut: Cut)
  def weight: Int = cut.size
type Weight = Map[Edge, Long]

def readGraph(alist: AList): (Graph, Weight) =
  def asVertex(v: String) = v match
    case s"$n@$_" => Vertex(id = n, nodes = Set(n))
    case n => Vertex(id = n, nodes = Set(n))
  def asWeight(v: String) = v match
    case s"$_@$w" => w.toLong
    case n => 1L
  def asEdges(k: String, v: String) =
    val t = (asVertex(k), asVertex(v))
    List(t, t.swap)
  def asWeights(k: String, v: String) =
    val t = (asVertex(k), asVertex(v))
    val w = asWeight(v)
    List(t -> w, t.swap -> w)

  val vertices = alist.flatMap((k, vs) => k +: vs).map(asVertex).toSet
  val edges = alist.flatMap((k, vs) => vs.flatMap(v => asEdges(k, v))).toSet
  val weights = alist.flatMap((k, vs) => vs.flatMap(v => asWeights(k, v))).toMap
  Graph(vertices, edges) -> weights

case class Graph(V: Set[Vertex], E: Set[Edge]):
  def mostTightlyConnectedVertex(A: Set[Vertex], U: Set[Vertex], m: Set[Edge], w: Weight): Vertex =
    // U = V diff A
    val candidateEdges = m.filter((_, y) => U(y)).groupBy((_, y) => y) //E.filter((a, y) => A(a) && U(y)).groupBy((_, y) => y)
    val candidate = U.maxBy(y =>
      // edges from A to y
      val edges = candidateEdges.getOrElse(y, Set.empty)
      // sum of weights
      cutWeight(edges, w)
    )
    // val edges = E.filter((a, z) => z == candidate && A(a))
    // val weight = edges.toSeq.map(w(_)).sum
    // println(s"adding z = ${candidate.show}@$weight, from: ${edges.show(using w)}, A: ${A.show}")
    candidate

  def cutWeight(cut: Cut, w: Weight): Long =
    cut.foldLeft(0L)(_ + w(_))

  def cutOfThePhase(t: Vertex): (cut: Cut, out: Set[String], in: Set[String]) =
    (cut = E.filter((t1, y) => t1 == t && V(y)), out = t.nodes, in = (V - t).flatMap(_.nodes))

  def shrinkGraph(s: Vertex, t: Vertex, w: Weight): (Graph, Weight) =
    // println(s"added(n)=t:${t.show}, added(n-1)=s:${s.show}")
    val V1 = V - s - t

    val (mergeable, replaceable) =
      val candidates: Set[Edge] = E.filter((st1, y) =>
        y != s && y != t && (st1 == s || st1 == t)
      )
      candidates.groupMap(_(1))(_(0)).partition(_(1).size > 1)

    val mergeableEdges: Cut = mergeable.flatMap((y, sts) => sts.toList.map((_, y))).toSet
    val replaceableEdges: Cut = replaceable.toSeq.flatMap((y, st1s) => st1s.map((_, y))).toSet
    // println(s"mergeableEdges: ${mergeableEdges.show(using w)}")
    // println(s"replaceableEdges: ${replaceableEdges.show(using w)}")

    val prunedKeys = (mergeableEdges ++ replaceableEdges + ((s, t))).flatMap(e => Set(e, e.swap))

    val prunedW = w -- prunedKeys
    val prunedEdges = E -- prunedKeys

    val st = s ++ t
    val (mergedEdges, mergedWeights) = mergeable.toSet.map((y, st1s) =>
      val e1 = st -> y
      // val weights = st1s.map(st1 => w((st1, y)))
      // println(s"new merged edge: ${e1.show}, weights: ${weights}, from: ${st1s.map(_.show)}")
      (e1, e1 -> st1s.toSeq.map(st1 => w((st1, y))).sum)
    ).unzip

    val (replacedEdges, replacedWeights) = replaceableEdges.map((st1, y) =>
      val e = st1 -> y
      val e1 = (st, y)
      (e1, e1 -> w(e))
    ).unzip

    val E1 = prunedEdges ++ (mergedEdges ++ replacedEdges).flatMap(e => Set(e, e.swap))
    val w1 = prunedW ++ (mergedWeights ++ replacedWeights).flatMap(e => Set(e, (e(0).swap, e(1))))
    // println(s"mergedEdges: ${mergedEdges.show(using w1)}")
    // println(s"replacedEdges: ${replacedEdges.show(using w1)}")
    (Graph(V1 + st, E1), w1)

def minimumCutPhase(G: Graph, w: Weight, a: Vertex) =
  var A = Set(a)
  var U = G.V - a
  var S = List(a)
  var (m, n) = G.E.partition((a1, _) => a1 == a)
  // var i = 0
  while A != G.V do
    // i += 1
    // if i % 10 == 0 then
    //   println(s"phase: $i")
    // add to A the most tightly connected vertex
    // store the cut-of-the-phase and shrink G by merging the two vertices added last
    val z = G.mostTightlyConnectedVertex(A, U, m, w)
    A += z
    U -= z
    val (m1, n1) = n.partition((z1, _) => z1 == z)
    m ++= m1
    n = n1
    S ::= z
  val t :: s :: _ = S: @unchecked
  val (g, w1) = G.shrinkGraph(s, t, w)
  (g, w1, G.cutOfThePhase(t))

def minimumCut(G: Graph, w: Weight, a: Vertex) =
  var g = G
  var w1 = w
  var i = 0
  var minCut = (cut = Set.empty[Edge], out = Set.empty[String], in = Set.empty[String])
  var minCutWeight = Long.MaxValue
  var minCutPhase = 0
  while g.V.size > 1 do
    i += 1
    // if i % 100 == 0 then
    println(s"phase: $i, g: ${g.V.size} vertices, ${g.E.size} edges")
    val (g1, w2, cutOfThePhase) = minimumCutPhase(g, w1, a)
    // println(s"new cut: ${cutOfThePhase.cut.show(using w1)}")//", weight: ${w2}, g: ${g1}")
    // println(s"weights:")
    // w2.foreach((k, v) => println(s"${k.show}: ${v}"))
    val weight = G.cutWeight(cutOfThePhase.cut, w1)
    if weight < minCutWeight then
      minCut = cutOfThePhase
      minCutWeight = weight
      minCutPhase = i
    g = g1
    w1 = w2
  (minCut, minCutWeight, minCutPhase)

def part1(input: String): Long =
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
  val (minCut, weight, i) = minimumCut(G, w, G.V.head)
  println(s"minCut: $weight at idx(${i}) of ${minCut.cut.showRaw}")
  println(s"in: ${minCut.in}, out: ${minCut.out}")
  minCut.in.size * minCut.out.size

def part2(input: String): Long =
  0

extension (v: Vertex) def show: String = v.nodes.mkString("{", ",", "}")
extension (e: Edge) def show: String = s"${e(0).show}->${e(1).show}"
extension (c: Cut) def show(using w: Weight): String =
  c.map(e => s"${e.show}:${w(e)}").mkString("[", ",", "]")
extension (c: Set[Vertex]) def show: String =
  c.map(e => s"${e.show}").mkString("[", ",", "]")
extension (c: Cut) def showRaw: String =
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
