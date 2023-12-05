package day03

import scala.language.experimental.namedTuples

type Num = (x: Int, length: Int, intValue: Int)
type Sym = (x: Int, charValue: Char)

trait Element[T]:
  extension (t: T)
    def x: Int
    def length: Int

given Element[Num] with
  extension (n: Num)
    def x = n.x
    def length = n.length

given Element[Sym] with
  extension (s: Sym)
    def x = s.x
    def length = 1

type Grid = (numbers: IArray[IArray[Num]], symbols: IArray[IArray[Sym]])

def parse(input: String): Grid =
  IArray.from(input.linesIterator.map(parseRow(_).dropNames)).unzip

def surrounds[A: Element, E: Element](y: Int, from: A, rows: IArray[IArray[E]]): List[E] =
  val (x0, x1, y0, y1) = (from.x - 1, from.x + from.length, y - 1, y + 1)
  def overlaps(e: E) = x0 <= (e.x + e.length - 1) && x1 >= e.x
  def findUp =
    if y0 < 0 then Nil
    else rows(y0).filter(overlaps).toList
  def findMiddle =
    rows(y).filter(overlaps).toList
  def findDown =
    if y1 >= rows.size then Nil
    else rows(y1).filter(overlaps).toList
  findUp ++ findMiddle ++ findDown

def part1(input: String): Long =
  val grid = parse(input)
  val matches =
    for
      (numbers, y) <- grid.numbers.iterator.zipWithIndex
      number <- numbers
    yield
      if surrounds(y, number, grid.symbols).sizeIs > 0 then number.intValue
      else 0
    end for
  matches.sum

def part2(input: String): Long =
  val grid = parse(input)
  val matches =
    for
      (symbols, y) <- grid.symbols.iterator.zipWithIndex
      symbol <- symbols
      if symbol.charValue == '*'
    yield
      val combined = surrounds(y, symbol, grid.numbers)
      if combined.sizeIs == 2 then
        combined.map(_.intValue).product
      else
        0
    end for
  matches.sum

def parseRow(row: String): (numbers: IArray[Num], symbols: IArray[Sym]) =
  val numbers = IArray.newBuilder[Num]
  val symbols = IArray.newBuilder[Sym]
  def addSymbol(x: Int, value: Char) = symbols += ((x, value))
  def addNumber(x: Int, length: Int, value: Int) = numbers += ((x, length, value))
  var begin = -1 // -1 = not building an number, >= 0 = start of a number
  var length = -1 // length of number
  var digits = -1 // accumutator for number
  var colIdx = 0
  val it = row.iterator
  while it.hasNext do
    val curr = it.next()
    val isSeparator = curr == '.'
    val inNumber = begin >= 0
    val kindChanged =
      inNumber && !curr.isDigit
      || !inNumber && !isSeparator
    if kindChanged then
      if inNumber then // end of entity
        addNumber(x = begin, length = length, value = digits)
        begin = -1
        length = -1
        digits = -1
      end if
      if curr.isDigit then // e.g line starts with a number
        begin = colIdx
        digits = curr.asDigit
        length = 1
      else if !isSeparator then
        addSymbol(x = colIdx, value = curr)
    else if inNumber then
      digits = digits * 10 + curr.asDigit
      length += 1
    end if
    colIdx += 1
  end while
  if begin >= 0 then // end of line
    addNumber(x = begin, length = length, value = digits)
  (numbers = numbers.result(), symbols = symbols.result())

import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")
