package day03

import scala.language.experimental.namedTuples

import regexglob.RegexGlobbing.*
import challenges.*

@main def part1: Unit =
  println(s"the answer is ${part1(inputToday())}")

@main def part2: Unit =
  println(s"the answer is ${part2(inputToday())}")

type Entity = (x: Int, value: String)
type Grid = (numbers: IArray[IArray[Entity]], symbols: IArray[IArray[Entity]])

def parse(input: String): Grid =
  IArray.from(input.linesIterator.map(parseRow(_).dropNames)).unzip

def surrounds(y: Int, from: Entity, rows: IArray[IArray[Entity]]): List[Entity] =
  val boundingBox = (x = from.x - 1, y = y - 1, w = from.x + from.value.size, h = y + 1)
  val width = boundingBox.x to boundingBox.w
  def overlaps(e: Entity) =
    val eWidth = e.x to (e.x + e.value.size - 1)
    width.min <= eWidth.max && width.max >= eWidth.min
  def findUp =
    if boundingBox.y < 0 then Nil
    else rows(boundingBox.y).filter(overlaps).toList
  def findMiddle =
    rows(y).filter(overlaps).toList
  def findDown =
    if boundingBox.h >= rows.size then Nil
    else rows(boundingBox.h).filter(overlaps).toList
  findUp ++ findMiddle ++ findDown

def part1(input: String): Long =
  val grid = parse(input)
  val matches =
    for
      (numbers, y) <- grid.numbers.iterator.zipWithIndex
      number <- numbers
    yield
      if surrounds(y, number, grid.symbols).sizeIs > 0 then number.value.toInt
      else 0
    end for
  matches.sum

def part2(input: String): Long =
  val grid = parse(input)
  val matches =
    for
      (symbols, y) <- grid.symbols.iterator.zipWithIndex
      symbol <- symbols
      if symbol.value == "*"
    yield
      val combined = surrounds(y, symbol, grid.numbers)
      if combined.sizeIs == 2 then
        combined.map(_.value.toInt).product
      else
        0
    end for
  matches.sum

def parseRow(row: String): (numbers: IArray[Entity], symbols: IArray[Entity]) =
  val buf = StringBuilder()
  val numbers = IArray.newBuilder[Entity]
  val symbols = IArray.newBuilder[Entity]
  var begin = -1 // -1 = not building an entity, >= 0 = start of an entity
  var knownSymbol = -1 // trinary: -1 = unknown, 0 = number, 1 = symbol
  def addEntity(isSymbol: Boolean, x: Int, value: String) =
    val entity = (x = x, value = value)
    if isSymbol then symbols += entity
    else numbers += entity
  for (curr, colIdx) <- row.zipWithIndex do
    val isSeparator = curr == '.'
    val inEntity = begin >= 0
    val kindChanged =
      !inEntity && !isSeparator
      || isSeparator && inEntity
      || knownSymbol == 1 && curr.isDigit
      || knownSymbol == 0 && !curr.isDigit
    if kindChanged then
      if inEntity then // end of entity
        addEntity(isSymbol = knownSymbol == 1, x = begin, value = buf.toString)
        buf.clear()
      if isSeparator then // reset all state
        begin = -1
        knownSymbol = -1
      else // begin new entity
        begin = colIdx
        knownSymbol = if curr.isDigit then 0 else 1
        buf += curr
    else
      if !isSeparator then buf += curr
    end if
  end for
  if begin >= 0 then // end of line
    addEntity(isSymbol = knownSymbol == 1, x = begin, value = buf.toString)
  (numbers = numbers.result(), symbols = symbols.result())