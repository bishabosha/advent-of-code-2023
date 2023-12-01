package challenges

import scala.language.experimental.namedTuples

type Challenge = (day: Int, year: Int)

extension (c: Challenge) def show: String =
  val d = if c.day < 10 then s"0${c.day}" else c.day.toString
  s"${c.year}-day$d"

inline def inputToday() = os.read(os.pwd / "advent-of-code" / "inputs" / s"${today.show}")

inline def today: Challenge =
  sourcecode.FileName() match
    case s"$year-day$index.scala" =>
      (day = index.toInt, year = year.toInt)
    case _ =>
      (day = 0, year = 0)
