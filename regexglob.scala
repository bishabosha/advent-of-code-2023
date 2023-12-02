package regexglob

import quoted.*
import scala.annotation.compileTimeOnly

/**
  * example:
  *
  ```
    def parseColors(pair: String): Colors =
      val r"$value $name" = pair: @unchecked
      (color = name, count = value.toInt)

    def parse(line: String): Game =
      val r"Game $id: ${rs"$pairss...(, )"}...(; )" = line: @unchecked
      (game = id.toInt, hands = pairss.map(_.map(parseColors)))
  ```
  * @param pair
  */
object RegexGlobbing:
  class RSStringContext[+R](pattern: Pattern)
  class RSSStringContext[+R](pattern: Pattern)

  enum PatternElement:
    case Glob(pattern: String)
    case Split(splitOn: String, pattern: String)

  object PatternElement:
    given ToExpr[PatternElement] with
      def apply(patternElement: PatternElement)(using Quotes): Expr[PatternElement] = patternElement match
        case Glob(pattern) =>
          '{ Glob(${Expr(pattern)}) }
        case Split(splitOn, pattern) =>
          '{ Split(${Expr(splitOn)}, ${Expr(pattern)}) }

  case class Pattern(elements: Seq[PatternElement]):
    def unapply(scrutinee: String): Option[Any] =
      def foldGlobs(acc: Seq[String], self: PatternElement): Seq[String] =
        self match
          case PatternElement.Glob(pattern) => acc :+ pattern
          case PatternElement.Split(_, pattern) => acc :+ pattern
      val globs = elements.foldLeft(Vector.empty[String]: Seq[String])(foldGlobs)
      StringContext.glob(globs, scrutinee) match
        case None => None
        case Some(stage1) =>
          val stage2 = stage1.lazyZip(elements.drop(1)).map: (globbed, element) =>
            element match
              case PatternElement.Glob(_) => globbed
              case PatternElement.Split(splitOn, _) => globbed.split(splitOn).toIndexedSeq
          if stage2.size == 1 then
            Some(stage2.head)
          else
            Some(Tuple.fromArray(stage2.toArray))

  case class Patterns(base: Pattern):
    def unapply(scrutinee: Seq[String]): Option[Any] =
      val stage1 = scrutinee.map(base.unapply)
      if stage1.forall(_.isDefined) then
        var state = Map.empty[Int, Any]
        stage1.map(_.get).foreach: res =>
          res match
            case res: Tuple =>
              res.productIterator.zipWithIndex.foreach:
                case (value, index) =>
                  value match
                    case value: String =>
                      val current = state.getOrElse(index, Vector.empty[String]).asInstanceOf[Vector[String]]
                      state = state.updated(index, current :+ value)
                    case seq: Seq[String] @unchecked =>
                      val current = state.getOrElse(index, Vector.empty[Seq[String]]).asInstanceOf[Vector[Seq[String]]]
                      state = state.updated(index, current :+ seq)
            case value: String =>
              val current = state.getOrElse(0, Vector.empty[String]).asInstanceOf[Vector[String]]
              state = state.updated(0, current :+ value)
            case seq: Seq[String] @unchecked =>
              val current = state.getOrElse(0, Vector.empty[Seq[String]]).asInstanceOf[Vector[Seq[String]]]
              state = state.updated(0, current :+ seq)
        val stage2_0 = state.toArray.sortBy(_._1).map(_._2)

        if stage2_0.size == 1 then
          Some(stage2_0.head)
        else
          Some(Tuple.fromArray(stage2_0))
      else
        None


  object Pattern:
    given ToExpr[Pattern] with
      def apply(pattern: Pattern)(using Quotes): Expr[Pattern] =
        '{ Pattern(${Expr.ofSeq(pattern.elements.map(Expr(_)))}) }

  extension (inline sc: scala.StringContext)
    @compileTimeOnly("should be used with `r` interpolator")
    transparent inline def r: RSStringContext[Any] = ${rsApplyExpr('sc)}
    transparent inline def rs: RSSStringContext[Any] = ${rssApplyExpr('sc)}

  extension [R](inline rsSC: RSStringContext[R])
    transparent inline def unapply(scrutinee: String): Option[R] =
      ${rsUnapplyExpr('rsSC, 'scrutinee)}

  extension [R](inline rssSC: RSSStringContext[R])
    transparent inline def unapply(scrutinee: Seq[String]): Option[R] =
      ${rssUnapplyExpr('rssSC, 'scrutinee)}

  def parsed(scExpr: Expr[StringContext])(using Quotes): Pattern =
    val sc: StringContext = scExpr.valueOrAbort
    val parts = sc.parts.map(scala.StringContext.processEscapes)
    val g +: rest = parts: @unchecked
    g match
      case s"...($regex)$rest0" =>
        quotes.reflect.report.errorAndAbort(s"split is not allowed without preceding splice: $g")
      case _ =>

    val rest0 = rest.map:
      case s"...($regex)$rest" =>
        if rest.indexOf("...") > 0 then
          quotes.reflect.report.errorAndAbort(s"split is not allowed without preceding splice: $rest")
        PatternElement.Split(regex, rest)
      case s"...$rest" =>
        quotes.reflect.report.errorAndAbort(s"split `$$foo...` is not allowed without qualifying regex e.g. `$$foo...(: )`: $rest")
      case rest =>
        PatternElement.Glob(rest)
    Pattern(PatternElement.Glob(g) +: rest0)

  def refineResult(pattern: Pattern)(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val args = pattern.elements.drop(1).map:
      case PatternElement.Glob(_) => TypeRepr.of[String]
      case PatternElement.Split(_, _) => TypeRepr.of[Seq[String]]
    if args.size == 1 then
      args.head
    else if args.size <= 22 then
      AppliedType(defn.TupleClass(args.size).typeRef, args.toList)
    else
      report.errorAndAbort(s"too many captures: ${args.size} (implementation restriction: max 22)")

  def refineResult2(pattern: Pattern)(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val args = pattern.elements.drop(1).map:
      case PatternElement.Glob(_) => TypeRepr.of[Seq[String]]
      case PatternElement.Split(_, _) => TypeRepr.of[Seq[Seq[String]]]
    if args.size == 1 then
      args.head
    else if args.size <= 22 then
      AppliedType(defn.TupleClass(args.size).typeRef, args.toList)
    else
      report.errorAndAbort(s"too many captures: ${args.size} (implementation restriction: max 22)")

  def rsApplyExpr(rsSCExpr: Expr[StringContext])(using Quotes): Expr[RSStringContext[Any]] =
    val pattern = parsed(rsSCExpr)
    val patternExpr = Expr(pattern)
    val patternTypeRepr = refineResult(pattern)
    patternTypeRepr.asType match
      case '[t] =>
        '{ new RSStringContext[t]($patternExpr) }

  def rssApplyExpr(rsSCExpr: Expr[StringContext])(using Quotes): Expr[RSSStringContext[Any]] =
    val pattern = parsed(rsSCExpr)
    val patternExpr = Expr(pattern)
    val patternTypeRepr = refineResult2(pattern)
    patternTypeRepr.asType match
      case '[t] =>
        '{ new RSSStringContext[t]($patternExpr) }

  def rssUnapplyExpr[R: Type](rssSCExpr: Expr[RSSStringContext[R]], scrutinee: Expr[Seq[String]])(using Quotes): Expr[Option[R]] =
    val '{ new RSSStringContext[R]($patternExpr: Pattern) } = rssSCExpr: @unchecked
    '{ new Patterns($patternExpr).unapply($scrutinee).asInstanceOf[Option[R]] }

  def rsUnapplyExpr[R: Type](rsSCExpr: Expr[RSStringContext[R]], scrutinee: Expr[String])(using Quotes): Expr[Option[R]] =
    val '{ new RSStringContext[R]($patternExpr: Pattern) } = rsSCExpr: @unchecked
    '{ $patternExpr.unapply($scrutinee).asInstanceOf[Option[R]] }
