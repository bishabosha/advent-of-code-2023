package regexglob

import quoted.*
import scala.annotation.compileTimeOnly


object RegexGlobbing:
  extension (inline sc: scala.StringContext)

    /** use in patterns like `case r"$foo...(, )" =>` */
    @compileTimeOnly("should be used with `r` pattern interpolator")
    transparent inline def r: RSStringContext[Any] = ${rsApplyExpr('sc)}

  extension [R](inline rsSC: RSStringContext[R])
    /** enables compile time splitting of string globs with the syntax `r"$foo...(, )"`.
     * can be arbitrarily nested, e.g. `r"${r"$foos...(, )"}...(; )"`.
     * ```
     * case r"Foo $id: $bars...(, )" => (id, bars)
     * ```
     * is equivalent to
     * ```
     * case s"Foo $id: $bars0" => (id, bars0.split(", ").toIndexedSeq)
     * ```
     */
    transparent inline def unapply[Base](scrutinee: Base): Option[Any] =
      ${rsUnapplyExpr('rsSC, 'scrutinee)}

  class RSStringContext[+Base](pattern: Pattern)

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

  case class PatternLive(elements: Seq[PatternElement], levels: Int):
    private def unapply0(scrutinee: String): Option[Any] =
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

    private def unapplyN(scrutinee: Any, level: Int): Option[Any] =
      level match
        case 0 => unapply0(scrutinee.asInstanceOf[String])
        case i =>
          val stageN1 = scrutinee.asInstanceOf[Seq[Any]].map(unapplyN(_, i - 1))
          if stageN1.forall(_.isDefined) then
            var state = Map.empty[Int, Any]
            stageN1.map(_.get).foreach: res =>
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

    def unapply[Base](scrutinee: Base): Option[Any] = unapplyN(scrutinee, levels)

  case class Pattern(elements: Seq[PatternElement])

  object Pattern:
    given ToExpr[Pattern] with
      def apply(pattern: Pattern)(using Quotes): Expr[Pattern] =
        '{ Pattern(${Expr.ofSeq(pattern.elements.map(Expr(_)))}) }

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

  def rsApplyExpr(rsSCExpr: Expr[StringContext])(using Quotes): Expr[RSStringContext[Any]] =
    val pattern = parsed(rsSCExpr)
    val patternExpr = Expr(pattern)
    val patternTypeRepr = refineResult(pattern)
    patternTypeRepr.asType match
      case '[t] =>
        '{ new RSStringContext[t]($patternExpr) }

  def wrapping[Base: Type](using Quotes): Int =
    import quotes.reflect.*
    Type.of[Base] match
      case '[String] => 0
      case '[Seq[t]] => wrapping[t] + 1
      case _ => report.errorAndAbort(s"unsupported type: ${TypeRepr.of[Base]}")

  def wrap[Elem: Type](times: Int)(using Quotes): Type[?] =
    import quotes.reflect.*
    times match
      case 0 => Type.of[Elem]
      case n =>
        wrap[Elem](n - 1) match
          case '[t] => Type.of[Seq[t]]

  def wrapAll[R: Type](times: Int)(using Quotes): Type[?] =
    import quotes.reflect.*
    val args = wrapAllSub[R](times)
    if args.size == 1 then
      args.head
    else if args.size <= 22 then
      AppliedType(defn.TupleClass(args.size).typeRef, args.toList.map({ case '[t] => TypeRepr.of[t] })).asType
    else
      report.errorAndAbort(s"too many captures: ${args.size} (implementation restriction: max 22)")

  def wrapAllSub[R: Type](times: Int)(using Quotes): List[Type[?]] =
    import quotes.reflect.*
    val consClass = Symbol.requiredClass("scala.*:")
    Type.of[R] match
      case '[t *: ts] =>
        wrap[t](times) :: wrapAllSub[ts](times)
      case '[EmptyTuple] => Nil
      case '[singleton] => wrap[singleton](times) :: Nil
      case _ => report.errorAndAbort(s"unsupported type: ${TypeRepr.of[R]}")

  def rsUnapplyExpr[R: Type, Base: Type](rsSCExpr: Expr[RSStringContext[R]], scrutinee: Expr[Base])(using Quotes): Expr[Option[Any]] =
    import quotes.reflect.*
    val '{ new RSStringContext[R]($patternExpr: Pattern) } = rsSCExpr: @unchecked
    val levels = wrapping[Base]
    val returnType = wrapAll[R](levels)
    val levelsExpr = Expr(levels)
    returnType match
      case '[t] =>
        '{ PatternLive($patternExpr.elements, levels = $levelsExpr).unapply($scrutinee).asInstanceOf[Option[t]] }
