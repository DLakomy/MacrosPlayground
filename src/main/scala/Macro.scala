package macrosPlayground

import scala.quoted.*

def mkClearFnImpl[S: Type, A](
    path: Expr[S => A],
    empty: Expr[A]
)(using Quotes): Expr[S => S] =

  import quotes.reflect.*

  def extractFocus(tree: Tree): String = tree match
    case Inlined(_, _, p) =>
      extractFocus(p)
    case Lambda(_, Select(Ident(_), fieldName)) =>
      fieldName
    case _ =>
      report.errorAndAbort("Unsupported shape, should be a path, like _.id")

  val tt = TypeTree.of[S]
  if !tt.symbol.flags.is(Flags.Case) then
    report.errorAndAbort(s"Only case classes are supported, ${tt.symbol.name} is not a case class")

  val focusedField = extractFocus(path.asTerm)

  val fields  = tt.symbol.caseFields.map(_.name)
  val clsName = tt.symbol.name

  if !fields.contains(focusedField) then
    report.errorAndAbort(s"${focusedField} is not a field in the class $clsName")

  def functionBody(param: Tree): Expr[S] =
    val paramt = param.asExprOf[S].asTerm
    val sym    = paramt.symbol

    val copyMethod = sym.methodMember("copy").head

    val methods = fields.zipWithIndex.map: (fld, idx) =>
      if fld == focusedField then NamedArg(fld, empty.asTerm)
      else paramt.select(sym.methodMember("copy$default$" + (idx + 1)).head)

    val ap = Apply(
      Select(paramt, copyMethod),
      methods
    )

    ap.asExprOf[S]

  val functionDefSymbol = Symbol.newMethod(
    Symbol.spliceOwner,
    "clearGenerated",
    MethodType(List("obj"))(
      _ => List(tt.tpe),
      _ => TypeRepr.of[S]
    )
  )

  val functionDef = DefDef(
    functionDefSymbol,
    {
      case List(List(obj)) => Some(functionBody(obj).asTerm.changeOwner(functionDefSymbol))
      case _               => None
    }
  )

  Block(List(functionDef), Closure(Ref(functionDefSymbol), None)).asExprOf[Function1[S, S]]


inline def mkClearFn[S, A: Empty](inline fn: S => A): S => S = ${
  mkClearFnImpl('fn, '{ summon[Empty[A]].empty })
}


inline def clearFld[S, A: Empty](inline obj: S, inline focus: S => A): S =
  val f = mkClearFn(focus)
  f(obj)
