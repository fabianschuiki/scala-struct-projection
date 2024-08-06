import scala.quoted.*

def selectUnderValue[T: Type](x: Expr[T], field: Expr[String])(using Quotes) = {
  import quotes.reflect.*
  // report.info("something running at compile time here")
  // val y = Select(x.asTerm, x.asTerm.tpe.asType.declaredField("hello"))
  // report.info("stuff: " + x.asTerm.show(using Printer.TreeStructure))
  val fieldName: String = field.asTerm match {
    case Inlined(_, _, Literal(StringConstant(k))) => k
  }
  val fieldSymbol: Symbol = x.asTerm.tpe.typeSymbol.fieldMember(fieldName)
  val y = x.asTerm.select(fieldSymbol)
  val yt = x.asTerm.tpe.memberType(fieldSymbol)
  report.info(f"projecting into field `${fieldSymbol}` of `${Type.show[T]}` with type `${yt.show}`")

  new java.io.PrintWriter("dummy.log") {
    write(f"x: ${x.asTerm.show(using Printer.TreeStructure)}\n")
    write(f"fieldName: ${fieldName}\n")
    write(f"fieldSymbol: ${fieldSymbol}\n")
    write(f"x.name: ${yt.show}\n")
    write(f"y: ${y.show(using Printer.TreeStructure)}\n")
    write(f"y.tpe: ${y.tpe.show}\n")
    write(f"y.tpe: ${y.tpe.simplified.show}\n")
    write(f"y.tpe: ${y.tpe.dealias.show}\n")
    write(f"x.tpe: ${x.asTerm.tpe.show}\n")
    write(f"x.tpe: ${Type.of[T]}\n")
    close
  }

  yt.asType match {
    case '[yt] => '{ Value[yt](${ y.asExprOf[yt] }) }
  }
}
