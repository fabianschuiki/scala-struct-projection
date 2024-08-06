import scala.language.dynamics

/// Wrapper for concrete values. Wraps around a potentially user-defined type
/// that can be selected into through `.` syntax. Basically allows for:
/// ```
/// class Foo(val x: Int, val y: String)
/// val foo = Value(Foo())
/// foo.x  // Value[Foo] -> Value[Int]
/// foo.y  // Value[Foo] -> Value[String]
/// ```
class Value[T](val inner: T) extends Dynamic {
  transparent inline def selectDynamic[R](name: String): Value[R] = ${ selectUnderValue('inner, 'name) }
  override def toString: String = f"Value(${inner})"
  def :=(other: Value[T]) = {
    unifyTypes(this.inner, other.inner)
  }
}

class Foo {
  val x = 9001
  val y = "hello"
}

object Wire {
  def apply[T](tpe: T): Value[T] = {
    Value(tpe)
  }
}

def unifyTypes[A, B](a: A, b: B) = (a, b) match {
  case (UInt(wa0), UInt(wb0)) => {
    val wa = wa0.simplify
    val wb = wb0.simplify
    if (wa != wb) {
      (wa, wb) match {
        case (ia: InferrableIntParam, ib: InferrableIntParam) => {
          if (ia.id < ib.id) {
            println(f"inferred ${ib} = ${ia}")
            ib.inferred = Some(ia)
          } else {
            println(f"inferred ${ia} = ${ib}")
            ia.inferred = Some(ib)
          }
        }
        case (ia: InferrableIntParam, _) => {
          println(f"inferred ${ia} = ${wb}")
          ia.inferred = Some(wb)
        }
        case (_, ib: InferrableIntParam) => {
          println(f"inferred ${ib} = ${wa}")
          ib.inferred = Some(wa)
        }
      }
    }
  }
  case _ => println(f"incompatible types ${a} and ${b}")
}

abstract class IntParam {
  def simplify: IntParam = this
}

class ConstIntParam(val width: Int) extends IntParam {
  override def toString = f"${width}"
}

class FreeIntParam extends IntParam

class InferrableIntParam extends IntParam {
  val id = IntParam.nextId
  var inferred: Option[IntParam] = None
  IntParam.nextId += 1

  override def toString = inferred match {
    case Some(p) => p.toString
    case None => f"?${id}"
  }

  override def simplify: IntParam = inferred match {
    case Some(p) => {
      val ps = p.simplify
      inferred = Some(ps)
      ps
    }
    case None => this
  }
}

object IntParam {
  var nextId = 0
}

case class UInt(val width: IntParam)

class AxiBuffer {
  val AW = InferrableIntParam()
  val DW = InferrableIntParam()
  val IW = InferrableIntParam()

  val in_addr = UInt(AW)
  val in_data = UInt(DW)
  val in_id = UInt(IW)

  val out_addr = UInt(AW)
  val out_data = UInt(DW)
  val out_id = UInt(IW)

  override def toString = f"AxiBuffer(${AW}, ${DW}, ${IW})"
}

object Main {
  def main(args: Array[String]): Unit = {
    println("----- 8< ----- Basic Wire Checks ----- 8< -----")
    val a = Wire(UInt(InferrableIntParam()))
    val b = Wire(UInt(InferrableIntParam()))
    val c = Wire(UInt(InferrableIntParam()))
    println(f"a = ${a}")
    println(f"b = ${b}")
    println(f"c = ${c}")

    a := b
    println(f"a = ${a}")
    println(f"b = ${b}")
    println(f"c = ${c}")

    c := b
    println(f"a = ${a}")
    println(f"b = ${b}")
    println(f"c = ${c}")

    val z = Wire(UInt(ConstIntParam(42)))
    println(f"z = ${z}")

    c := z
    println(f"a = ${a}")
    println(f"b = ${b}")
    println(f"c = ${c}")
    println(f"z = ${z}")

    println()
    println("----- 8< ----- Params Through Instances ----- 8< -----")
    val buf0 = Wire(AxiBuffer())
    val buf1 = Wire(AxiBuffer())
    println(f"buf0 = ${buf0}")
    println(f"buf1 = ${buf1}")

    buf1.in_addr := buf0.out_addr
    buf1.in_data := buf0.out_data
    buf1.in_id := buf0.out_id
    println(f"buf0 = ${buf0}")
    println(f"buf1 = ${buf1}")

    z := buf0.in_addr
    // buf0.in_addr := z // <-- fails in type inference
    Wire(UInt(ConstIntParam(512))) := buf1.out_data
    Wire(UInt(ConstIntParam(5))) := buf0.in_id
    // val id = Wire(UInt(FreeIntParam()))  // <-- fails in type inference
    // id
    println(f"buf0 = ${buf0}")
    println(f"buf1 = ${buf1}")

    // val buf2 = Wire(AxiBuffer())
    // buf2 := buf1
  }

  // def main(args: Array[String]): Unit = {
  //   val foo = Value(Foo())
  //   val x: Value[Int] = foo.x
  //   val y: Value[String] = foo.y
  //   println(f"foo = ${foo}")
  //   println(f"foo.x = ${x}")
  //   println(f"foo.y = ${y}")
  // }
}
