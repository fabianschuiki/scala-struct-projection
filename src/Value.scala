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
  transparent inline def selectDynamic[R](name: String) = ${ selectUnderValue('inner, 'name) }
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
  case (ua: Unifiable, ub: Unifiable) => if (!ua.unifyWith(ub)) {
    println(f"error: incompatible types ${a} and ${b}")
  }
  case _ => println(f"error: incompatible types ${a} and ${b}")
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

trait Unifiable {
  def unifyWith(other: Unifiable): Boolean
}

class AxiBuffer extends Unifiable {
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

  def unifyWith(other: Unifiable) = other match {
    case other: AxiBuffer => {
      unifyTypes(in_addr, other.in_addr)
      unifyTypes(in_data, other.in_data)
      unifyTypes(in_id, other.in_id)
      unifyTypes(out_addr, other.out_addr)
      unifyTypes(out_data, other.out_data)
      unifyTypes(out_id, other.out_id)
      true
    }
    case _ => false
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println("----- 8< ----- Basic Wire Checks ----- 8< -----")
    println()
    val a = Wire(UInt(InferrableIntParam()))
    val b = Wire(UInt(InferrableIntParam()))
    val c = Wire(UInt(InferrableIntParam()))
    println(f"a = ${a}")
    println(f"b = ${b}")
    println(f"c = ${c}")

    println("\n# Assign a := b")
    a := b
    println(f"a = ${a}")
    println(f"b = ${b}")
    println(f"c = ${c}")

    println("\n# Assign c := b")
    c := b
    println(f"a = ${a}")
    println(f"b = ${b}")
    println(f"c = ${c}")

    println("\n# Create z: UInt(42)")
    val z = Wire(UInt(ConstIntParam(42)))
    println(f"z = ${z}")

    println("\n# Assign c := z")
    c := z
    println(f"a = ${a}")
    println(f"b = ${b}")
    println(f"c = ${c}")
    println(f"z = ${z}")

    println()
    println("----- 8< ----- Params Through Instances ----- 8< -----")
    println()
    val buf0 = Wire(AxiBuffer())
    val buf1 = Wire(AxiBuffer())
    println(f"buf0 = ${buf0}")
    println(f"buf1 = ${buf1}")

    println("\n# Assign buf1.in_* := buf0.out_*")
    buf1.in_addr := buf0.out_addr
    buf1.in_data := buf0.out_data
    buf1.in_id := buf0.out_id
    println(f"buf0 = ${buf0}")
    println(f"buf1 = ${buf1}")

    println("\n# Assign buf0.in_addr := z")
    buf0.in_addr := z
    Wire(UInt(ConstIntParam(512))) := buf1.out_data
    println(f"buf0 = ${buf0}")
    println(f"buf1 = ${buf1}")

    println("\n# Assign buf2.in_id := id")
    val id = Wire(UInt(InferrableIntParam()))
    val buf2 = Wire(AxiBuffer())
    buf2.in_id := id
    println(f"id = ${id}")
    println(f"buf2 = ${buf2}")

    println("\n# Assign id := UInt(5)")
    id := Wire(UInt(ConstIntParam(5)))
    println(f"id = ${id}")
    println(f"buf2 = ${buf2}")

    // Wholesale connect buf2 to buf1. This should unify the two AxiBuffer
    // types by unifying corresponding fields.
    println("\n# Assign buf2 := buf1")
    buf2 := buf1
    println(f"id = ${id}")
    println(f"buf0 = ${buf0}")
    println(f"buf1 = ${buf1}")
    println(f"buf2 = ${buf2}")
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
