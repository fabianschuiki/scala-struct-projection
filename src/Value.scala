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
}

class Foo {
  val x = 9001
  val y = "hello"
}

object Main {
  def main(args: Array[String]): Unit = {
    val foo = Value(Foo())
    val x: Value[Int] = foo.x
    val y: Value[String] = foo.y
    println(f"foo = ${foo}")
    println(f"foo.x = ${x}")
    println(f"foo.y = ${y}")
  }
}
