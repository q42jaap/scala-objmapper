package util.objmapper


object ObjMapperExample extends App {

  case class MainClass(foo: String, bar: String, baz: Int)

  case class SubSetClass1(bar: String)

  case class SubSetClass2(foo: String, bar: String)

  case class SubSetClass3(bar: String, baz: Int)

  case class NotASubSetClass(foo: Int)

  def mapperExample1() {
    import Macros._

    implicit val mapper = objMapper[MainClass, SubSetClass1]

    val mainVal = MainClass(foo = "fooVal", bar = "barVal", baz = 42)
    val mappedVal: SubSetClass1 = ObjMapper.mapValue(mainVal)

    println("Example1")
    printValues(mainVal, mappedVal)
  }

  def mapperExample2() {
    import Macros._

    implicit val mapper = objMapper[MainClass, SubSetClass2]

    val mainVal = MainClass(foo = "fooVal", bar = "barVal", baz = 42)
    val mappedVal: SubSetClass2 = ObjMapper.mapValue(mainVal)

    println("Example2")
    printValues(mainVal, mappedVal)
  }

  def mapperExample3() {
    import Macros._

    implicit val mapper = objMapper[MainClass, SubSetClass3]

    val mainVal = MainClass(foo = "fooVal", bar = "barVal", baz = 42)
    val mappedVal: SubSetClass3 = ObjMapper.mapValue(mainVal)

    println("Example3")
    printValues(mainVal, mappedVal)
  }

  def mapperNotCompilesExample() {
    // enable line below to see compile error
    // val mapper = Macros.objMapper[MainClass, NotASubSetClass]
  }


  def printValues(mainVal: Any, mappedVal: Any) {
    println()
    println(s"main value: $mainVal")
    println(s"mapped value: $mappedVal")
    println()
    println()
  }

  mapperExample1()
  mapperExample2()
  mapperExample3()


}
