package tiger

/**
 * Created by jose on 5/9/14.
 */

trait KindOfSweetType

trait Test {
  type SweetType <: KindOfSweetType

  def methodA(l: String, esc: Boolean): SweetType
  def methodB(a: SweetType): String
}

case class MySweetType(l:String, esc:Boolean) extends KindOfSweetType

class MyTest extends Test {

  type SweetType = MySweetType

  override def methodA(l: String, esc: Boolean): MySweetType = ???

  override def methodB(a: MySweetType): String = ???
}

object Test {
  def apply(): Test = new MyTest()
}

trait SomethingElse {
  def methodA(a:KindOfSweetType):String
}

class MySomethingElse extends SomethingElse {
  val test = Test()

  override def methodA(a: KindOfSweetType): String = test.methodB(a)
}

