package tiger

trait Temp {
  type Label
  type Temp

  def newTemp(): Temp
  def newLabel(): Label
  def namedLabel(s:String): Label
}

object Temp extends Temp {
  type Label = String
  type Temp = String

  var i, j = 0

  override def newTemp(): Temp = {
    i += 1
    "T" + i
  }

  override def newLabel(): Label = {
    j += 1
    "L" + i
  }

  override def namedLabel(s: String): Label = s
}