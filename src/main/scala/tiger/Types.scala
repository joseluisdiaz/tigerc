package tiger

import tiger.Types.Ty

/**
 * User: jose
 * Date: 9/18/13
 * Time: 1:18 PM
 */

object Types {

  def unpack(ty: Ty): Ty = ty match {
    case ALIAS(_, x) => unpack(x)
    case x => x
  }

  def unpack(option: Option[Ty]): Ty = option match {
    case Some(a) => a
    case _ => throw new Error("Type alias not found")
  }


  sealed abstract class Ty {

    // TODO: Se podra usar una funcion parcial?
    override def equals(that: Any) = (this, that) match {
      case (NIL(), NIL()) => true
      case (INT(), INT()) => true
      case (UNIT(), UNIT()) => true
      case (STRING(), STRING()) => true

      case (NIL(), RECORD(_)) => true
      case (RECORD(_), NIL()) => true

      case (ARRAY(_), t@ARRAY(_)) => this.eq(t)
      case (RECORD(_), r@RECORD(_)) => this.eq(r)

      case (a, ALIAS(_, b)) => a == unpack(b)
      case (ALIAS(_, a), b) => unpack(a) == b

      case (FUNCTION(p1,r1), FUNCTION(p2,r2)) => (p1 == p2) && (r1 == r2)

      case (_, _) => false
    }

  }

  case class UNIT() extends Ty

  case class INT() extends Ty

  case class STRING() extends Ty

  case class NIL() extends Ty

  case class RECORD(records: List[(String, Ty, Int)]) extends Ty

  case class ARRAY(tipo: Ty) extends Ty

  case class FUNCTION(params: List[Ty], result: Ty) extends Ty

  case class ALIAS(name: String, var ty: Option[Ty]) extends Ty

}


object Env {
  val mainLevel = ()

  sealed abstract class EnvEntry

  case class VIntro() extends EnvEntry

  /* int readonly */
  case class VarEntry(ty: Ty) extends EnvEntry

  case class FuncEntry(level: Unit, label: Temp.Label, params: List[Ty],
                       result: Ty, extern: Boolean) extends EnvEntry


}

trait Temp {
  type Label = String
  type Temp = String

  def newTemp(): Temp

  def newLabel(): Label

}

object Temp extends Temp {
  var i, j = 0;

  def newTemp(): Temp = {
    i = i + 1;
    "T" + i
  }

  def newLabel(): Label = {
    j = j + 1;
    "L" + i
  }

}
