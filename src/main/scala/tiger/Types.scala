package tiger

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

    def isNil = this match {
      case NIL => true
      case _ => false
    }

    override def equals(that: Any) = (this, that) match {
      case (NIL, NIL) => true
      case (INT(_), INT(_)) => true
      case (UNIT, UNIT) => true
      case (STRING, STRING) => true

      case (NIL, RECORD(_)) => true
      case (RECORD(_), NIL) => true

      case (ARRAY(_), t@ARRAY(_)) => this.eq(t)
      case (RECORD(_), r@RECORD(_)) => this.eq(r)

      case (a, ALIAS(_, b)) => a == unpack(b)
      case (ALIAS(_, a), b) => unpack(a) == b

      case (FUNCTION(p1,r1), FUNCTION(p2,r2)) => (p1 == p2) && (r1 == r2)

      case (_, _) => false
    }

  }

  case object UNIT extends Ty

  case class INT(readOnly:Boolean) extends Ty //ro: readOnly

  case object STRING extends Ty

  case object NIL extends Ty

  case class RECORD(records: List[(String, Ty, Int)]) extends Ty

  case class ARRAY(tipo: Ty) extends Ty

  case class FUNCTION(params: List[Ty], result: Ty) extends Ty

  case class ALIAS(name: String, var ty: Option[Ty]) extends Ty {

    override def toString = "ALIAS(" + name + ")"
  }

  object INT {
    def apply():INT = INT(readOnly=false)
    def readOnly():INT = INT(readOnly=true)
  }

}

