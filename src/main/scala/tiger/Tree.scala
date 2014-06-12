package tiger

object Tree {

  sealed abstract class Exp


  case class CONST(v: Int) extends Exp
  case class NAME(n: Temp.Label) extends Exp
  case class TEMP(t: Temp.Temp) extends Exp
  case class BINOP(o: Binop, l: Exp, r: Exp) extends Exp
  case class MEM(e: Exp) extends Exp

  case class CALL(e: Exp, args: List[Exp]) extends Exp
  case class ESEQ(s: Stm, e: Exp) extends Exp

  sealed abstract class Stm
  case class MOVE(e1: Exp, e2: Exp) extends Stm
  case class EXP(e: Exp) extends Stm
  case class JUMP(e: Exp, labs: List[Temp.Label]) extends Stm
  case class CJUMP(o: Relop, e1: Exp, e2: Exp, t: Temp.Label, f: Temp.Label) extends Stm
  case class SEQ(s1: Stm, s2: Stm) extends Stm
  case class LABEL(n: Temp.Label) extends Stm

  sealed abstract class Binop
  case object PLUS extends Binop
  case object MINUS extends Binop
  case object MUL extends Binop
  case object DIV extends Binop
  case object AND extends Binop
  case object OR extends Binop
  case object LSHIFT extends Binop
  case object RSHIFT extends Binop
  case object ARSHIFT extends Binop
  case object XOR extends Binop

  sealed abstract class Relop
  case object EQ extends Relop
  case object NE extends Relop
  case object LT extends Relop
  case object GT extends Relop
  case object LE extends Relop
  case object GE extends Relop
  case object ULT extends Relop
  case object ULE extends Relop
  case object UGT extends Relop
  case object UGE extends Relop

}
