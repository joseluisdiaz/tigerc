package tiger

object Tree {

  trait TREE

  sealed abstract class Expr extends TREE {}

  case class CONST(v: Int) extends Expr
  case class NAME(n: Temp.Label) extends Expr
  case class TEMP(t: Temp.Temp) extends Expr
  case class BINOP(o: BinaryOp, l: Expr, r: Expr) extends Expr
  case class MEM(e: Expr) extends Expr

  case class CALL(e: Expr, args: List[Expr]) extends Expr
  case class ESEQ(s: Stm, e: Expr) extends Expr

  sealed abstract class Stm extends TREE

  case class MOVE(d: Expr, s: Expr) extends Stm {
    override def toString = s"MOVE( d = $d, s = $s )"
  }
  case class EXP(e: Expr) extends Stm
  case class JUMP(e: Expr, labs: List[Temp.Label]) extends Stm
  case class CJUMP(o: Relop, e1: Expr, e2: Expr, t: Temp.Label, f: Temp.Label) extends Stm
  case class SEQ(s1: Stm, s2: Stm) extends Stm
  case class LABEL(n: Temp.Label) extends Stm

  sealed abstract class BinaryOp
  case object PLUS extends BinaryOp
  case object MINUS extends BinaryOp
  case object MUL extends BinaryOp
  case object DIV extends BinaryOp
  case object AND extends BinaryOp
  case object OR extends BinaryOp
  case object LSHIFT extends BinaryOp
  case object RSHIFT extends BinaryOp
  case object ARSHIFT extends BinaryOp
  case object XOR extends BinaryOp

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


  /**
   * Sequences
   * @param stms
   * @return
   */

  // TODO: mover estos metodos a 'applys' de SEQ
  def seq(stms: List[Stm]): Stm = if (stms.isEmpty) EXP(CONST(0)) else if (stms.length == 1 ) stms.head else SEQ(stms.head, seq(stms.tail))

  def seq(s1: Stm, l: List[Stm]): Stm = seq(s1 :: l)

  def seq(s1: Stm, s2: Stm, l: List[Stm]): Stm = seq(s1 :: s2 :: l)

  def seq(s1: Stm, s2: Stm, s3: Stm, l: List[Stm]): Stm = seq(s1 :: s2 :: s3 :: l)

  def seq(s1: Stm, s2: Stm, s3: Stm, s4: Stm, l: List[Stm]): Stm = seq(s1 :: s2 :: s3 :: s4 :: l)

  def seq(s1: Stm, s2: Stm, s3: Stm, s4: Stm, s5: Stm, l: List[Stm]): Stm = seq(s1 :: s2 :: s3 :: s4 :: s5 :: l)

  def seq(s: Stm*): Stm = seq(s.toList)

}
