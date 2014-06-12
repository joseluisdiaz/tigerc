package tiger

import tiger.Tree._

import scala.collection.mutable
import tiger.Temp.Label
import tiger.Types._
import tiger.Abs._
import scala.Some
import tiger.Tree.EXP
import tiger.Tree.MEM
import tiger.Types.Ty
import tiger.Tree.SEQ
import tiger.Tree.ESEQ
import tiger.Tree.MOVE
import tiger.Tree.BINOP
import tiger.Tree.NAME
import tiger.Tree.CJUMP
import tiger.Frame.{extenalCall, InFrame}
import tiger.Tree.TEMP
import tiger.Tree.JUMP
import tiger.Tree.CONST
import tiger.Tree.LABEL
import java.beans.Expression


object SemanHelper {

  /**
   * Seman interoperability
   */
  sealed case class InteropExp()

  case class Ex(e: Tree.Exp) extends InteropExp

  case class Nx(s: Tree.Stm) extends InteropExp

  case class Cx(genstm: (Temp.Label, Temp.Label) => Tree.Stm) extends InteropExp


  def seq(stms: List[Stm]): Stm = if (stms.isEmpty) EXP(CONST(0)) else stms reduce SEQ

  def seq(s1: Stm, l: List[Stm]) = seq(s1 :: l)

  def seq(s1: Stm, s2: Stm, l: List[Stm]) = seq(s1 :: s2 :: l)

  def seq(s1: Stm, s2: Stm, s3: Stm, l: List[Stm]) = seq(s1 :: s2 :: s3 :: l)

  def seq(s1: Stm, s2: Stm, s3: Stm, s4: Stm, l: List[Stm]) = seq(s1 :: s2 :: s3 :: s4 :: l)

  def seq(s1: Stm, s2: Stm, s3: Stm, s4: Stm, s5: Stm, l: List[Stm]) = seq(s1 :: s2 :: s3 :: s4 :: s5 :: l)

  def seq(s: Stm*) = seq(s.toList)


  def unEx(ex: InteropExp) = ex match {
    case Ex(e) => e
    case Nx(s) => ESEQ(s, CONST(0))
    case Cx(genstm) =>
      val r = Temp.newTemp()
      val t = Temp.newLabel()
      val f = Temp.newLabel()

      ESEQ(
        seq(MOVE(TEMP(r), CONST(1)),
          genstm(t, f),
          LABEL(f),
          MOVE(TEMP(r), CONST(0)),
          LABEL(t)),
        TEMP(r))
  }

  def unNx(ex: InteropExp) = ex match {
    case Ex(e) => EXP(e)
    case Nx(s) => s
    case Cx(genstm) =>
      val t = Temp.newLabel()
      val f = Temp.newLabel()

      seq(genstm(t, f),
        LABEL(t),
        LABEL(f))
  }


  def unCx(ex: InteropExp) = ex match {
    case Ex(CONST(0)) => (t: Temp.Label, f: Temp.Label) => JUMP(NAME(f), List(f))
    case Ex(CONST(_)) => (t: Temp.Label, f: Temp.Label) => JUMP(NAME(t), List(t))

    case Ex(e) => (t: Temp.Label, f: Temp.Label) => CJUMP(NE, e, CONST(0), t, f)

    case Nx(s) => throw new Error("Error (UnCx(Nx..))")
    case Cx(genstm) => genstm
  }
}

trait Translate {
  def whileExp(expression: Seman.Expression, expression1: _root_.tiger.Seman.translate.Expression) = ???


  type Level
  type Access
  type Expression

  def newLevel(parent: Option[Level], name: Temp.Label, formals: List[Boolean]): Level

  def formals(l: Level): List[Access]

  def allocLocal(l: Level, esc: Boolean): Access

  def outermost(): Level

  // While

  def preWhile()
  def postWhile()


  // Variables
  def simpleVar(acc: Access, currentLevel: Level): Expression

  def subscriptVar(leftValue: Expression, index: Expression): Expression

  def fieldVar(record: Expression, offset: Int): Expression

  // Expressions
  def unitExp(): Expression

  def nilExp(): Expression

  def intExp(n: Int): Expression

  def stringExp(s: String): Expression

  def binOpExp(op: Oper, left: Expression, rigth: Expression): Expression

  def relOpExp(op: Oper, ty: Types.Ty, left: Expression, rigth: Expression): Expression

  def recordExp(expressions: List[Expression]): Expression

  def arrayExp(size: Expression, init: Expression): Expression

  def seqExp(expressions: List[Expression]): Expression

  def assignExp(left: Expression, right: Expression): Expression

}

class MyTranslate extends Translate {

  import SemanHelper._

  class MyLevel(val parent: Option[MyLevel], val frame: Frame) {

    def count(to: MyLevel): Int = parent match {
      //      case None => op(this,z)
      case _ if this == to => 1
      case Some(x) => 1 + x.count(to)
    }

  }

  object MyLevel {
    def apply(parent: Option[MyLevel], frame: Frame) = new MyLevel(parent, frame)
  }

  override type Level = MyLevel
  override type Access = (MyLevel, Frame#Access)
  override type Expression = InteropExp

  val levels = new mutable.Stack[Level]

  //  override def newLevel(parent: Option[Level], name: Temp.Label, formals: List[Boolean]):Level =
  //

  override def newLevel(parent: Option[MyLevel], name: Label, formals: List[Boolean]): MyLevel =
    MyLevel(parent, Frame(name, true :: formals))

  override def formals(l: MyLevel): List[(MyLevel, Frame#Access)] = l.frame.formals map ((l, _))

  override def allocLocal(l: MyLevel, esc: Boolean): (MyLevel, Frame#Access) = (l, l.frame.allocLocal(esc))

  override def outermost(): Level = ???

  // Implicit convertions
  implicit def temp2ExpTemp(value: Temp.Temp) = TEMP(value)

  implicit def label2ExpLabel(value: Temp.Label) = LABEL(value)

  implicit def int2contant(value: Int) = CONST(value)

  implicit def transBinOp(op: Oper) = op match {
    case PlusOp => PLUS
    case MinusOp => MINUS
    case TimesOp => MUL
    case DivideOp => DIV
  }

  implicit def transRelOp(op: Oper) = op match {
    case EqOp => EQ
    case NeqOp => NE
    case LtOp => LT
    case LeOp => LE
    case GtOp => GT
    case GeOp => GE
  }


  val labels = new mutable.Stack[Temp.Label]

  override def preWhile(): Unit = labels.push(Temp.newLabel())

  override def postWhile(): Unit = labels.pop()


  /**
   * Interaction with SEMAN
   */

  // variables
  override def simpleVar(acc: (MyLevel, Frame#Access), currentLevel: MyLevel): InteropExp = {
    val SL = InFrame(-1)

    // static link calculation
    def genStaticLink(n: Int): Tree.Exp = n match {
      case 0 => Frame.exp(acc._2, TEMP(Frame.FP))
      case _ => Frame.exp(SL, genStaticLink(n - 1))
    }

    Ex(genStaticLink(0))
  }

  override def subscriptVar(array: InteropExp, index: InteropExp): InteropExp = {
    val arrayEx = unEx(array)
    val indexEx = unEx(index)

    val arrayTemp = Temp.newTemp()
    val indexTemp = Temp.newTemp()

    Ex(ESEQ(seq(
      MOVE(arrayTemp, arrayEx),
      MOVE(indexTemp, indexEx)
      //      EXP(externalCall("_checkindex", ra, ri)))))
    ),
      MEM(BINOP(PLUS, arrayTemp, BINOP(MUL, indexTemp, Frame.WS)))))
  }


  override def fieldVar(record: InteropExp, offset: Int): InteropExp = {
    val recordTemp = Temp.newTemp()

    Ex(ESEQ(seq(
      MOVE(recordTemp, unEx(record))
      //      EXP(externalCall("_checkRecord", List(TEMP(ra), TEMP(ri)))))
    ),
      MEM(BINOP(PLUS, recordTemp, offset * Frame.WS))))

  }

  // Expressions
  override def unitExp(): InteropExp = Nx(EXP(0))

  override def nilExp(): InteropExp = Ex(0)

  override def intExp(n: Int): InteropExp = Ex(n)


  override def relOpExp(op: Oper, ty: Ty, left: InteropExp, rigth: InteropExp): InteropExp = ty match {
    case INT(_) =>
      Cx((t: Temp.Label, f: Temp.Label) => CJUMP(op, unEx(left), unEx(rigth), t, f))

    case STRING =>
      val l = Temp.newTemp()
      val r = Temp.newTemp()
      val rt = Temp.newTemp()

      Ex(ESEQ(seq(

        MOVE(l, unEx(left)),
        MOVE(r, unEx(rigth)),
        //        EXP (CALL (NAME (namedlabel "_compString"),List(CONST opc,TEMP t1, TEMP t2)),
        MOVE(rt, Frame.RV))
        ,
        rt))

    case _ => throw new UnsupportedOperationException()
  }

  override def binOpExp(op: Oper, left: InteropExp, rigth: InteropExp): InteropExp = {
    val t1 = Temp.newTemp()
    val t2 = Temp.newTemp()
    val rt = Temp.newTemp()

    // optimización en la siguiente etapa?
    Ex(ESEQ(
      seq(
        MOVE(t1, unEx(left)),
        MOVE(t2, unEx(rigth)),
        MOVE(rt, BINOP(op, t1, t2)))
      , TEMP(rt)))
  }

  override def stringExp(s: String): InteropExp = {
    val l = Temp.newLabel()

    Frame.STRING(l, s) // it should add it to global string -what-ever-

    Ex(Tree.NAME(l))
  }

  override def recordExp(expressions: List[InteropExp]): InteropExp = {
    val rt = Temp.newTemp()

    val values = for ((exp, index) <- expressions.zipWithIndex) yield
      MOVE(MEM(BINOP(PLUS, rt, index * Frame.WS)), unEx(exp))

    Ex(ESEQ(seq(
      EXP(extenalCall("_newRecord", expressions.length)),
      MOVE(rt, Frame.RV),
      values
    ), rt))
  }

  override def arrayExp(size: InteropExp, init: InteropExp) = {
    val rt = Temp.newTemp()

    Ex(ESEQ(
      seq(
        EXP(extenalCall("_allocArray", unEx(size), unEx(init))),
        MOVE(rt, Frame.RV))
      , rt))
  }

  override def seqExp(expressions: List[InteropExp]): InteropExp = {
    val init = expressions.init.map(unNx)

    Ex(ESEQ(
      seq(init),
      unEx(expressions.head)))
  }

  override def assignExp(left: InteropExp, right: InteropExp): InteropExp = {
    Nx(MOVE(unEx(left), unEx(right)))
  }

}


object Translate {
  val translate = new MyTranslate()

  def get(): Translate = translate
}

