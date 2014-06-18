package tiger

import java.util.logging.Level

import tiger.Abs._
import tiger.Frame.{InFrame, externalCall}
import tiger.Temp.Label
import tiger.Tree.{BINOP, CJUMP, CONST, ESEQ, EXP, JUMP, LABEL, MEM, MOVE, NAME, SEQ, TEMP, _}
import tiger.Types.{Ty, _}


trait TranslateComponent {

  val translate: Translate

  trait Translate {

    type Access
    type Expression
    type Level <: BaseLevel

    trait BaseLevel {
      def preWhile(): Unit

      def postWhile(): Unit
    }

    def newLevel(parent: Option[Level], name: Temp.Label): Level

    def formals(l: Level): List[Access]

    def allocLocal(l: Level, esc: Boolean): Access

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

    def whileExp(condition: Expression, body: Expression, currentLevel: Level): Expression

    def ifThenExp(test: Expression, then: Expression): Expression

    def ifThenElseExp(test: Expression, then: Expression, elsa: Expression): Expression

    def forExp(lo: Expression, hi: Expression, v: Expression, body: Expression): Expression

    def callExp(name: Temp.Label, params: List[Expression], level: BaseLevel, isProc: Boolean, extern: Boolean): Expression

    def letExp(decsExp: List[Expression], body: Expression): Expression

    def breakExp(l: Level): Option[Expression]

    // Declarations
    def varDec(acc: Access, currentLevel: Level, init: Expression): Expression

    def functionDec(body: Expression, currentLevel: Level, isProc: Boolean): Expression

  }


  class MyTranslate extends Translate {

    override type Level = MyLevelImpl

    class MyLevelImpl(val parent: Option[MyLevelImpl], val frame: Frame) extends BaseLevel {

      import scala.collection.mutable

      val labels = new mutable.Stack[Temp.Label]

      def count(to: MyLevelImpl): Int = parent match {
        //      case None => op(this,z)
        case _ if this == to => 1
        case Some(x) => 1 + x.count(to)
      }


      override def preWhile(): Unit = labels.push(Temp.newLabel())

      override def postWhile(): Unit = labels

    }

    object MyLevelImpl {
      def apply(parent: Option[MyLevelImpl], frame: Frame) = new MyLevelImpl(parent, frame)
    }


    override def newLevel(parent: Option[MyLevelImpl], name: Label): MyLevelImpl =
      MyLevelImpl(parent, Frame(name, List(true))) // el primer parametro es el static link

    override def formals(l: MyLevelImpl): List[(MyLevelImpl, Frame#Access)] = l.frame.formals map ((l, _))

    override def allocLocal(l: MyLevelImpl, esc: Boolean): (MyLevelImpl, Frame#Access) = (l, l.frame.allocLocal(esc))



    override type Access = (Level, Frame#Access)


    /**
     * Seman interoperability
     */
    sealed case class InteropExp()

    case class Ex(e: Tree.Exp) extends InteropExp

    case class Nx(s: Tree.Stm) extends InteropExp

    case class Cx(genstm: (Temp.Label, Temp.Label) => Tree.Stm) extends InteropExp

    /**
     * Sequences
     * @param stms
     * @return
     */

    def seq(stms: List[Stm]): Stm = if (stms.isEmpty) EXP(CONST(0)) else stms reduce SEQ

    def seq(s1: Stm, l: List[Stm]) = seq(s1 :: l)

    def seq(s1: Stm, s2: Stm, l: List[Stm]) = seq(s1 :: s2 :: l)

    def seq(s1: Stm, s2: Stm, s3: Stm, l: List[Stm]) = seq(s1 :: s2 :: s3 :: l)

    def seq(s1: Stm, s2: Stm, s3: Stm, s4: Stm, l: List[Stm]) = seq(s1 :: s2 :: s3 :: s4 :: l)

    def seq(s1: Stm, s2: Stm, s3: Stm, s4: Stm, s5: Stm, l: List[Stm]) = seq(s1 :: s2 :: s3 :: s4 :: s5 :: l)

    def seq(s: Stm*) = seq(s.toList)


    //  implicit def stm2list(stm: Stm): List[Stm] = List(stm)
    //
    //  def seq(stms: List[Stm]*): Stm = {
    //    val flat = stms.flatten
    //    if (flat.isEmpty) EXP(CONST(0)) else stms reduce SEQ
    //  }


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

    override type Expression = InteropExp

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


    /**
     * Interaction with SEMAN
     */

    // variables
    override def simpleVar(acc: (MyLevelImpl, Frame#Access), currentLevel: MyLevelImpl): InteropExp = {
      val SL = InFrame(-1)

      // static link calculation
      def calcStaticLink(n: Int): Tree.Exp = n match {
        case 0 => Frame.exp(acc._2, TEMP(Frame.FP))
        case _ => Frame.exp(SL, calcStaticLink(n - 1))
      }

      Ex(calcStaticLink(0))
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


    override def relOpExp(op: Oper, ty: Types.Ty, left: InteropExp, rigth: InteropExp): InteropExp = ty match {
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
        EXP(externalCall("_newRecord", expressions.length)),
        MOVE(rt, Frame.RV),
        values
      ), rt))
    }

    override def arrayExp(size: InteropExp, init: InteropExp) = {
      val rt = Temp.newTemp()

      Ex(ESEQ(
        seq(
          EXP(externalCall("_allocArray", unEx(size), unEx(init))),
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

    override def whileExp(condition: InteropExp, body: InteropExp, currentLevel: MyLevelImpl): InteropExp = {
      val f = currentLevel.labels.head
      val t = Temp.newLabel()
      val i = Temp.newLabel()

      Nx(
        seq(LABEL(i),
          unCx(condition)(t, f),
          LABEL(t),
          unNx(body),
          JUMP(NAME(i), List(i)),
          LABEL(f)))
    }

    override def ifThenExp(test: InteropExp, then: InteropExp): InteropExp = {
      val f = Temp.newLabel()
      val t = Temp.newLabel()

      Nx(
        seq(
          unCx(test)(t, f),
          LABEL(t),
          unNx(then),
          LABEL(f)))

    }

    override def ifThenElseExp(test: InteropExp, then: InteropExp, elsa: InteropExp): InteropExp = {
      val f = Temp.newLabel()
      val t = Temp.newLabel()
      val end = Temp.newLabel()
      val rt = Temp.newTemp()

      Ex(ESEQ(seq(
        unCx(test)(t, f),
        LABEL(t),
        MOVE(rt, unEx(then)),
        JUMP(NAME(end), List(end)),
        LABEL(f),
        MOVE(rt, unEx(elsa)),
        LABEL(end))
        , rt))
    }

    override def forExp(lo: InteropExp, hi: InteropExp, v: InteropExp, body: InteropExp): InteropExp = ???

    override def callExp(name: Label, params: List[InteropExp], level: BaseLevel, isProc: Boolean, extern: Boolean): InteropExp = {
      val SL = MEM(CONST(0))

      // prepend static link
      val firstParam = if (extern) List(SL) else List()

      val paramsEx = firstParam ++ (params map unEx)


      if (isProc)
        Nx(EXP(externalCall(name, paramsEx)))
      else {
        val rt = Temp.newTemp()
        Ex(ESEQ(seq(
          EXP(externalCall(name, paramsEx)),
          MOVE(rt, Frame.RV))
          , rt))
      }
    }

    override def letExp(decsExp: List[Expression], body: Expression): Expression = {
      if (decsExp.isEmpty) Ex(unEx(body))
      else Ex(ESEQ(seq(decsExp map unNx), unEx(body)))
    }

    override def breakExp(l: MyLevelImpl): Option[Expression] = {
      if (l.labels.isEmpty)
        return None

      val jmp = l.labels.head

      Some(Nx(JUMP(NAME(jmp), List(jmp))))
    }

    // Declarations
    override def varDec(acc: (MyLevelImpl, Frame#Access), currentLevel: MyLevelImpl, init: Expression): Expression = {
      Nx(MOVE(unEx(simpleVar(acc, currentLevel)), unEx(init)))
    }

    override def functionDec(body: InteropExp, currentLevel: MyLevelImpl, isProc: Boolean): Expression = {
      val bodyTree = if (isProc) unNx(body) else MOVE(Frame.RV, unEx(body))

      //    currentLevel.frame.procEntryExit(body)
      //
      //    procEntryExit1(currentLevel.frame, body)
      //
      //    val () = procEntryExit{body=Nx body', level=l}
      //    in	Ex(CONST 0) end
      Ex(0)
    }

  }

}
