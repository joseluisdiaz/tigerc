package tiger

import scala.collection.mutable.LinkedList
import tiger.Abs._
import tiger.Frame.{Frag, InFrame, InReg, externalCall}
import tiger.Temp.Label
import tiger.Tree.{BINOP, CJUMP, CONST, ESEQ, EXP, JUMP, LABEL, MEM, MOVE, NAME, SEQ, TEMP, _}
import tiger.Types.{Ty, _}


trait TranslateComponent {

  val translate: Translate

  trait Translate {


    /* level definition */
    type Access
    type Expression
    type Level <: BaseLevel

     trait BaseLevel {
      def preLoop(): Unit

      def postLoop(): Unit
    }

    /* interaction with */
    def fragments(): List[Frag]

    def newLevel(parent: Option[Level], name: Temp.Label): Level

    def formals(l: Level): List[Access]

    def allocLocal(l: Level, esc: Boolean): Access

    def allocFormal(l: Level, esc: Boolean): Access

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

    def forExp(lo: Expression, hi: Expression, v: Expression, body: Expression, currentLevel: Level): Expression

    def callExp(name: Temp.Label, params: List[Expression], caller: Level, callee:Level, isProc: Boolean, extern: Boolean): Expression

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

      def countTop(): Int = parent match {
        case None => 0
        case Some(x) => 1 + x.countTop()
      }

      override def preLoop(): Unit = labels.push(Temp.newLabel())

      override def postLoop(): Unit = labels.pop()

      override def toString() = s"(frame: $frame)"

    }

    object MyLevelImpl {
      def apply(parent: Option[MyLevelImpl], frame: Frame) = new MyLevelImpl(parent, frame)
    }

    val frags = scala.collection.mutable.ListBuffer.empty[Frag]

    override def fragments = frags.toList

    override def newLevel(parent: Option[MyLevelImpl], name: Label): MyLevelImpl =
      MyLevelImpl(parent, Frame(name, List(true))) // el primer parametro es el static link

    override def formals(l: MyLevelImpl): List[(MyLevelImpl, Frame#Access)] = l.frame.formals map ((l, _))

    override def allocLocal(l: MyLevelImpl, esc: Boolean): (MyLevelImpl, Frame#Access) = (l, l.frame.allocLocal(esc))

    override def allocFormal(l: MyLevelImpl, esc: Boolean): (MyLevelImpl, Frame#Access) = {
      println("allocFormal from translate: " + l.frame.name)
      val access = l.frame.allocFormal(esc)
      println(access)

      (l, access)
    }

    override type Access = (Level, Frame#Access)

    /**
     * Seman interoperability
     */
    sealed abstract class InteropExp()

    case class Ex(e: Tree.Expr) extends InteropExp

    case class Nx(s: Tree.Stm) extends InteropExp

    case class Cx(genstm: (Temp.Label, Temp.Label) => Tree.Stm) extends InteropExp


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
          seq(MOVE(TEMP(r), CONST(0)),
            genstm(t, f),
            LABEL(t),
            MOVE(TEMP(r), CONST(1)),
            LABEL(f)),
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
      case _ => error("transBinOp no debería pasar")
    }

    implicit def transRelOp(op: Oper) = op match {
      case EqOp => EQ
      case NeqOp => NE
      case LtOp => LT
      case LeOp => LE
      case GtOp => GT
      case GeOp => GE
      case _ => error("transRelOp no debería pasar")
    }


    /*
     * Interaction with SEMAN
     */

    // variables
    override def simpleVar(acc: (MyLevelImpl, Frame#Access), currentLevel: MyLevelImpl): InteropExp = {
      val (level, access) = acc

      val t = Temp.newTemp()

      val result = access match {
        case f@InFrame(i) => {
          val n = currentLevel.countTop() - level.countTop()

          if (n != 0) {
            val offset = 1 to (n - 1) map { x => MOVE(t, MEM(BINOP(PLUS, t, CONST(Frame.SL))))}

            ESEQ(
              seq(
                MOVE(TEMP(t), MEM(BINOP(PLUS, Frame.FP, CONST(Frame.SL)))),
                offset.toList),

              MEM(BINOP(MINUS, t, CONST(Math.abs(i)))))
          }
          else {
            f.exp()
          }
        }

        case InReg(l) => TEMP(l)
      }

      Ex(result)
    }

    override def subscriptVar(array: InteropExp, index: InteropExp): InteropExp = {
      val arrayEx = unEx(array)
      val indexEx = unEx(index)

      val arrayTemp = Temp.newTemp()
      val indexTemp = Temp.newTemp()

      Ex(
        ESEQ(seq(
          MOVE(arrayTemp, arrayEx),
          MOVE(indexTemp, indexEx),
          EXP(externalCall("_checkIndexArray", arrayEx, indexEx))),

          MEM(BINOP(PLUS, arrayTemp, BINOP(MUL, indexTemp, Frame.WS)))))
    }

    override def fieldVar(record: InteropExp, offset: Int): InteropExp = {
      val recordTemp = Temp.newTemp()

      Ex(
        ESEQ(seq(
          MOVE(recordTemp, unEx(record)),
          EXP(externalCall("_checkNil", recordTemp))),

          MEM(BINOP(PLUS, recordTemp, offset * Frame.WS))))

    }

    // Expressions
    override def unitExp(): InteropExp = Nx(EXP(0))

    override def nilExp(): InteropExp = Ex(0)

    override def intExp(n: Int): InteropExp = {

      Ex(n)
    }

    override def relOpExp(op: Oper, ty: Types.Ty, left: InteropExp, rigth: InteropExp): InteropExp = ty match {
      case STRING() =>
        Cx((t: Temp.Label, f: Temp.Label) => CJUMP(op,
          ESEQ(EXP( externalCall("_stringCompare",unEx(left),unEx(rigth))  ), TEMP(Frame.RV)),
          0,
          t,f)  )

      case _ =>
        Cx((t: Temp.Label, f: Temp.Label) => CJUMP(op, unEx(left), unEx(rigth), t, f))
    }

    override def binOpExp(op: Oper, left: InteropExp, rigth: InteropExp): InteropExp = {
      val t1 = Temp.newTemp()
      val t2 = Temp.newTemp()
      val rt = Temp.newTemp()

      Ex(
        ESEQ(
          seq(
            MOVE(t1, unEx(left)),
            MOVE(t2, unEx(rigth)),
            MOVE(rt, BINOP(op, t1, t2))),

          TEMP(rt)))
    }

    override def stringExp(s: String): InteropExp = {
      val l = Temp.newLabel()

      frags += Frame.STRING(l, s)

      Ex(Tree.NAME(l))
    }

    override def recordExp(expressions: List[InteropExp]): InteropExp = {
      val rt = Temp.newTemp()

      val values = for ((exp, index) <- expressions.zipWithIndex) yield
        MOVE(MEM(BINOP(PLUS, index * Frame.WS, rt)), unEx(exp))

      Ex(
        ESEQ(
          seq(
            EXP(externalCall("_allocRecord", expressions.length)),
            MOVE(rt, Frame.RV),
            values),

          rt))
    }

    override def arrayExp(size: InteropExp, init: InteropExp) = {
      val rt = Temp.newTemp()

      Ex(
        ESEQ(
          seq(
            EXP(externalCall("_allocArray", unEx(size), unEx(init))),
            MOVE(rt, Frame.RV))

        , rt))
    }

    override def seqExp(expressions: List[InteropExp]): InteropExp = {
      val init = expressions.init.map(unNx)

      Ex(
        ESEQ(
          seq(init),

          unEx(expressions.last)))
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

      val d = seq(
        unCx(test)(t, f),
        LABEL(t),
        unNx(then),
        LABEL(f))

      Nx(d)

      //      Nx(
//        seq(
//          unCx(test)(t, f),
//          LABEL(t),
//          unNx(then),
//          LABEL(f)))

    }

    override def ifThenElseExp(test: InteropExp, then: InteropExp, elsa: InteropExp): InteropExp = {
      val f = Temp.newLabel()
      val t = Temp.newLabel()
      val end = Temp.newLabel()
      val rt = Temp.newTemp()

      val uncx = unCx(test)(t, f)
      val unExThen = unEx(then)
      val unExElsa = unEx(elsa)

      Ex(
        ESEQ(
          seq(
            uncx,
            LABEL(t),
            MOVE(rt, unExThen),
            JUMP(NAME(end), List(end)),
            LABEL(f),
            MOVE(rt, unExElsa),
            LABEL(end))
            , rt))
    }

    override def forExp(lo: InteropExp, hi: InteropExp, v: InteropExp, body: InteropExp, currentLevel: MyLevelImpl): InteropExp = {
      val max = Temp.newTemp()
      val loopVar = unEx(v)
      val loop = Temp.newLabel()
      val end = currentLevel.labels.head

      Nx (seq(
          MOVE(loopVar, unEx(lo)),
          MOVE(TEMP(max), unEx(hi)),
          CJUMP (LE, loopVar, TEMP(max),loop, end),
          LABEL(loop),
          unNx(body),
          MOVE (loopVar, BINOP(PLUS, loopVar, CONST(1))),
          CJUMP (GT,loopVar,TEMP(max),end,loop),
          LABEL(end)))

    }

    override def callExp(name: Label, params: List[InteropExp], caller: MyLevelImpl, callee:MyLevelImpl, isProc: Boolean, extern: Boolean): InteropExp = {

      val lcaller = caller.countTop()
      val lcallee = callee.countTop()

      println(s"caller: ${caller.frame.name} - callee: ${callee.frame.name}")

      val SL = if (lcaller > lcallee) {
        val n = lcaller - lcallee

        val t = Temp.newTemp()

        val offset = (1 to n) map { x:Int => MOVE(t, MEM(BINOP(PLUS, CONST(Frame.SL), t ))) }

        ESEQ(
          seq(
            MOVE(TEMP(t), MEM(BINOP(PLUS, Frame.FP, CONST(Frame.SL)))),
            offset.toList),

          t)

      } else if (lcaller == lcallee) {
        // SL -> SL

        MEM(BINOP(PLUS, Frame.FP, CONST(Frame.SL)))

      } else {
        // SL -> FP

        TEMP(Frame.FP)
      }

      // prepend static link
      val firstParam = if (!extern) List(SL) else List()

      val paramsEx = firstParam ++ (params map unEx)

      if (isProc) {
        Nx(EXP(externalCall(name, paramsEx)))
      }
      else {
        val rt = Temp.newTemp()
        Ex(
          ESEQ(
            seq(
              EXP(externalCall(name, paramsEx)),
              MOVE(rt, Frame.RV))

            ,rt))
      }
    }

    override def letExp(decsExp: List[Expression], body: Expression): Expression = {
      if (decsExp.isEmpty)
        Ex(unEx(body))
      else
        Ex(ESEQ(seq(decsExp map unNx), unEx(body)))
    }

    override def breakExp(l: MyLevelImpl): Option[Expression] = {
      if (l.labels.isEmpty)
        return None

      val jmp = l.labels.head

      Some(Nx(JUMP(NAME(jmp), List(jmp))))
    }

    //
    override def varDec(acc: (MyLevelImpl, Frame#Access), currentLevel: MyLevelImpl, init: Expression): Expression = {
      Nx(MOVE(unEx(simpleVar(acc, currentLevel)), unEx(init)))
    }

    override def functionDec(body: InteropExp, currentLevel: MyLevelImpl, isProc: Boolean): Expression = {

      val bodyTree = if (isProc) unNx(body) else MOVE(Frame.RV, unEx(body))

      val bodyTree1 = currentLevel.frame.procEntryExit1(bodyTree)

      procEntryExit(currentLevel, Nx(bodyTree1))

      Ex(CONST(0))
    }

    def procEntryExit(level: MyLevelImpl, body: Expression):Unit = {
      val bodyProc = Frame.PROC(unNx(body), level.frame)

      frags += bodyProc
    }

  }

}
