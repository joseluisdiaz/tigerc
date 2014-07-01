spackage tiger

import tiger.Temp.Label
import tiger.Tree._

/**
 * Created by jose on 6/22/14.
 */
trait CanonComponent {

  val canon:Canon

  trait Canon {

    def linearize(stm: Stm): List[Stm]

    def basicBlocks(stms: List[Stm]): (List[List[Stm]], Temp.Label)

    def traceSchedule(stms: List[List[Stm]], label: Temp.Label): List[Stm]

  }

  class MyCanon extends Canon {

    /*
     * Linarize
     */
    val NOP = EXP(CONST(0))

    case class RichStm(e: Stm) {
      def %(that: Stm): Stm = (this.e, that) match {
        case (EXP(CONST(_)), _) => that
        case (_, EXP(CONST(_))) => this.e
        case (_, _) => SEQ(this.e, that)
      }
    }

    implicit def enrichStm(x: Stm) = RichStm(x)

    private def immutable(expr: TREE): Boolean = expr match {
      case CONST(_) => true
      case TEMP(Frame.FP) => true
      case BINOP(_, x, y) => immutable(x) && immutable(y)
      case _ => false
    }

    def commute(t1: TREE, t2: TREE): Boolean = (t1, t2) match {
      case (EXP(CONST(_)), _) => true
      case (_, NAME(_)) => true
      case (_, CONST(_)) => true
      case (EXP(CALL(NAME("_checkIndexArray"), _)), _) => true
      case (EXP(CALL(NAME("_checkNil"), _)), _) => true
      case (EXP(x), y) => immutable(x) || immutable(y)
      case (_,_) => false
    }

    def doStm(stm: Stm): Stm = stm match {
      /* Seq */
      case SEQ(s1, s2) => doStm(s1) % doStm(s2)

      /* Jump */
      case JUMP(e, labs) => reorderStm(List(e), { case List(x) => JUMP(x, labs)})

      /* CJump */
      case CJUMP(o, e1, e2, t, f) => reorderStm(List(e1, e2), { case List(x1, x2) => CJUMP(o, x1, x2, t, f)})

      /* Move */
      case MOVE(TEMP(t), CALL(e, el)) => reorderStm(e :: el, { case x :: xs => MOVE(TEMP(t), CALL(x, xs))})
      case MOVE(TEMP(t), b) => reorderStm(List(b), { case List(x) => MOVE(TEMP(t), x)})
      case MOVE(MEM(e), b) => reorderStm(List(e, b), { case List(x1, x2) => MOVE(MEM(x1), x2)})
      case MOVE(ESEQ(s, e), b) => doStm(SEQ(s, MOVE(e, b)))

      /* Exp */
      case EXP(CALL(e, el)) => reorderStm(e :: el, { case x :: xs => EXP(CALL(x, xs))})
      case EXP(e) => reorderStm(List(e), { case List(x) => EXP(x)})

      /* Default */
      case _ => reorderStm (List(), _ => stm)
    }



    def doExp(exp: Expr): (Stm, Expr) = exp match {
      /* Binop */
      case BINOP(p, a, b) => reorderExpr(List(a, b), { case List(x1, x2) => BINOP(p, x1, x2)})

      /* Mem */
      case MEM(a) => reorderExpr(List(a), { case List(x) => MEM(x)})

      case ESEQ(s, e) => {
        val stms1 = doStm(s)
        val (stms2, e2) = doExp(e)
        (stms1 % stms2, e2)
      }

      /* Call */
      case CALL(e, el) => reorderExpr(e :: el, { case x :: xs => CALL(x, xs)})

      /* Default */

      case _ => reorderExpr(List(), _ => exp)
    }


    def reorder(zs: List[Expr]): (Stm, List[Expr]) = zs match {
      case CALL(n, args) :: xs =>
        val t = Temp.newTemp()
        reorder(ESEQ(MOVE(TEMP(t), CALL(n, args)), TEMP(t)) :: xs)

      case x :: xs =>
        val (stms1, e) = doExp(x)
        val (stms2, exprs) = reorder(xs)

        if (commute(stms2, e)) {
          (stms1 % stms2, e :: exprs)
        }
        else {
          val t = Temp.newTemp()
          (stms1 % MOVE(TEMP(t), e) % stms2, TEMP(t) :: exprs)
        }

      case List() => (NOP, List())

    }

    def reorderExpr(el: List[Expr], build: (List[Expr] => Expr)): (Stm, Expr) = {
      val (stms, el2) = reorder(el)
      (stms, build(el2))
    }

    def reorderStm(el: List[Expr], build: (List[Expr] => Stm)): Stm = {
      val (stms, el2) = reorder(el)
      stms % build(el2)
    }

    def linear(s: Stm, l: List[Stm]): List[Stm] = s match {
      case SEQ(a, b) => linear(a, linear(b, l))
      case _ => s :: l
    }

    override def linearize(stm: Stm): List[Stm] = linear(doStm(stm), List())

    override def basicBlocks(stms: List[Stm]): (List[List[Stm]], Label) = {
      var done = Temp.newLabel()

      def blocks(bs:List[Stm], blist:List[List[Stm]]):List[List[Stm]] = {

        def next(l: List[Stm], thisblock: List[Stm]):List[List[Stm]] = l match {
          case (s@JUMP(_,_)) :: xs => endblock(xs, s :: thisblock)
          case (s@CJUMP(_,_,_,_,_)) :: xs => endblock(xs, s :: thisblock)
          case stms@LABEL(lab) :: xs => next(JUMP(NAME(lab), List(lab)) :: stms, thisblock)
          case s :: rest => next(rest, s :: thisblock)
          case List() => next(List(JUMP(NAME(done), List(done))), thisblock)
        }

        def endblock(l: List[Stm], thisblock: List[Stm]) = blocks(l, thisblock.reverse :: blist)

        bs match {
          case (head@LABEL(_)) :: tail => next(tail, List(head))
          case List() => blist.reverse
          case _ => blocks(LABEL(Temp.newLabel())::bs, blist)
        }

      }

      (blocks(stms, List()), done)
    }


    override def traceSchedule(stms: List[List[Stm]], label: Label): List[Stm] = ???

  }

}
