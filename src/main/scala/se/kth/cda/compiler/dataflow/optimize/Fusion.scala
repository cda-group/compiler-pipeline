package se.kth.cda.compiler.dataflow.optimize

import se.kth.cda.arc.syntaxtree.AST.{Expr, ExprKind}
import se.kth.cda.compiler.dataflow.{ChannelKind, Node, NodeKind}
import se.kth.cda.compiler.dataflow.NodeKind.{Sink, Source, Task, Window}
import se.kth.cda.compiler.dataflow.TaskKind.FlatMap
import se.kth.cda.compiler.dataflow.Analyzer._
import se.kth.cda.compiler.dataflow.ChannelKind.{Local, Remote}
import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.Type.Function
import se.kth.cda.compiler.Utils._
import se.kth.cda.arc.syntaxtree.ASTUtils._

object Fusion {

  implicit class Fusion(val node: Node) extends AnyVal {
    // TODO: Value ranges

    // Fusion takes two operators (FlatMaps) and fuses them together.
    //
    // Notes:
    // - Operator specialization can occur either before or after fusion
    // - In order to fuse two operators, they must be side-effect free (no CUDFs)
    // - Fusion can be either vertical or horizontal
    //
    // Example:
    //
    // Let us fuse two operators.
    //   let source1 = result(for(source0, sink0, |b,i,e| merge(b, e+5)));
    //   let source2 = result(for(source1, sink1, |b,i,e| merge(b, e-5)));
    //
    // -------------------------------------------------
    // CASE 1: Operator specialization turns the functions into Maps:
    //   |e| e+5
    //   |e| e-5
    //
    // Which fuse to:
    //   |e| (|e| e-5)((|e| e+5)(e))
    //
    // |b,i,e| (|b,i,e| merge(b, e-5))(b, e+5)
    // |e| (|e| e-5)(e+5)
    //
    //
    // -------------------------------------------------
    // Case 2: Without specialization, the functions are FlatMaps:
    //   |e| let b = appender; let res = merge(b, e+5); result(res)
    //   |e| let b = appender; let res = merge(b, e-5); result(res)
    //
    // The second function gets converted to a flatmap over vectors:
    //   |v| result(for(v, appender, |b,i,e| merge(b, e-5));
    //
    // -------------------------------------------------
    // Case 3: Without specialization, the functions are FlatMaps.
    //         We optimize by replacing every merge with an application
    //
    //   |b,e| let b1 = merge(b, e+5); let b2 = merge(b1, e+5); b2
    //   |b,e| let b1 = merge(b, e-5); let b2 = merge(b1, e-5); b2
    //
    //   |b,e| let b1 = (|b,e| let b1 = merge(b, e-5); let b2 = merge(b1, e-5); b2)(b, e+5);
    //         let b2 = (|b,e| let b1 = merge(b, e-5); let b2 = merge(b1, e-5); b2)(b1, e+5);
    //         b2
    //
    //
    // |b,i,e| f1(f2(e))
    //                      Sink
    //                     /
    // Source---Task---Task
    //                     \
    //                      Sink

    // Fuses an operator with its predecessor
    def fuseHorizontally: Node = {
      node.kind match {
        case self: Sink   => self.predecessor.fuseHorizontally
        case self: Window => self.predecessor.fuseHorizontally
        case self: Task =>
          self.predecessor.kind match {
            // For now, only allow fusing horizontally if there are no siblings
            case pred: Task if pred.weldFunc.is_pure && !self.predecessor.has_siblings =>
              // Each successor of self should point to the predecessor of self
              self.successors.foreach { channel =>
                val succ = channel match {
                  case channel: Local  => channel.node
                  case channel: Remote => channel.node
                }
                succ.kind match {
                  case succ: Sink   => succ.predecessor = self.predecessor
                  case succ: Task   => succ.predecessor = self.predecessor
                  case succ: Window => succ.predecessor = self.predecessor
                  case _: Source    => ???
                }
              }
              pred.successors = self.successors
              pred.weldFunc = fuseWeldFuncs(pred.weldFunc, self.weldFunc)
              self.predecessor.id = s"${self.predecessor.id}_${node.id}"
              self.removed = true
            case _ => ()
          }
          self.predecessor.fuseHorizontally
        case _: Source => node
      }
    }

    // Fuses an operator with its sibling
    def fuseVertically: Node = {
      ???
    }

    private def fuseWeldFuncs(weldFunc1: Expr, weldFunc2: Expr): Expr = {
      (weldFunc1.kind, weldFunc2.kind) match {
        case (Lambda(params1, body1), Lambda(_, body2)) =>
          Lambda(
            params1,
            fix[Expr, Expr] { f =>
              {
                expr =>
                  if (expr.ty.isArcType && expr.ty.isBuilderType) {
                    expr.kind match {
                      case e: Merge       => Application(weldFunc2, Vector(e.builder, e.value)).toExpr(expr.ty)
                      case e: If          => If(e.cond, f(e.onTrue), f(e.onFalse)).toExpr(expr.ty)
                      case e: Select      => Select(e.cond, f(e.onTrue), f(e.onFalse)).toExpr(expr.ty)
                      case e: For         => For(e.iterator, e.builder, f(e.body)).toExpr(expr.ty)
                      case e: Lambda      => Lambda(e.params, f(e.body)).toExpr(expr.ty)
                      case e: Application => Application(f(expr), e.args.map(arg => f(arg))).toExpr(expr.ty)
                      case e: Let         => Let(e.symbol, e.bindingTy, f(e.value), f(e.body)).toExpr(expr.ty)
                      case _              => expr
                    }
                  } else {
                    expr
                  }
              }
            }(body1)
          ).toExpr(Function(params1.map(_.ty), body2.ty))
        case _ => ???
      }
    }
  }
}
