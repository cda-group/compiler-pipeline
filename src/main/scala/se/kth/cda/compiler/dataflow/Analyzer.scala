package se.kth.cda.compiler.dataflow

import se.kth.cda.arc.syntaxtree.AST.Expr
import se.kth.cda.arc.syntaxtree.AST.ExprKind._
import se.kth.cda.arc.syntaxtree.ASTUtils._
import se.kth.cda.arc.syntaxtree.Type
import se.kth.cda.arc.syntaxtree.Type.Builder.{StreamAppender, Windower}
import se.kth.cda.arc.syntaxtree.Type.Struct
import se.kth.cda.compiler.Utils.fix
import se.kth.cda.compiler.dataflow.NodeKind.{Sink, Source, Task, Window}

object Analyzer {

  implicit class LambdaAnalyser(val self: Lambda) extends AnyVal {

    // Calculates the ratio between input and output elements
    def selectivity: Float = {
      fix[Expr, Float] { f => expr =>
        if (expr.ty.isArcType && expr.ty.isBuilderType) {
          expr.kind match {
            case e: Merge       => 1 + f(e.builder)
            case _: For         => Float.MaxValue
            case e: If          => (f(e.onTrue) + f(e.onFalse)) / 2
            case e: Select      => (f(e.onTrue) + f(e.onFalse)) / 2
            case e: Let         => f(e.value) + f(e.body)
            case e: Application => e.args.map(f).sum + f(e.expr)
            case e: Lambda      => f(e.body)
            case e: MakeStruct  => e.elems.map(f).sum
            case _              => 0
          }
        } else {
          0
        }
      }(self.body)
    }

    // Calculates the number of output channels
    def fanOut: Int = {
      fix[Type, Int] { f =>
        {
          case _: StreamAppender => 1
          case _: Windower       => 1
          case ty: Struct        => ty.elemTys.map(f).sum
          case _                 => 0
        }
      }(self.body.ty)
    }

    def mutationFree: Boolean = {
      val Vector(_, _, elem) = self.params
      fix[Expr, Boolean] { f => expr =>
        if (expr.ty.isArcType && expr.ty.isBuilderType) {
          expr.kind match {
            case e: Merge =>
              e.value.kind match {
                case id: Ident if id.symbol.name == elem.symbol.name => f(e.builder)
                case _                                               => false
              }
            case e: For         => f(e.builder) && f(e.body)
            case e: If          => f(e.onTrue) && f(e.onFalse)
            case e: Select      => f(e.onTrue) && f(e.onFalse)
            case e: Let         => f(e.value) && f(e.body)
            case e: Application => e.args.forall(f) && f(e.expr)
            case e: Lambda      => f(e.body)
            case e: MakeStruct  => e.elems.forall(f)
            case _              => true
          }
        } else {
          true
        }
      }(self.body)
    }

  }

  implicit class ExprAnalyzer(val self: Expr) extends AnyVal {

    // Finds out if an expression is side-effect free
    def isPure: Boolean = {
      fix[Expr, Boolean] { f => expr =>
        expr.kind match {
          case _: CUDF => false
          case _ =>
            val children = expr.children()
            if (children.isEmpty) {
              true
            } else {
              expr.children().forall(f)
            }
        }
      }(self)
    }

  }

  implicit class NodeAnalyzer(val self: Node) extends AnyVal {

    def numSiblings: Int =
      self.kind match {
        case _: Task   => self.numSuccessors - 1
        case _: Sink   => self.numSuccessors - 1
        case _: Window => self.numSuccessors - 1
        case _         => 0
      }

    def numSuccessors: Int =
      self.kind match {
        case kind: Source => kind.successors.length
        case kind: Task   => kind.successors.length
        case kind: Window => kind.successors.length
        case _            => 0
      }

    def hasSiblings: Boolean = numSiblings > 0

  }

}
