package se.kth.cda.compiler.dataflow.encode

import io.circe.{Encoder, Json}
import io.circe.syntax._
import se.kth.cda.arc.syntaxtree.{MergeOp, Type}
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.arc.syntaxtree.Type._

object EncodeType {
  implicit val encodeType: Encoder[Type] = {
    case I8              => Json.obj(("Scalar", "I8".asJson))
    case I16             => Json.obj(("Scalar", "I16".asJson))
    case I32             => Json.obj(("Scalar", "I32".asJson))
    case I64             => Json.obj(("Scalar", "I64".asJson))
    case U8              => Json.obj(("Scalar", "I8".asJson))
    case U16             => Json.obj(("Scalar", "I16".asJson))
    case U32             => Json.obj(("Scalar", "I32".asJson))
    case U64             => Json.obj(("Scalar", "I64".asJson))
    case F32             => Json.obj(("Scalar", "I32".asJson))
    case F64             => Json.obj(("Scalar", "I64".asJson))
    case UnitT           => Json.obj(("Scalar", "Unit".asJson))
    case StringT         => Json.obj(("Scalar", "String".asJson))
    case Vec(elemTy)     => Json.obj(("Vec", Json.obj(("elem_ty", elemTy.asJson(encodeType)))))
    case Struct(elemTys) => Json.obj(("Vec", Json.obj(("elem_tys", elemTys.asJson))))
    case Dict(keyTy, valueTy) =>
      Json.obj(
        ("Dict",
         Json.obj(
           ("key_ty", keyTy.asJson(encodeType)),
           ("value_ty", valueTy.asJson(encodeType)),
         )))
    case Appender(elemTy, _) =>
      Json.obj(
        ("Appender",
         Json.obj(
           ("elem_ty", elemTy.asJson(encodeType)),
         )))
    case Merger(elemTy, opTy, _) =>
      Json.obj(
        ("Merger",
         Json.obj(
           ("elem_ty", elemTy.asJson(encodeType)),
           ("op_ty", opTy.asJson(encodeMergeOp)),
         )))
    case GroupMerger(keyTy, valueTy, _) =>
      Json.obj(
        ("GroupMerger",
         Json.obj(
           ("key_ty", keyTy.asJson(encodeType)),
           ("value_ty", valueTy.asJson(encodeType)),
         )))
    case DictMerger(keyTy, valueTy, opTy, _) =>
      Json.obj(
        ("DictMerger",
         Json.obj(
           ("key_ty", keyTy.asJson(encodeType)),
           ("value_ty", valueTy.asJson(encodeType)),
           ("op_ty", opTy.asJson(encodeMergeOp)),
         )))
    case VecMerger(elemTy, opTy, _) =>
      Json.obj(
        ("VecMerger",
         Json.obj(
           ("elem_ty", elemTy.asJson(encodeType)),
           ("op_ty", opTy.asJson(encodeMergeOp)),
         )))
  }

  implicit val encodeMergeOp: Encoder[MergeOp] = {
    case MergeOp.Max => Json.obj(("MergeOp", "max".asJson))
    case MergeOp.Min => Json.obj(("MergeOp", "max".asJson))
    case MergeOp.Product => Json.obj(("MergeOp", "*".asJson))
    case MergeOp.Sum => Json.obj(("MergeOp", "+".asJson))
  }


}