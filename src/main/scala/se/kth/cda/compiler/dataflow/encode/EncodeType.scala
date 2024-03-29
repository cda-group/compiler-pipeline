package se.kth.cda.compiler.dataflow.encode

import io.circe.syntax._
import io.circe.{Encoder, Json}
import se.kth.cda.arc.syntaxtree.Type.Builder._
import se.kth.cda.arc.syntaxtree.Type._
import se.kth.cda.arc.syntaxtree.{MergeOp, Type}
import se.kth.cda.compiler.dataflow.IdGenerator.StructId

object EncodeType {

  implicit def encodeType(nodeId: String = StructId.newGlobalId, key: Option[Long] = None): Encoder[Type] = {
    StructId.resetLocal()
    encodeTypeRec(nodeId, key)
  }

  def encodeTypeRec(nodeId: String, key: Option[Long] = None): Encoder[Type] = {
    case Bool         => Json.obj(("Scalar", "Bool".asJson))
    case I8           => Json.obj(("Scalar", "I8".asJson))
    case I16          => Json.obj(("Scalar", "I16".asJson))
    case I32          => Json.obj(("Scalar", "I32".asJson))
    case I64          => Json.obj(("Scalar", "I64".asJson))
    case U8           => Json.obj(("Scalar", "U8".asJson))
    case U16          => Json.obj(("Scalar", "U16".asJson))
    case U32          => Json.obj(("Scalar", "U32".asJson))
    case U64          => Json.obj(("Scalar", "U64".asJson))
    case F32          => Json.obj(("Scalar", "F32".asJson))
    case F64          => Json.obj(("Scalar", "F64".asJson))
    case UnitT        => Json.obj(("Scalar", "Unit".asJson))
    case StringT      => Json.obj(("Scalar", "String".asJson))
    case Simd(elemTy) => Json.obj(("Simd", elemTy.asJson(encodeTypeRec(nodeId, key))))
    case Vec(elemTy)  => Json.obj(("Vector", Json.obj(("elem_ty", elemTy.asJson(encodeTypeRec(nodeId, key))))))
    case Struct(elemTys) =>
      Json.obj(
        ("Struct",
         Json.obj(("id", StructId.nextLocal(nodeId).asJson),
                  ("field_tys", elemTys.map(_.asJson(encodeTypeRec(nodeId, key))).asJson))))
    case Dict(keyTy, valueTy) =>
      Json.obj(
        ("Dict",
         Json.obj(("key_ty", keyTy.asJson(encodeTypeRec(nodeId, key))),
                  ("value_ty", valueTy.asJson(encodeTypeRec(nodeId, key))))))
    case Appender(elemTy, _) =>
      Json.obj(("Appender", Json.obj(("elem_ty", elemTy.asJson(encodeTypeRec(nodeId, key))))))
    case Merger(elemTy, opTy, _) =>
      Json.obj(("Merger", Json.obj(("elem_ty", elemTy.asJson(encodeTypeRec(nodeId, key))), ("op_ty", opTy.asJson))))
    case VecMerger(elemTy, opTy, _) =>
      Json.obj(("VecMerger", Json.obj(("elem_ty", elemTy.asJson(encodeTypeRec(nodeId, key))), ("op_ty", opTy.asJson))))
    case GroupMerger(keyTy, valueTy, _) =>
      Json.obj(
        ("GroupMerger",
         Json.obj(("key_ty", keyTy.asJson(encodeTypeRec(nodeId, key))),
                  ("value_ty", valueTy.asJson(encodeTypeRec(nodeId, key))))))
    case DictMerger(keyTy, valueTy, opTy, _) =>
      Json.obj(
        ("DictMerger",
         Json.obj(("key_ty", keyTy.asJson(encodeTypeRec(nodeId, key))),
                  ("value_ty", valueTy.asJson(encodeTypeRec(nodeId, key))),
                  ("op_ty", opTy.asJson))))
    case _ => ???
  }

  implicit val encodeMergeOp: Encoder[MergeOp] = {
    case MergeOp.Max     => Json.obj(("MergeOp", "max".asJson))
    case MergeOp.Min     => Json.obj(("MergeOp", "min".asJson))
    case MergeOp.Product => Json.obj(("MergeOp", "*".asJson))
    case MergeOp.Sum     => Json.obj(("MergeOp", "+".asJson))
  }

}
