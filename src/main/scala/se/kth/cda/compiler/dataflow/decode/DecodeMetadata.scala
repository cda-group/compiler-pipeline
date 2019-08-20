package se.kth.cda.compiler.dataflow.decode

import se.kth.cda.compiler.dataflow.NodeKind.{Task, Window}
import se.kth.cda.compiler.dataflow._

object DecodeMetadata {
  import io.circe._
  import io.circe.generic.semiauto._

  implicit val metadataDecoder: Decoder[Metadata] =
    (cursor: HCursor) =>
      for {
        nodes <- cursor.get[List[Node]]("nodes")
        timestamp_extractor <- cursor.get[Int]("timestamp_extractor")
        age <- cursor.get[String]("arc_code")
      } yield Metadata(nodes, timestamp_extractor, age)

  implicit val nodeDecoder: Decoder[Node] =
    (cursor: HCursor) =>
      for {
        id <- cursor.get[String]("id")
        kind <- cursor.get[NodeKind]("kind")
      } yield Node(id = id, kind = kind)
  implicit val nodeKindDecoder: Decoder[NodeKind] = io.circe.generic.semiauto.deriveDecoder[NodeKind]

  implicit val sourceDecoder: Decoder[NodeKind.Source] =
    (cursor: HCursor) =>
      for {
        format <- cursor.get[String]("format").map{
          case "CSV" => Format.CSV
          case "UTF8" => Format.UTF8
          case "JSON" => Format.JSON
        }
        kind <- cursor.get[SourceKind]("kind")
      } yield NodeKind.Source(format = format, kind = kind)
  implicit val sourceKindDecoder: Decoder[SourceKind] = deriveDecoder

  implicit val localFileSourceDecoder: Decoder[SourceKind.LocalFile] =
    (cursor: HCursor) =>
      for {
        path <- cursor.get[String]("path")
      } yield SourceKind.LocalFile(path)

  implicit val socketSourceDecoder: Decoder[SourceKind.Socket] =
    (cursor: HCursor) =>
      for {
        host <- cursor.get[String]("host")
        port <- cursor.get[Long]("port")
      } yield SourceKind.Socket(host, port)

  implicit val sinkDecoder: Decoder[NodeKind.Sink] =
    (cursor: HCursor) =>
      for {
        format <- cursor.get[String]("format").map{
          case "CSV" => Format.CSV
          case "UTF8" => Format.UTF8
          case "JSON" => Format.JSON
        }
        kind <- cursor.get[SinkKind]("kind")
      } yield NodeKind.Sink(format = format, kind = kind)
  implicit val sinkKindDecoder: Decoder[SinkKind] = deriveDecoder

  implicit val localFileSinkDecoder: Decoder[SinkKind.LocalFile] =
    (cursor: HCursor) => for { path <- cursor.get[String]("path") } yield SinkKind.LocalFile(path)

  implicit val socketSinkDecoder: Decoder[SinkKind.Socket] =
    (cursor: HCursor) =>
      for {
        host <- cursor.get[String]("host")
        port <- cursor.get[Long]("port")
      } yield SinkKind.Socket(host, port)

  implicit val formatDecoder: Decoder[Format] = deriveDecoder

  implicit val windowDecoder: Decoder[Window] = (cursor: HCursor) => for {
    _ <- cursor.get[String]("name")
  } yield Window(null, null, null, null, null, null, null)

  implicit val taskDecoder: Decoder[Task] = (cursor: HCursor) => for {
    _ <- cursor.get[String]("name")
  } yield Task(null, null, null, null)
}
