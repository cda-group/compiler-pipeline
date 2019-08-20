package se.kth.cda.compiler

import org.scalatest.{FunSuite, Matchers}
import se.kth.cda.compiler.dataflow.IdGenerator

import scala.language.implicitConversions

class ParserTests extends FunSuite with Matchers {

  test("normalize") {
    val input =
      """
        |{
        |  "nodes": [
        |    {
        |      "id": "source_0",
        |      "kind": {
        |        "Source": {
        |          "format": "CSV",
        |          "kind": {
        |            "LocalFile": {
        |              "path": "input.txt"
        |            }
        |          }
        |        }
        |      }
        |    },
        |    {
        |      "id": "sink_0",
        |      "kind": {
        |        "Sink": {
        |          "format": "CSV",
        |          "kind": {
        |            "LocalFile": {
        |              "path": "output.txt"
        |            }
        |          }
        |        }
        |      }
        |    }
        |  ],
        |  "timestamp_extractor": 0,
        |  "arc_code": "|source_0: stream[i64], sink_0: streamappender[?]|\nlet operator_1 = result(for(source_0, windower[unit,appender[?],?,vec[?]](\n  |ts,windows,state| { [ts/60L], () },\n  \t|wm,windows,state| { result(for(windows, appender, |b,i,e| if(i < wm, merge(b, i), b))), () },\n  \t|agg| result(agg)\n), |sb,si,se| merge(sb, se)));\nfor(operator_1, sink_0, |sb,si,se| merge(sb,\nlet obj102 = (se);\nlet obj105 = (result(\n    for(\n        obj102,\n        merger[i64, +],\n        |b: merger[i64, +], i: i64, e: i64| \n            merge(b, e)\n    )\n));\nlet obj106 = (len(obj102));\nlet obj107 = (obj105 / obj106);\nlet obj108 = (result(\n    for(obj102, \n        appender[i64], \n        |b: appender[i64], i: i64, e: i64| \n            merge(b, e / obj107)\n    )\n));\nobj108\n))\n"
        |}
      """.stripMargin

    //println(input)
    val output = Compiler.compile(input)
    //println(output)
  }

  test("touchpad") {
    val input =
      """
        |{
        |  "nodes": [
        |    {
        |      "id": "source_0",
        |      "kind": {
        |        "Source": {
        |          "format": "CSV",
        |          "kind": {
        |            "Socket": {
        |              "addr": "127.0.0.1:8000"
        |            }
        |          }
        |        }
        |      }
        |    },
        |    {
        |      "id": "sink_0",
        |      "kind": {
        |        "Sink": {
        |          "format": "CSV",
        |          "kind": {
        |            "Socket": {
        |              "addr": "127.0.0.1:9000"
        |            }
        |          }
        |        }
        |      }
        |    }
        |  ],
        |  "timestamp_extractor": 0,
        |  "arc_code": "|source_0: stream[{i64,f64,f64,f64}], sink_0: streamappender[?]|\n# preprocess\nlet operator_1 = result(for(source_0, streamappender[?], |sb,si,se| if(\nlet obj101 = (se);\nlet obj102 = (obj101.$2);\nlet obj103 = (obj102 < f64(0.0));\nlet obj104 = (obj101.$2);\nlet obj105 = (obj104 > f64(1.0));\nlet obj106 = (obj103 && obj105);\nobj106\n, merge(sb, se), sb)));\n# extract timestamp\nlet operator_2 = result(for(operator_1, streamappender[?], |sb,si,se| merge(sb,\nlet obj107 = (se);\nlet obj108 = ({ obj107.$1,obj107.$2,obj107.$3 });\nobj108\n)));\n# extract key\nlet operator_3 = result(for(operator_2, streamappender[?], |sb,si,se| merge(sb,\nlet obj110 = (se);\nlet obj111 = (obj110.$0);\nlet obj112 = (obj111 / f64(25));\nlet obj113 = (i64(obj112));\nlet obj114 = (obj110.$1);\nlet obj115 = (obj114 / f64(25));\nlet obj116 = (i64(obj115));\nlet obj117 = (obj110.$2);\nlet obj118 = ({ obj113,obj116 });\nlet obj119 = ({ obj118,obj117 });\nobj119\n)));\n# create tumbling window\nlet operator_4 = result(for(operator_3, windower[unit,appender[?],?,vec[?]](\n  |ts,windows,state| { [ts/60L], () },\n  \t|wm,windows,state| { result(for(windows, appender, |b,i,e| if(i < wm, merge(b, i), b))), () },\n  \t|agg| result(agg)\n), |sb,si,se| merge(sb, se)));\n# sum up pressures\nfor(operator_4, sink_0, |sb,si,se|\nlet groups = tovec(result(for(se, groupmerger, |b,i,e| merge(b,e))));\nlet keyvals = result(for(groups, appender, |gb,gi,ge| merge(gb,{ge.$0,\nlet obj122 = (ge.$1);\nlet obj125 = (result(\n    for(obj122, \n        appender[f64], \n        |b: appender[f64], i: i64, e: f64| \n            merge(b, e + 0.1)\n    )\n));\nlet obj126 = (result(\n    for(\n        obj125,\n        merger[f64, +],\n        |b: merger[f64, +], i: i64, e: f64| \n            merge(b, e)\n    )\n));\nobj126\n})));\nmerge(sb, keyvals)\n)\n"
        |}
      """.stripMargin

    //println(input)
    val output = Compiler.compile(input)
    println(output)
  }

}
