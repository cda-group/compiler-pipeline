package se.kth.cda.compiler.dataflow

object IdGenerator {

  def resetGlobal(): Unit = {
    DFGId.globalIdCounter = 0
    ChannelId.globalIdCounter = 0
    TaskId.globalIdCounter = 0
    SourceId.globalIdCounter = 0
    SinkId.globalIdCounter = 0
    WindowId.globalIdCounter = 0
  }

  object DFGId {
    var globalIdCounter = 0
    def generate: String = {
      val id = globalIdCounter
      globalIdCounter += 1
      s"dfg_$id"
    }
  }

  object NodeId {
    var globalIdCounter = 0
    def newGlobalOrd: Int = {
      val id = globalIdCounter
      globalIdCounter += 1
      id
    }
    def fuse(pred: String, succ: String): String = {
      s"${pred}_$succ"
    }
  }

  object ChannelId {
    var globalIdCounter = 0
    def newGlobalId: String = {
      val id = globalIdCounter
      globalIdCounter += 1
      s"channel_$id"
    }
  }

  object TaskId {
    var globalIdCounter = 0
    def newGlobalId: String = {
      val id = globalIdCounter
      globalIdCounter += 1
      s"task_$id"
    }
  }

  object SourceId {
    var globalIdCounter = 0
    def newGlobalId: String = {
      val id = globalIdCounter
      globalIdCounter += 1
      s"source_$id"
    }
  }

  object SinkId {
    var globalIdCounter = 0
    def newGlobalId: String = {
      val id = globalIdCounter
      globalIdCounter += 1
      s"sink_$id"
    }
  }

  object WindowId {
    var globalIdCounter = 0
    def newGlobalId: String = {
      val id = globalIdCounter
      globalIdCounter += 1
      s"window_$id"
    }
  }

  object StructId {
    def from(pred: String, succ: String): String = {
      s"struct_${pred}_${succ}_"
    }
    var localIdCounter = 0 // Resets for each node
    def nextLocal(nodeId: String): String = {
      val id = localIdCounter
      localIdCounter += 1
      s"$nodeId$id"
    }
    var globalIdCounter = 0
    def newGlobalId: String = {
      val id = globalIdCounter
      globalIdCounter += 1
      s"struct_${id}_"
    }
    def resetLocal(): Unit = localIdCounter = 0
  }

}
