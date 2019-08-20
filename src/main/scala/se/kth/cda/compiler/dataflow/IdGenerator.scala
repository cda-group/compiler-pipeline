package se.kth.cda.compiler.dataflow

object IdGenerator {

  def reset(): Unit = {
    DFGId.idCounter = 0
    ChannelId.idCounter = 0
    TaskId.idCounter = 0
    SourceId.idCounter = 0
    SinkId.idCounter = 0
    WindowId.idCounter = 0
  }

  object DFGId {
    var idCounter = 0
    def generate: String = {
      val id = idCounter
      idCounter += 1
      s"dfg_$id"
    }
  }

  object ChannelId {
    var idCounter = 0
    def newId: String = {
      val id = idCounter
      idCounter += 1
      s"channel_$id"
    }
  }

  object TaskId {
    var idCounter = 0
    def newId: String = {
      val id = idCounter
      idCounter += 1
      s"task_$id"
    }
  }

  object SourceId {
    var idCounter = 0
    def newId: String = {
      val id = idCounter
      idCounter += 1
      s"source_$id"
    }
  }

  object SinkId {
    var idCounter = 0
    def newId: String = {
      val id = idCounter
      idCounter += 1
      s"sink_$id"
    }
  }

  object WindowId {
    var idCounter = 0
    def newId: String = {
      val id = idCounter
      idCounter += 1
      s"window_$id"
    }
  }


}