import fr.univ_lille.cristal.emeraude.n2s3.dsl.N2S3DSLImplicits._
import fr.univ_lille.cristal.emeraude.n2s3.features.builder.{N2S3, NeuronGroupObserverRef, NeuronGroupRef}
import fr.univ_lille.cristal.emeraude.n2s3.features.io.input.{InputGenerator, InputPacket, N2S3InputPacket, StreamSupport}
import scala.collection.mutable

/*defining the package name for the following module*/
package fr.univ_lille.cristal.emeraude.n2s3.dsl
/*WE can use the case classes to minimize the function and increase some efficiency*/
/*Top level defintion*/
object N2S3SimulationDSL {
  def apply(): N2S3SimulationDSL = new N2S3SimulationDSL()
}

/*WE can use the case classes to minimize the function and increase some efficiency*/
class N2S3SimulationDSL() {
  private val n2s3 = new N2S3
  private var modules: Map[String, NeuronGroupDSL] = Map()
  private var graphedConnections: mutable.MutableList[(String, String)] = new mutable.MutableList[(String, String)]()
  private var inputStream: Option[StreamSupport[InputPacket, N2S3InputPacket]] = None
  private val neuronGroups: collection.mutable.Map[String, NeuronGroupRef] = collection.mutable.Map()
  private val activeObservers: collection.mutable.Map[String, NeuronGroupObserverRef] = collection.mutable.Map()
/*lambda methods and map and reduce functions in scala*/
  def addInputModule(inputModule: InputNeuronGroupDSL): InputNeuronGroupDSL = {
    this.modules += (inputModule.id -> inputModule)
    val neuronGroup = inputModule.toNeuronGroup
    this.neuronGroups.put(neuronGroup._1, neuronGroup._2)
    inputModule
  }
/*lambda methods and map and reduce functions in scala*/
  def hasNeuronGroup(module: NeuronGroupDSL): NeuronGroupDSL = {
    this.modules += (module.id -> module)
    val neuronGroup = module.toNeuronGroup
    this.neuronGroups.put(neuronGroup._1, neuronGroup._2)
    module
  }
/*lambda methods and map*/
  def hasInputNeuronGroup(id: String)(implicit network : N2S3SimulationDSL): InputNeuronGroupDSL = this.addInputModule(inputNeuronGroup(id))

  def hasInput(input: StreamSupport[_ <: InputPacket, N2S3InputPacket]): N2S3SimulationDSL = {
    this.inputStream = Some(input.asInstanceOf[StreamSupport[InputPacket, N2S3InputPacket]])
    this
  }

  implicit def toN2S3: N2S3 = this.n2s3

  def getNeuronGroupById(id: String): NeuronGroupDSL = {
    this.modules(id)
  }

  def getNeuronGroupRef(ident: String): NeuronGroupRef = {
    this.neuronGroups(ident)
  }

  def addSynapsesWeightGraph(originId: String, destinationId: String): Unit = {
    this.graphedConnections += originId -> destinationId
  }

  def addObserver[V <: NeuronGroupObserverRef](ref: V): V = n2s3.addNetworkObserver(ref)

  def trainOn(stream: InputGenerator[_ <: InputPacket]): Unit = {
    val networkStream = this.inputStream.get
    networkStream.clean()
    networkStream.append(stream.asInstanceOf[InputGenerator[InputPacket]])
    this.n2s3.runAndWait()
  }

  def testOn(stream: InputGenerator[_ <: InputPacket]): Unit = {
    val networkStream = this.inputStream.get
    networkStream.clean()
    networkStream.append(stream.asInstanceOf[InputGenerator[InputPacket]])
    this.n2s3.layers.foreach(G => G.fixNeurons())
    this.n2s3.runAndWait()
    this.n2s3.layers.foreach(G => G.unfixNeurons())
  }

  def getObserverOn(s: String): NeuronGroupObserverRef = {
    this.activeObservers(s)
  }

  def getInput: StreamSupport[_ <: InputPacket, N2S3InputPacket] ={
    this.inputStream.get
  }
}
