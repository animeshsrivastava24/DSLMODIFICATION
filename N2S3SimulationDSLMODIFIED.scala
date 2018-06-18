/**
  * Created by omariott on 16/02/17.
  */
/**
  * Noise reduced, symbols added, scala implicits used to optimize the code by Animesh on 18 June 2018
  */

package fr.univ_lille.cristal.emeraude.n2s3.dsl
import fr.univ_lille.cristal.emeraude.n2s3.dsl.N2S3DSLImplicits._

import fr.univ_lille.cristal.emeraude.n2s3.features.builder.{N2S3, NeuronGroupObserverRef, NeuronGroupRef}
import fr.univ_lille.cristal.emeraude.n2s3.features.io.input.{InputGenerator, InputPacket, N2S3InputPacket, StreamSupport}
import scala.collection.mutable


object N2S3SimulationDSL {
  def apply(): N2S3SimulationDSL = new N2S3SimulationDSL()
}

class N2S3SimulationDSL() {
  private val n2s3 = new N2S3
  private var modules: Map[String, NeuronGroupDSL] = Map()
  private var graphedConnections: mutable.MutableList[(String, String)] = new mutable.MutableList[(String, String)]()
  private var inputStream: Option[StreamSupport[InputPacket, N2S3InputPacket]] = None
  private val neuronGroups: collection.mutable.Map[String, NeuronGroupRef] = collection.mutable.Map()
  private val activeObservers: collection.mutable.Map[String, NeuronGroupObserverRef] = collection.mutable.Map()

  def addInputModule(inputModule: InputNeuronGroupDSL): InputNeuronGroupDSL = {
    this.modules += (inputModule.id -> inputModule)
    val neuronGroup = inputModule.toNeuronGroup
    this.neuronGroups.put(neuronGroup._1, neuronGroup._2)
    inputModule
  }
//"network hasNeunronGroup "group_1" ofSize 28 ofModel LIF" changed to "network _NG "group_1" ofSize 28 ofModel LIF" 
  def _NG(module: NeuronGroupDSL): NeuronGroupDSL = {
    this.modules += (module.id -> module)
    val neuronGroup = module.toNeuronGroup
    this.neuronGroups.put(neuronGroup._1, neuronGroup._2)
    module
  }

//network hasInputNeuronGroup "input"
//network I_NG "input"
  def I_NG(id: String)(implicit network : N2S3SimulationDSL): InputNeuronGroupDSL = this.addInputModule(inputNeuronGroup(id))

//network hasInput InputMnist.Entry >> SampleToSpikeTrainConverter[Float, InputSample2D[Float]](0, 23, 150 MilliSecond, 350 MilliSecond) >> N2S3Entry
//network I InputMnist.Entry >> SampleToSpikeTrainConverter[Float, InputSample2D[Float]](0, 23, 150 MilliSecond, 350 MilliSecond) >> N2S3Entry
  def I(input: StreamSupport[_ <: InputPacket, N2S3InputPacket])= {
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

  def addSynapsesWeightGraph(originId: String, destinationId: String){
    this.graphedConnections += originId -> destinationId
  }

  def addObserver[V <: NeuronGroupObserverRef](ref: V): V = n2s3.addNetworkObserver(ref)
//trainOn to trainN means train Network
  def trainN(stream: InputGenerator[_ <: InputPacket]){
    val networkStream = this.inputStream.get
    networkStream.clean()
    networkStream.append(stream.asInstanceOf[InputGenerator[InputPacket]])
    this.n2s3.runAndWait()
  }

//the 'testOn' function name has been changed to '%N' as it tells us the percentage efficiency of test on the network
// network %N MnistFileInputStream(/path/to/MNISTTestSet, /path/to/MNISTTestLabels)
  def %N(stream: InputGenerator[_ <: InputPacket]){
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

/**

Changes made

N= network _NG=neurongroup %=test %N=testON Network  I=hasInput trainN=trainnetwork I_NG=input neuron group

1. Many functions had return type of Unit so just dropped return type and = sign
2. the 'testOn' function name has been changed to '%N' as it tells us the percentage efficiency of the network
3. "network hasNeunronGroup "group_1" ofSize 28 modeling LIF" changed to "network _NG "group_1" ofSize 28 ofModel LIF" as ++ adding the group to it ------- _NG shows neuron group
4. "network hasInputNeuronGroup "input""  changed to "network I_NG "input""   -------I_NG input neuron group
5. "network hasInput InputMnist.Entry " changed to "network I InputMnist.Entry"  -------I shows input

**/
