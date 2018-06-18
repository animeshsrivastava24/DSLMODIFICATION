/**
  * Created by omariott on 16/02/17.
  */
/**
  * Noise reduced, symbols added, scala implicits used to optimize the code by Animesh on 18 June 2018
  */

package fr.univ_lille.cristal.emeraude.n2s3.dsl


import java.util.NoSuchElementException
import fr.univ_lille.cristal.emeraude.n2s3.core.{NeuronConnection, SynapseBuilder}
import fr.univ_lille.cristal.emeraude.n2s3.features.builder.NeuronGroupRef
import fr.univ_lille.cristal.emeraude.n2s3.features.builder.connection.{ConnectionPolicy, ConnectionTypeBuilder}
import fr.univ_lille.cristal.emeraude.n2s3.features.logging.graph.SynapsesWeightGraphBuilderRef
object N2S3DSLImplicits {
  implicit def stringToNeuronGroupRef(id: String)(implicit network: N2S3SimulationDSL): NeuronGroupRef = {
    network.getNeuronGroupRef(id)
  }
  implicit def stringToNeuronGroup(id: String)(implicit network: N2S3SimulationDSL): NeuronGroupDSL = {
    try {
      network.getNeuronGroupById(id)
    }
    catch {
      case _: NoSuchElementException => basicNeuronGroup(id)
    }
  }
  def inputNeuronGroup(id: String)(implicit network: N2S3SimulationDSL): InputNeuronGroupDSL = new InputNeuronGroupDSL(id)
  def basicNeuronGroup(id: String)(implicit network: N2S3SimulationDSL): BasicNeuronGroupDSL = new BasicNeuronGroupDSL(id)
  implicit def stringToConnectionBuilder(id: String)(implicit network: N2S3SimulationDSL): ConnectionBuilder = new ConnectionBuilder(id, network)

  class ConnectionBuilder(originId: String, network: N2S3SimulationDSL) {
    var connectionStrategy : ConnectionTypeBuilder = _
    var destinationId: String = _
    var connectionPolicy : ConnectionPolicy = _
    var neuronConnection : NeuronConnection = _
//"group_1" <--> "group_2" X  FullConnection -- SimplifiedSTDP
    def <-->(destinationId: String)= {
      this.destinationId = destinationId
      this
    }
//"group_1" <--> "group_2" X  FullConnection -- SimplifiedSTDP
    def X(connectionType: ConnectionTypeBuilder)= {
      this.connectionStrategy = connectionType
      this
    }
//"group_1" <--> "group_2" X  FullConnection -- SimplifiedSTDP
    def --(synapseBuilder : SynapseBuilder) {
      network.getNeuronGroupRef(this.originId)
          .connectTo(
            network.getNeuronGroupRef(this.destinationId),
            this.connectionStrategy.createConnection(() => synapseBuilder.createSynapse))
    }
    def withPolicy(connectionType: ConnectionPolicy){this.connectionPolicy = connectionType}
    def usingSynapse(synapse: NeuronConnection){this.neuronConnection = synapse}
    }
  def observeConnectionsBetween(originId: String, destinationId: String)(implicit network: N2S3SimulationDSL): SynapsesWeightGraphBuilderRef = {
    network.addSynapsesWeightGraph(originId, destinationId)
    network.toN2S3.addNetworkObserver(new SynapsesWeightGraphBuilderRef(stringToNeuronGroupRef(originId), stringToNeuronGroupRef(destinationId)))
  }
}
/**

Changes made
1. Changed the function name `connectsTo` to `<-->` as the two outward headed arrows show connection between two entitites.
2. Dropped the return type of the function because they contained `this` in the end if error comes then add `this.type` as the return type.
3. Changed the function name 'using' to 'X' as it was showing the connection and in Neural network the connections between layers are often using lines.
4. Changed the 'withSynapse' to '--' as it defines the space connection between pre-synaptic and post-synaptic neuron and space between them.
5. Functions had return type of Unit so just dropped return type and = sign
**/






