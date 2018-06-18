/*Importing the required packages*/
import java.util.NoSuchElementException
import fr.univ_lille.cristal.emeraude.n2s3.core.{NeuronConnection, SynapseBuilder} /*We can add an & method here for importing both with an & option  */
import fr.univ_lille.cristal.emeraude.n2s3.features.builder.NeuronGroupRef
import fr.univ_lille.cristal.emeraude.n2s3.features.builder.connection.{ConnectionPolicy, ConnectionTypeBuilder} /*We can add an & method here for importing both with an & option  */
import fr.univ_lille.cristal.emeraude.n2s3.features.logging.graph.SynapsesWeightGraphBuilderRef

/*defining the package name for the following module*/
package fr.univ_lille.cristal.emeraude.n2s3.dsl
/*Top level defintion*/
object N2S3DSLImplicits {

/*Here a line is repeated we will try to follow the DRY principle here to make some good changes*/
  implicit def stringToNeuronGroupRef(id: String)(implicit network: N2S3SimulationDSL): NeuronGroupRef = {
    network.getNeuronGroupRef(id) /*optional dots */
  }

  implicit def stringToNeuronGroup(id: String)(implicit network: N2S3SimulationDSL): NeuronGroupDSL = {
    try {
      network.getNeuronGroupById(id) /*optional dots */
    }
    catch {
      case _: NoSuchElementException => basicNeuronGroup(id) 
    }
  }
 
  def inputNeuronGroup(id: String)(implicit network: N2S3SimulationDSL): InputNeuronGroupDSL = new InputNeuronGroupDSL(id)  /*lambda methods*/
  def basicNeuronGroup(id: String)(implicit network: N2S3SimulationDSL): BasicNeuronGroupDSL = new BasicNeuronGroupDSL(id)  /*lambda methods*/
  implicit def stringToConnectionBuilder(id: String)(implicit network: N2S3SimulationDSL): ConnectionBuilder = new ConnectionBuilder(id, network) /*lambda methods to define the implicits*/


  class ConnectionBuilder(originId: String, network: N2S3SimulationDSL) {
    var connectionStrategy : ConnectionTypeBuilder = _

    var destinationId: String = _
    var connectionPolicy : ConnectionPolicy = _
    var neuronConnection : NeuronConnection = _ 

  /*lambda methods*/
    def connectsTo(destinationId: String): ConnectionBuilder = { 
      this.destinationId = destinationId
      this
    }

 /*lambda methods*/
    def using(connectionType: ConnectionTypeBuilder): ConnectionBuilder = {
      this.connectionStrategy = connectionType
      this
    }
 /*lambda methods*/

    def withSynapse(synapseBuilder : SynapseBuilder): Unit ={
      network.getNeuronGroupRef(this.originId)
          .connectTo(
            network.getNeuronGroupRef(this.destinationId),
            this.connectionStrategy.createConnection(() => synapseBuilder.createSynapse))
    }

 /*lambda methods*/

    def withPolicy(connectionType: ConnectionPolicy): Unit = {
      this.connectionPolicy = connectionType
    }

 /*lambda methods*/
    def usingSynapse(synapse: NeuronConnection): Unit = {
      this.neuronConnection = synapse
    }
  }
 /*lambda methods*/
  def observeConnectionsBetween(originId: String, destinationId: String)(implicit network: N2S3SimulationDSL): SynapsesWeightGraphBuilderRef = {
    network.addSynapsesWeightGraph(originId, destinationId)
    network.toN2S3.addNetworkObserver(new SynapsesWeightGraphBuilderRef(stringToNeuronGroupRef(originId), stringToNeuronGroupRef(destinationId)))
  }

}
