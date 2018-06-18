import fr.univ_lille.cristal.emeraude.n2s3.core.{NeuronModel, Property}
import fr.univ_lille.cristal.emeraude.n2s3.features.builder.NeuronGroupRef


/*defining the package name for the following module*/
package fr.univ_lille.cristal.emeraude.n2s3.dsl
/*This package has 3 class defintion*/
/*Top level defintion*/


/*WE can use the case classes to minimize the function and increase some efficiency*/



class NeuronGroupDSL(identifier: String)(implicit network: N2S3SimulationDSL) {

  val id: String = identifier
  var neuronGroup: Option[NeuronGroupRef] = None
  private var neuronParameters: Seq[(Property[_], _)] = Seq()
  private var neuronModel: NeuronModel = _


  def ofSize(size: Int): NeuronGroupDSL = {
    this.neuronGroup.get.setNumberOfNeurons(size)
    this
  }

  def modeling(model: NeuronModel): NeuronGroupDSL = {
    this.neuronModel = model
    this.neuronGroup.get.setNeuronModel(model, this.neuronParameters)
    this
  }

  def withParameters(properties: (Property[_], _)*): NeuronGroupDSL = {
    this.neuronParameters = properties
    this.neuronGroup.get.setNeuronModel(this.neuronModel, properties)
    this
  }

  def toNeuronGroup: (String, NeuronGroupRef) = {
    (this.id, this.neuronGroup.get)
  }
}


/*The next two class extends the NeuronGroupDSL Class*/
class InputNeuronGroupDSL(identifier: String)(implicit network: N2S3SimulationDSL) extends NeuronGroupDSL(identifier) {
  this.neuronGroup = Some(network.toN2S3.createInput(network.getInput))
}



class BasicNeuronGroupDSL(identifier: String)(implicit network: N2S3SimulationDSL) extends NeuronGroupDSL(identifier) {
  this.neuronGroup = Some(network.toN2S3.createNeuronGroup(identifier, 1))
}
