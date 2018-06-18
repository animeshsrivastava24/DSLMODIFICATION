/**
  * Created by omariott on 16/02/17.
  */
/**
  * Noise reduced, symbols added, scala implicits used to optimize the code by Animesh on 18 June 2018
  */

package fr.univ_lille.cristal.emeraude.n2s3.dsl //package name

import fr.univ_lille.cristal.emeraude.n2s3.core.{NeuronModel, Property} // Imported NeuronModel and Property from the Core
import fr.univ_lille.cristal.emeraude.n2s3.features.builder.NeuronGroupRef // Imported NeuronGroupReference from builder

// This class below creates a neuron group and provides size, model and parameters

class NeuronGroupDSL(identifier: String)(implicit network: N2S3SimulationDSL )
 {
  implicit val sizeDefault : Int = 28 // added the implicit default value for the setNumberOfNeurons method for the MNIST dataset
  val id: String = identifier 
  var neuronGroup: Option[NeuronGroupRef] = None // this variable stores all the information by using the get property and a associated function with it
  private var neuronParameters: Seq[(Property[_], _)] = Seq()
  private var neuronModel: NeuronModel = _

/**Properties of the NeuronGroupDSL **/

//Changes made now we have to type "network hasNeunronGroup "group_1" ofSize ofModel LIF"
// Added a default size to the size variable [done]

//The return type is just the type of `this`. The `this`-reference has type NeuronGroupDSL so we can just drop the return type
  def ofSize(implicit size: Int)= {
    this.neuronGroup.get.setNumberOfNeurons(size)
    this
  }
// Give user the opportunity to choose the type of available models by the help of pattern matching
/**Neuron Model:
1. Linear leaky integrate-and-fire model
2. FitzHugh-Nagumo model
3. Izhikevich model
**/

//Changes made now we have to type "network hasNeunronGroup "group_1" ofSize 28 ofModel LIF"
//Added a default model type of LIF  [pending]
  def ofModel(model: NeuronModel) = {
    this.neuronModel = model
    this.neuronGroup.get.setNeuronModel(model, this.neuronParameters)
    this
  }
//we can define a default property the neuron group model here
//"group_1" hasParameters (MembranePotentialThreshold -> 35.millivolts) 
  def hasParameters(properties: (Property[_], _)*)= {
    this.neuronParameters = properties
    this.neuronGroup.get.setNeuronModel(this.neuronModel, properties)
    this
  }
//we can add shape of neurons, set actor policy, set the synchronizer values [pending]
  def toNeuronGroup: (String, NeuronGroupRef) = {
    (this.id, this.neuronGroup.get)
  }
}

class InputNeuronGroupDSL(identifier: String)(implicit network: N2S3SimulationDSL) extends NeuronGroupDSL(identifier) {
  this.neuronGroup = Some(network.toN2S3.createInput(network.getInput))
}

class BasicNeuronGroupDSL(identifier: String)(implicit network: N2S3SimulationDSL) extends NeuronGroupDSL(identifier) {
  this.neuronGroup = Some(network.toN2S3.createNeuronGroup(identifier, 1))
}

/**

Changes made and results inferred
1. Add implicit value to the size
2. Changed the function name `modeling` to `ofModel` & `withParameters` to  `hasParameters`
3. Dropped the return type of the function because they contained `this` in the end if error comes then add `this.type` as the return type
4. Private val in case class leads to either to weird compilation error or ClassCastException so can't make the case classes
**/
