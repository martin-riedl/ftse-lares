package ftse.transformations.laresflat

import ftse.transformations.AbstrTransformer
import ftse.simulation.laresflat.LFReachTypes
import ftse.simulation.laresflat.LFReachTypes._

/**
 * This trait provides a base implementation of a transformation o the ReachResult and its
 * belonging ReachExtension. It is used to fulfil the stackable trait pattern.
 */
trait AbstractReachExtensionModifier extends 
	AbstrTransformer[(ReachResult, ReachExtension), (ReachResult, ReachExtension)] 
	with LFReachTypes {

  /**
   * This method provides the ability to transform the ReachExtension in a proper way.
   * It has to be implemented by sub classes / sub traits.
   */
  def extensionTransformation(obj: (ReachStructure, ReachExtension)): ReachExtension = {
    obj._2
  }
  
  
  /**
   * This method provides the base implementation for the stackable trait pattern.
   * It leaves the argument unchanged.
   */
  override def transform(obj: (ReachResult, ReachExtension)): (ReachResult, ReachExtension) = {
    obj
  }
}