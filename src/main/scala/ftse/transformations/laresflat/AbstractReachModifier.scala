package ftse.transformations.laresflat

import ftse.transformations.AbstrTransformer
import ftse.simulation.laresflat.LFReachTypes._
import ftse.simulation.laresflat.LFReachTypes

/**
 * This trait is used as a base implementation for transforming results from the reachability analysis.
 */
trait AbstractReachModifier extends AbstrTransformer[ReachResult, ReachResult] with LFReachTypes {
  
  /**
   * very base implementation of the only method of AbstrTransformer: it leaves the argument unchanged.
   */
  override def transform(obj: ReachResult): ReachResult = {
    obj
  }
}