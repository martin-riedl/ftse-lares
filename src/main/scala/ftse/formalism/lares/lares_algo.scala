package ftse.formalism.lares

import ftse.formalism.arith._
import ArithExpr_metamodel._
import LARES_metamodel._

/**
 * Provides some helper methods/algorithms for LARES 
 */
trait LaresAlgo extends ArithExpr_algo {

  /**
   * Generates from a given distribution type a Distribution with weight or rate 1
   * 
   * @param d A distribution type 
   * @return The generated distribution  
   */
  def obtainNeutralDistribution(d : DistributionType) : Distribution= {
    if (d==ExponentialType) return Exponential1
    if (d==DiracType) return Dirac1
    else Dirac1
  } 
    
  /**
   * Composing Distributions
   * 
   * following the composition semantics of two distributions in LARES 
   */
  def compose(a: Distribution, b : Distribution) = (a,b) match {
    case (Exponential(r1),Exponential(r2)) => Exponential(ArithMult(r1,r2))
    case (Dirac(w1),Dirac(w2)) => Dirac(ArithMult(w1,w2))
  }	
  
  /**
   * Evaluate a distribution 
   * @param a A distribution 
   * @return Either an Integer or a Double value
   */
  def eval(a : Distribution) = a match {
    case Dirac(w) => w.eval()
    case Exponential(r) => r.eval()
    
  }
}

