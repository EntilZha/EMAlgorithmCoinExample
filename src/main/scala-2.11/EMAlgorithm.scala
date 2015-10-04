/* 
 * Copyright 2015 Pedro Rodriguez. 
 * This file is subject to terms and conditions defined in LICENSE
 *
 * This code performs the EM algorithm using the coin toss example found here:
 * http://ai.stanford.edu/~chuongdo/papers/em_tutorial.pdf
 */
object EMAlgorithm {

  val flips = 10

  def binomial(p: Double, heads: Int, n: Int): Double = {
    math.pow(p, heads) * math.pow(1 - p, n - heads)
  }

  def expected_counts(p: Double, obs: Int): Experiment = {
    val heads = obs
    val tails = flips - obs
    Experiment(p * heads, tails * p)
  }

  case class Experiment(heads: Double, tails: Double)

  def em(theta_a_0: Double, theta_b_0: Double, iter_max: Int, observations: List[Int]): (Double, Double) = {
    var theta_a = theta_a_0
    var theta_b = theta_b_0
    for (i <- 0 until iter_max) {
      println(s"i= $i ThetaA: $theta_a ThetaB: $theta_b")

      // E Step

      // Calculate the probability of observation using Binomial Distribution
      val a_probs = observations.map(h => binomial(theta_a, h, 10))
      val b_probs = observations.map(h => binomial(theta_b, h, 10))
      val norms = a_probs.zip(b_probs).map{case (a, b) => a + b}
      val a_norm = a_probs.zip(norms).map{case (p, total) => p / total}
      val b_norm = b_probs.zip(norms).map{case (p, total) => p / total}

      // Compute sufficient statistics for ThetaA and ThetaB
      val a_expected_counts: Experiment = a_norm
        .zip(observations)
        .map{ case (p, obs) => expected_counts(p, obs)}
        .reduce((e0, e1) => Experiment(e0.heads + e1.heads, e0.tails + e1.tails))
      val b_expected_counts = b_norm
        .zip(observations)
        .map(t => expected_counts(t._1, t._2))
        .reduce((e0, e1) => Experiment(e0.heads + e1.heads, e0.tails + e1.tails))

      // M Step

      // Using expected counts, set parameters again
      theta_a = a_expected_counts.heads / (a_expected_counts.heads + a_expected_counts.tails)
      theta_b = b_expected_counts.heads / (b_expected_counts.heads + b_expected_counts.tails)
    }
    (theta_a, theta_b)
  }

  def main(args: Array[String]) {
    println("Running EM Algorithm")
    val observations = List(5, 9, 8, 4, 7)
    val theta = em(0.6, 0.5, 20, observations)
    println(s"Theta: $theta")
  }
}
