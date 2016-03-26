package me.shreyasr.evolution

object Main {

  def main(args: Array[String]) {
    while (true) {
      val evolutionaryAlgorithm = new Queens
      evolutionaryAlgorithm.init()
      val iterations = evolutionaryAlgorithm.run()
//      println()
      println(iterations)
    }
  }
}
