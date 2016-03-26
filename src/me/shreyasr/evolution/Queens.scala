package me.shreyasr.evolution

import scala.collection.mutable.ArrayBuffer

class Queens {
  import Queens._

  var population: Array[Genotype] = new Array(populationSize)

  def init(): Unit = {
    (0 until populationSize).foreach(population(_) = new Genotype())
  }

  def run(): Int = {
    var iterations = 0
    while (!isDone()) {
      iterations += 1
      val parents: Array[Genotype] = population
        .shuffle.slice(0, parentPoolCount)
        .sortBy(_.fitness).slice(0, parentPairCount*2)
      val children = new ArrayBuffer[Genotype](parentPairCount * 2)
      parents.sliding(2, 2).slice(0, parentPairCount).foreach((parents) => {
//        println(parents.mkString(" | "))
        children.append(
          parents(0).recombine(parents(1)),
          parents(1).recombine(parents(0))
        )
      })
      children.foreach(g => if(util.Random.nextDouble() < 0.8) g.mutate())
      population = (population ++ children).sortBy(_.fitness).swapRandom(randomSwaps).slice(0, populationSize)
//      println(population.foldLeft(0)(_ + _.fitness) + " " + population.mkString(" | "))
//      println(children.mkString(" | "))
//      print(population.foldLeft(0)(_ + _.fitness) + " ")
    }
    iterations
  }

  def isDone(): Boolean = {
    population.sortBy(_.fitness).head.fitness == 0
  }
}
object Queens {

  val populationSize = 100
  val parentPairCount = 3
  val parentPoolCount = 15
  val randomSwaps = 10
  val boardSize = 8

  class Genotype(val arr: Array[Int]) {

    def this() = {
      this(util.Random.shuffle(1 to boardSize).toArray)
    }

    def recombine(otherParent: Genotype): Genotype = {
      val cutIndex = util.Random.nextInt(boardSize)
      val childVals = arr.slice(0, cutIndex).toBuffer
      otherParent.arr.foreach((i) => {
        if (!childVals.contains(i)) {
          childVals.append(i)
        }
      })
      new Genotype(childVals.toArray)
    }

    def mutate(): Genotype = {
      arr.swapRandom(1)
      this
    }

    def fitness: Int = {
      var fitness: Int = 0
      arr.indices.foreach({
        (i) => if (checkCheck(i)) fitness += 1
      })
      fitness
    }

    private def checkCheck(i: Int): Boolean = {
      for (j <- i+1 until arr.length) {
        if (j-i == (arr(j)-arr(i)).abs) {
          return true
        }
      }
      false
    }

    override def toString: String = {
      fitness + ": " + arr.mkString("[", ", ", "]")
    }
  }
}
