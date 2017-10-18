
package chiselknn

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester

import scala.io.Source
import scalaknn.NearestNeighbours


class HardwareNearestNeighbours(val fixedType: FixedPoint,
                                val k: Int,
                                val keySize: Int,
                                val dataY: Seq[String],
                                val dataX: Array[Array[Double]]) extends Module {
  val io = IO(new Bundle {
    var x   = Input(Vec(keySize, fixedType))
    val out = Output(fixedType)
  })

  private val tabHash0 = dataX.map(_.map(_.F(fixedType.getWidth.W, fixedType.binaryPoint)))

  private val distances = tabHash0.indices.map { ind1 =>
    val dist: FixedPoint = io.x.zip(tabHash0(ind1)).foldLeft(0.F(fixedType.binaryPoint)) { case (accum, (x, t)) =>
      accum + ((x - t) * (x - t))
    }
    dist
  }
  val topKClasses = distances.zipWithIndex.sortBy(_._1).take(k).map { case (dist, idx) => dataY(idx) }

  val label = topKClasses.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
  io.out := label(0).toByte.toDouble.F(fixedType.getWidth.W, fixedType.binaryPoint)
}


object HardwareNearestNeighboursDriver {
  val euclideanDist = (v1: Array[Double], v2: Array[Double]) => v1.zip(v2).map(x => math.pow((x._1 - x._2), 2)).sum

  //function to transform the data into a couple of list of  Doubles and a string
  def line2Data(line: String): (List[Double], String) = {
    val elts = line.split(",")
    val y = elts.last
    val x = elts.dropRight(1).map(_.toDouble).toList
    (x, y)
  }


  def main(args: Array[String]): Unit = {

    //loading the data from file
    val data = Source.fromFile("ionosphere.data.txt").getLines().map(x => line2Data(x)).toList

    val outputs = data.map(_._2).toSeq //take the last column of strings and convert to a sequence.
    val inputs = data.map(_._1).toArray //convert the list of list double in to an array of list double.

    println("The output size is" + outputs.size)


    val dataX = inputs.map(_.toArray).take(300) //we convert the array of list double into an array of array double and take just 300 from the inputs data.
    val dataY = outputs.take(300) //we take just 300 of the sequence of strings
    val keySize = dataX(2).length //to keySize we assign the length of the second array of dataX
    println(keySize) //we check the length
    val fixedWidth = 64
    val binaryPoint = 32
    val k = 5

    def makeModule: Module = {
      new HardwareNearestNeighbours(
        FixedPoint(fixedWidth.W, binaryPoint.BP),
        k, keySize, dataY, dataX
      )
    }
    iotesters.Driver.execute(args, makeModule _) { c =>
      new PeekPokeTester(c) {
        val traininputs = inputs.take(300) //use the first 300 data points of our data sets
        val trainoutputs = outputs.take(300)
        val myNN = new NearestNeighbours(k = 4, dataX = traininputs.map(x => x.toArray), dataY = trainoutputs, euclideanDist)

        var correct = 0
        (300 to 350).foreach { exampleId =>
          val pred = myNN.predict(inputs(exampleId).toArray)
          val target = outputs(exampleId)
          if (pred == target) correct += 1
        }

        println("The accuracy is\n" + correct.toDouble / (300 to 350).length)

      }
    }
  }
}

