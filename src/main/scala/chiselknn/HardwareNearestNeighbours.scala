
package chiselknn

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester
import chisel3.util.log2Ceil
import hardwaresort.SortIndexAndTake

import scala.io.Source
import scalaknn.NearestNeighbours

/**
  * Compute the nearest neighbor for a key
  * @param fixedType
  * @param k
  * @param keySize
  * @param dataY
  * @param dataX
  */
class HardwareNearestNeighbours(val fixedType: FixedPoint,
                                val k: Int,
                                val keySize: Int,
                                val dataY: Seq[Int],
                                val dataX: Array[Array[Double]]) extends Module {
  val io = IO(new Bundle {
    val dataLoaded = Input(Bool())
    val key        = Input(Vec(keySize, fixedType))
    val out        = Output(UInt(16.W))
    val ready      = Output(Bool())
  })

  val busy = RegInit(false.B)

  val sorter = Module(new SortIndexAndTake(dataX.length, k, fixedType))
  val mfo    = Module(new MostFrequentlyOccurring(k, log2Ceil(dataX.length) + 1))

  private val tabHash0 = dataX.map(_.map(_.F(fixedType.getWidth.W, fixedType.binaryPoint)))

  private val distances = tabHash0.indices.map { ind1 =>
    val dist: FixedPoint = io.key.zip(tabHash0(ind1)).foldLeft(0.F(fixedType.binaryPoint)) { case (accum, (x, t)) =>
      accum + ((x - t) * (x - t))
    }
    dist
  }

  val predictions = VecInit(dataY.map { y => y.U(log2Ceil(dataY.length).W) })

  sorter.io.inputs := distances

  sorter.io.outputs.zip(mfo.io.input).foreach { case (sortOutput, mfoInput) =>
    mfoInput := predictions(sortOutput)
  }

  io.out := mfo.io.mostFrequentlyOccurringValue

  io.ready := ! busy

  when(io.dataLoaded) {
    sorter.io.newInputs := true.B
    busy := true.B
  }.elsewhen(busy && ! io.dataLoaded) {
    when(sorter.io.sortDone) {
      busy := false.B
    }
  }
}


object HardwareNearestNeighboursDriver {
  val TrainingSize = 300

  val euclideanDist = (v1: Array[Double], v2: Array[Double]) => v1.zip(v2).map(x => math.pow((x._1 - x._2), 2)).sum

  //function to transform the data into a couple of list of  Doubles and a string
  def line2Data(line: String): (List[Double], String) = {
    val elts = line.split(",")
    val y = elts.last
    val x = elts.dropRight(1).map(_.toDouble).toList
    (x, y)
  }


  //scalastyle:off method.length regex
  def main(args: Array[String]): Unit = {

    //loading the data from file
    val data = Source.fromFile("ionosphere.data.txt").getLines().map(x => line2Data(x)).toList

    val outputs = data.map(_._2)        //take the last column of strings.
    val inputs = data.map(_._1).toArray //convert the list of list double in to an array of list double.

    println("The full dataset size is ${outputs.size}")

    val distinctOutputs = outputs.distinct.toArray
    val outputToIndex = distinctOutputs.zipWithIndex.map { case (x, index) => x -> index }.toMap

    println(s"distinctOutputs: ${outputToIndex.map { case (s, n) => f"$s:$n"}.mkString(",")}")

    val dataX = inputs.map(_.toArray).take(TrainingSize) //we convert the array of list double into an array of array double and take just TrainingSize from the inputs data.
    val dataY = outputs.take(TrainingSize).map { string => outputToIndex(string)}              //we take just TrainingSize of the sequence of strings
    val keySize = dataX(2).length //to keySize we assign the length of the second array of dataX
    println(s"The key size is $keySize") //we check the length
    val fixedWidth = 64
    val binaryPoint = 32
    val k = 5

    def makeModule() = {
      () => new HardwareNearestNeighbours(
        FixedPoint(fixedWidth.W, binaryPoint.BP),
        k, keySize, dataY, dataX
      )
    }

    iotesters.Driver.execute(args, makeModule()) { c =>
      new PeekPokeTester(c) {
        val traininputs  = inputs.take(TrainingSize) //use the first TrainingSize data points of our data sets
        val trainoutputs = outputs.take(TrainingSize)
        val myNN = new NearestNeighbours(k = 4, dataX = traininputs.map(x => x.toArray), dataY = trainoutputs, euclideanDist)

        var correct = 0
        (TrainingSize until outputs.length).foreach { exampleId =>
          val pred = myNN.predict(inputs(exampleId).toArray)
          val target = outputs(exampleId)
          if (pred == target) correct += 1

          poke(c.io.dataLoaded, 1)
          inputs(exampleId).zipWithIndex.foreach { case (value, index) =>
              pokeFixedPoint(c.io.key(index), value)
          }
          step(1)

          poke(c.io.dataLoaded, 0)

          var count = 0
          while(peek(c.io.ready) == 0 && count < 1000) {
            println(s"Waiting for KNN, cycle is $count")
            step(1)
            count = count + 1
          }
          if(count >= 1000) {
            println(s"HardwareNearestNeighbor failed to complete")
          }

          val prediction = peek(c.io.out).toInt
          val predictedString = distinctOutputs(prediction)
          println(s"NearestNeighbor: output => $prediction which corresponds to prediction $predictedString")
        }

        println("The accuracy is\n" + correct.toDouble / (TrainingSize to outputs.length).length)

      }
    }
  }
}

