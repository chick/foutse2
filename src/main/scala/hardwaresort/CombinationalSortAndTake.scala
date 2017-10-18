package hardwaresort

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.internal.firrtl.KnownBinaryPoint
import chisel3.iotesters.PeekPokeTester
import chisel3.util.log2Ceil

//scalastyle:off magic.number
/**
  * Implements a naive and non-optimized combinational hardware sort of FixedPoint numbers.
  * Sorts the inputs and returns either the outputSize highest or lowest depending on reverseSort.
  * Similar to SortAndTake but fully unrolled.  Very difficult to synthesize accept for
  * very small input sizes.
  *
  * @param inputSize   how many values to sort
  * @param outputSize  how many of the top or bottom sorted values to return
  * @param fixedType   Size of FixedPointer numbers in input
  * @param reverseSort returns lowest sorted values when false, highest sorted values when true
  */
class CombinationalSortAndTake(
                                val inputSize: Int,
                                val outputSize: Int,
                                val fixedType: FixedPoint,
                                val reverseSort: Boolean = false
                              ) extends Module {
  val io = IO(new Bundle {
    val inputs    = Input(Vec(inputSize, fixedType))
    val newInputs = Input(Bool())
    val outputs   = Output(Vec(outputSize, fixedType))
    val sortDone  = Output(Bool())
  })

  val sortedInputs = io.inputs.indices.foldLeft(io.inputs.toList) { case (ll, index) =>
    def reorderPairs(list: List[FixedPoint]) = {
      list.sliding(2, 2).toList.map {
        case a :: b :: Nil =>
          Mux(a > b, b, a) :: Mux(a > b, a, b) :: Nil
        case b :: Nil =>
          b :: Nil
        case _ => Nil
      }.flatten
    }

    if(index % 2 == 0) {
      reorderPairs(ll)

    }
    else {
      List(ll.head) ++ reorderPairs(ll.tail)
    }
  }


  private val orderedRegs = if(reverseSort) sortedInputs.reverse else sortedInputs
  io.outputs.zip(orderedRegs).foreach { case (out, reg) =>
    out := reg
  }

  io.sortDone := true.B
}

//noinspection ScalaStyle
class CombinationalSortTester(c: CombinationalSortAndTake) extends PeekPokeTester(c) {

//  def pokeFixedPoint(signal: FixedPoint, value: Double): Unit = {
//    val bigInt = value.F(signal.binaryPoint).litValue()
//    poke(signal, bigInt)
//  }
//  def peekFixedPoint(signal: FixedPoint): Double = {
//    val bigInt = peek(signal)
//    signal.binaryPoint match {
//      case KnownBinaryPoint(bp) => FixedPoint.toDouble(bigInt, bp)
//      case _ => throw new Exception("Cannot peekFixedPoint with unknown binary point location")
//    }
//  }

  def showOutputs(): Unit = {
    for(i <- 0 until c.outputSize) {
      print(f"${peekFixedPoint(c.io.outputs(i))}%10.5f ")
    }
    println()
  }

  Array(20.1,3.0,12.3,4.0,2.3,1.0,0.9,0.5,0.6,0.03,0.41,5.64,89.0,20.3,20.1)
//    .map(_.F(fixedType.getWidth.W, fixedType.binaryPoint))
  for(i <- 0 until c.inputSize) {
    pokeFixedPoint(c.io.inputs(i), (c.inputSize - i).toDouble / 2.0)
  }
  poke(c.io.newInputs, 1)
  step(1)

  poke(c.io.newInputs, 0)
  step(1)

  // wait for sort to finish

  while(peek(c.io.sortDone) == 0) {
    showOutputs()
    step(1)
  }

  showOutputs()

}

object CombinationalSortTest {
  def main(args: Array[String]): Unit = {
    iotesters.Driver.execute(
      Array.empty[String],
      () => new CombinationalSortAndTake(5, 5, FixedPoint(16.W, 8.BP))
    ) { c =>
      new CombinationalSortTester(c)
    }

    iotesters.Driver.execute(
      Array.empty[String],
      () => new CombinationalSortAndTake(20, 5, FixedPoint(16.W, 8.BP), reverseSort = true)
    ) { c =>
      new CombinationalSortTester(c)
    }
  }
}