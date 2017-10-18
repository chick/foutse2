package hardwaresort

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.internal.firrtl.KnownBinaryPoint
import chisel3.iotesters.PeekPokeTester
import chisel3.util.log2Ceil

//scalastyle:off magic.number
/**
  * Implements a naive and non-optimized hardware sort of FixedPoint numbers.  Sorts the inputs and returns either
  * the outputSize highest or lowest depending on reverseSort.
  * This has a very primitive flow control.  Parent sets when inputs are to be read, and then should wait until
  * sort is complete.  Sort may be complete before circuit realizes it.  (this could be fixed)
  *
  * Basic stragey is to copy inputs to a vector
  * then adjacent registers and flip them if the first is greater than the second
  * When selecting register pairs on even cycles compare 0 to 1, 2 to 3... on odd cycles compare 1 to 2, 3 to 4 ...
  *
  * @param inputSize   how many values to sort
  * @param outputSize  how many of the top or bottom sorted values to return
  * @param fixedType   Size of FixedPointer numbers in input
  * @param reverseSort returns lowest sorted values when false, highest sorted values when true
  */
class SortAndTake(val inputSize: Int, val outputSize: Int, val fixedType: FixedPoint, val reverseSort: Boolean = false)
  extends Module {
  val io = IO(new Bundle {
    val inputs    = Input(Vec(inputSize, fixedType))
    val newInputs = Input(Bool())
    val outputs   = Output(Vec(outputSize, fixedType))
    val sortDone  = Output(Bool())
  })

  val sortReg      = Reg(Vec(inputSize, FixedPoint(64.W,16.BP)))
  val busy         = RegInit(false.B)
  val sortCounter  = RegInit(0.U(log2Ceil(inputSize).W))
  val isEvenCycle  = RegInit(false.B)

  when(io.newInputs) {
    // when parent module loads new inputs to be sorted, we load registers and prepare to sort
    sortReg.zip(io.inputs).foreach { case (reg, in) => reg := in }

    busy := true.B
    sortCounter := 0.U
    isEvenCycle := false.B
  }
    .elsewhen(busy) {
      isEvenCycle := ! isEvenCycle

      sortCounter := sortCounter + 1.U
      when(sortCounter >= inputSize.U) {
        busy := false.B
      }

      when(isEvenCycle) {
        sortReg.toList.sliding(2, 2).foreach {
          case regA :: regB :: Nil =>
            when(regA > regB) {
              // a is bigger than b, so flip this pair
              regA := regB
              regB := regA
            }
          case _ =>
          // this handles end case when there is nothing to compare register to
        }
      }
        .otherwise {
          sortReg.tail.toList.sliding(2, 2).foreach {
            case regA :: regB :: Nil =>
              when(regA > regB) {
                // a is bigger than b, so flip this pair
                regA := regB
                regB := regA
              }
            case _ =>
              // this handles end case when there is nothing to compare register to
          }
        }
    }

  io.sortDone := ! busy

  private val orderedRegs = if(reverseSort) sortReg.reverse else sortReg
  io.outputs.zip(orderedRegs).foreach { case (out, reg) =>
    out := reg
  }
}

class SortTester(c: SortAndTake) extends PeekPokeTester(c) {

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

object SortTest {
  def main(args: Array[String]): Unit = {
    iotesters.Driver.execute(Array.empty[String], () => new SortAndTake(5, 5, FixedPoint(16.W, 8.BP))) { c =>
      new SortTester(c)
    }

    iotesters.Driver.execute(
      Array.empty[String],
      () => new SortAndTake(20, 5, FixedPoint(16.W, 8.BP), reverseSort = true)
    ) { c =>
      new SortTester(c)
    }
  }
}