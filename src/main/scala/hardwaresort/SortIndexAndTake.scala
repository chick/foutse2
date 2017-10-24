package hardwaresort

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.internal.firrtl.KnownBinaryPoint
import chisel3.iotesters.PeekPokeTester
import chisel3.util.log2Ceil

//scalastyle:off magic.number
/**
  * Implements a naive and non-optimized hardware sort of FixedPoint numbers.  Sorts the inputs and returns either
  * the selected number (outputSize) of indices of the lowest values.
  * This has a very primitive flow control.
  * Parent sets when inputs are to be read by toggling newInputs
  * and then should wait until
  * sort is complete.  Sort may be complete before circuit realizes it.  (this could be fixed)
  *
  * Basic strategy is to copy the indices of the input to a register vector
  * then adjacent registers and flip them if the value at the first index is greater than the value at the second index
  * When selecting register pairs on even cycles compare 0 to 1, 2 to 3... on odd cycles compare 1 to 2, 3 to 4 ...
  *
  * @param inputSize   how many values to sort
  * @param outputSize  how many of the top or bottom sorted values to return
  * @param fixedType   Type template FixedPoint numbers in input
  */
class SortIndexAndTake(val inputSize: Int, val outputSize: Int, val fixedType: FixedPoint)
  extends Module {
  val io = IO(new Bundle {
    val inputs    = Input(Vec(inputSize, fixedType))
    val newInputs = Input(Bool())
    val outputs   = Output(Vec(outputSize, UInt((log2Ceil(inputSize) + 1).W)))
    val sortDone  = Output(Bool())
  })

  val sortReg      = Reg(Vec(inputSize, UInt((log2Ceil(inputSize) + 1).W)))
  val busy         = RegInit(false.B)
  val sortCounter  = RegInit(0.U(log2Ceil(inputSize).W))
  val isEvenCycle  = RegInit(false.B)

  when(io.newInputs) {
    // when parent module loads new inputs to be sorted, we load registers and prepare to sort
    sortReg.zipWithIndex.foreach { case (reg, index) => reg := index.U }

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
            when(io.inputs(regA) > io.inputs(regB)) {
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
              when(io.inputs(regA) > io.inputs(regB)) {
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

  io.outputs.zip(sortReg).foreach { case (out, reg) =>
    out := reg
  }
}

class SortIndexTester(c: SortIndexAndTake) extends PeekPokeTester(c) {
  val valuesToSort = (0 until c.inputSize).map { i => (c.inputSize - i).toDouble / 2.0 }

  def showOutputs(): Unit = {
    for(i <- 0 until c.outputSize) {
      val index = peek(c.io.outputs(i)).toInt
      print(f"$index%3d ${valuesToSort(index)}%8.4f   ")
    }
    println()
  }
  for(i <- 0 until c.inputSize) {
    pokeFixedPoint(c.io.inputs(i), valuesToSort(i))
  }
  poke(c.io.newInputs, 1)
  step(1)

  poke(c.io.newInputs, 0)
  step(1)

  showOutputs()

  // wait for sort to finish

  while(peek(c.io.sortDone) == 0) {
    showOutputs()
    step(1)
  }

  showOutputs()

}

object SortIndexTest {
  def main(args: Array[String]): Unit = {
    iotesters.Driver.execute(Array.empty[String], () => new SortIndexAndTake(5, 5, FixedPoint(16.W, 8.BP))) { c =>
      new SortIndexTester(c)
    }

    iotesters.Driver.execute(
      Array.empty[String],
      () => new SortIndexAndTake(20, 5, FixedPoint(16.W, 8.BP))
    ) { c =>
      new SortIndexTester(c)
    }
  }
}