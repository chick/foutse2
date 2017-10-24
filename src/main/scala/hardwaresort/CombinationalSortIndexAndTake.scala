// See LICENSE for license details.

package hardwaresort

import chisel3._

//scalastyle:off magic.number
/**
  * Implements a naive and non-optimized combinational hardware sort of FixedPoint numbers.
  * Sorts the inputs and returns either the outputSize highest or lowest depending on reverseSort.
  * Similar to SortAndTake but fully unrolled.  Very difficult to synthesize accept for
  * very small input sizes.
  *
  * @param inputSize   how many values to sort
  * @param outputSize  how many of the top or bottom sorted values to return
  * @param elementType   Size of FixedPointer numbers in input
  */
class CombinationalSortIndexAndTake(
                                val inputSize: Int,
                                val outputSize: Int,
                                val elementType: UInt
                              ) extends Module {
  val io = IO(new Bundle {
    val inputs    = Input(Vec(inputSize, elementType))
    val newInputs = Input(Bool())
    val outputs   = Output(Vec(outputSize, UInt(inputSize.W)))
    val sortDone  = Output(Bool())
  })

  val sortedInputs = io.inputs.indices.foldLeft(io.inputs.indices.map(_.U).toList) { case (ll, index) =>
    def reorderPairs(list: List[UInt]) = {
      list.sliding(2, 2).toList.map {
        case a :: b :: Nil =>
          Mux(io.inputs(a) > io.inputs(b), b, a) :: Mux(io.inputs(a) > io.inputs(b), a, b) :: Nil
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


  io.outputs.zip(sortedInputs).foreach { case (out, reg) =>
    out := reg
  }

  io.sortDone := true.B
}
