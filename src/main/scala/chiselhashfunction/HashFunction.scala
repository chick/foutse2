// See LICENSE for license details.

package chiselHashFunctiontion

import chisel3._
import chisel3.experimental.FixedPoint

/**
  *  Generate the hash value for a given vector x depending on w, b, tabHash1
  *  NOTE: w acts a divisor that can only be a power of 2.
  *
  *  The short form of the compuation block below is
  *    {{{
  *    io.tabHash1.map { row =>
  *      (row.zip(io.xa).map { case (a, b) => a * b }.reduce(_ + _) + io.b ) * io.wInverse
  *    }.reduce(_ + _)
  *    }}}
  */
class HashFunction(val keySize:Int, val fixedType: FixedPoint) extends Module {
  val io = IO(new Bundle {
    val x        = Input(Vec(keySize, fixedType))
    val tabHash1 = Input(Vec(keySize, Vec(keySize,  fixedType)))
    val b        = Input(fixedType)
    val wInverse = Input(fixedType)
    var out      = Output(fixedType)
  })

  io.out := io.tabHash1.map { row =>
    val keyRowPairs   = io.x.zip(row)
    val rowProducts   = keyRowPairs.map { case (a, b) => a * b }
    val rowSum        = rowProducts.reduce(_ + _)
    val normalizedSum = (rowSum + io.b) * io.wInverse
    normalizedSum
  }.reduce(_ + _)
}


object HashFunctionDriver extends App {
  chisel3.Driver.execute(args, () => new HashFunction(4, FixedPoint(16.W, 8.BP)))
}