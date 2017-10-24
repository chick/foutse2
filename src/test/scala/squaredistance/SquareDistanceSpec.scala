// See LICENSE for license details.

package squaredistance

import chisel3._
import chisel3.iotesters
import chisel3.experimental.FixedPoint
import chisel3.internal.firrtl.KnownBinaryPoint
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Random

object SquareDistance {
  def apply(vector1: Seq[Double], vector2: Seq[Double]): Double = {
    assert(vector1.length == vector2.length)

    (vector1 zip vector2).map { case (x, y) => (y - x) * (y - x) }.reduceLeft(_ + _)
  }
}

class SquareDistanceHardwareTester(c: SqDist) extends PeekPokeTester(c) {
  for(trial <- 0 to 20) {

    val v1 = Seq.tabulate(10) { x => Random.nextDouble() }
    val v2 = Seq.tabulate(10) { x => Random.nextDouble() }

    v1.zipWithIndex.foreach { case (value, index) => pokeFixedPoint(c.io.in1(index), value) }
    v2.zipWithIndex.foreach { case (value, index) => pokeFixedPoint(c.io.in2(index), value) }

    step(1)

    val hardwareResult = peekFixedPoint(c.io.out)
    val softwareResult = SquareDistance(v1, v2)
    val difference = ((hardwareResult - softwareResult).abs / softwareResult ) * 100.0
    println(f"$trial%2d hardware $hardwareResult%10.8f    software $softwareResult%10.8f    " +
      f"error $difference%8.6f %%")
  }
}

class SquareDistanceSpec extends FreeSpec with Matchers {
  "hardware should match software up to some precision difference" - {
    "software testing with 10-vector" in {
      val v1 = Seq.tabulate(10) { x => x.toDouble }
      val v2 = Seq.tabulate(10) { x => x.toDouble }.reverse

      println(s"result is ${SquareDistance(v1, v2)}")
    }
    "hardware testing with 10-vector" in {
      iotesters.Driver.execute(Array.empty[String], () => new SqDist(10, FixedPoint(32.W, 16.BP))) { c =>
        new SquareDistanceHardwareTester(c)
      }
    }
  }
}
