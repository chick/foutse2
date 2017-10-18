package hardwaresort

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.internal.firrtl.KnownBinaryPoint
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FreeSpec, Matchers}

//scalastyle:off magic.number

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

  //noinspection ScalaStyle
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

  val outputs = if(c.reverseSort) {
    c.io.outputs.map { x => peekFixedPoint(x) }.reverse
  }
  else {
    c.io.outputs.map { x => peekFixedPoint(x) }
  }

  assert(outputs.zip(outputs.tail).forall { case (a: Double, b:Double) => a <= b })
}

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
    //noinspection ScalaStyle
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

  val outputs = if(c.reverseSort) {
    c.io.outputs.map { x => peekFixedPoint(x) }.reverse
  }
  else {
    c.io.outputs.map { x => peekFixedPoint(x) }
  }

  assert(outputs.zip(outputs.tail).forall { case (a: Double, b:Double) => a < b })

}


class SortTestSpec extends FreeSpec with Matchers {
  "multi cycle sorter tests" - {
    "reversed sort takes only top 5 of 15 inputs" in {
      iotesters.Driver.execute(
        Array.empty[String],
        () => new SortAndTake(15, 5, FixedPoint(16.W, 8.BP), reverseSort = true)
      ) { c =>
        new SortTester(c)
      } should be(true)
    }

    "basic sort of all elements" in {
      iotesters.Driver.execute(Array.empty[String], () => new SortAndTake(5, 5, FixedPoint(16.W, 8.BP))) { c =>
        new SortTester(c)
      } should be(true)
    }

    "reversed sort takes only top 5 of 20 inputs" in {
      iotesters.Driver.execute(
        Array.empty[String],
        () => new SortAndTake(20, 5, FixedPoint(16.W, 8.BP), reverseSort = true)
      ) { c =>
        new SortTester(c)
      } should be(true)
    }
  }
  "single cycle sorter tests" - {
    "basic sort of all elements" in {
      iotesters.Driver.execute(
        Array.empty[String],
        () => new CombinationalSortAndTake(5, 5, FixedPoint(16.W, 8.BP))
      ) { c =>
        new CombinationalSortTester(c)
      } should be(true)
    }

    "reversed sort takes only top 5 of 20 inputs" in {
      iotesters.Driver.execute(
        Array.empty[String],
        () => new CombinationalSortAndTake(20, 5, FixedPoint(16.W, 8.BP), reverseSort = true)
      ) { c =>
        new CombinationalSortTester(c)
      } should be(true)
    }
  }
}