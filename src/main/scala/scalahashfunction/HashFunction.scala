package scalahashfunction

import scala.util.Random

object HashFunction {
  //Here I want to create table of random vectors with components taken from the normal law
  // for local sensitive hashing.
  val tabHash = (n: Int, m: Int) => {

    val tabHash0 = new Array[Array[Double]](n)
    for (ind <- 0 until n) {
      val vechash1 = new Array[Double](m)
      for (ind2 <- 0 until n) vechash1(ind2) = Random.nextGaussian
      tabHash0(ind) = vechash1
    }

    tabHash0
  }

  //and generate the hash value for a given vector x depending on w, b, tabHash1:
  def hashfunc(x: Array[Double], w: Double, b: Double, tabHash1: Array[Array[Double]]): Double = {
    val tabHash = new Array[Double](tabHash1.size)
    for (ind <- tabHash1.indices) {
      var sum = 0.0
      for (ind2 <- 0 until x.size) {
        sum += (x(ind2) * tabHash1(ind)(ind2))
      }
      tabHash(ind) = (sum + b) / w
    }
    tabHash.reduce(_ + _)
  }


  //To test my code
  def main(args: Array[String]): Unit = {
    val n = 5
    val w = 2.5
    val b = 3.5
    val p = 5
    val m = 8
    // I generate a random vector
    val x = Seq.fill(n)(Random.nextDouble).toArray
    println("Given x = \n" + x.mkString(" "))
    //println("The hash value is\n" +  hashfunc(x, args(0).toDouble, args(1).toDouble, tabHash(args(2).toInt,args(3).toInt)))
    println("The hash value is\n" + hashfunc(x, w, b, tabHash(p, m)))

  }
}

//The command line arguments I used in testing are for exemple: sbt "run 0.5 6.2 4 5"
//Where w=3.5, b=6.2, n=4, m=5
//n= The length of the table I want to create
//m=length of vechash1
//m and n parameters of tabHash



