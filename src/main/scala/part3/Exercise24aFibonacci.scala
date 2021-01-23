package part3

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import timing._

object Exercise24aFibonacci {
  // fib(n) is a computation of exponential complixity in terms of n.
  // - at n ~= 40 it will start taking a noticeable amount of time.
  // - at n ~= 50 it'll take an extremely long time.
  def fib(n: Int): Int =
    if (n <= 2) 1 else fib(n - 1) + fib(n - 2)

  def sync() = {
    fib(45)
    fib(45)
    fib(45)
    fib(45)
  }
  def async() = {
    val fa = Future { fib(45) }
    val fb = Future { fib(45) }
    val fc = Future { fib(45) }
    val fd = Future { fib(45) }
    val sth = for {
      a <- fa
      b <- fb
      c <- fc
      d <- fd
    } yield (a + b + c + d)
    Await.result(sth, Duration("10 seconds"))
  }

  def main(args: Array[String]): Unit = {
    // Step 1.
    // The computations below run in sequence.
    // Make them run in parallel.
    println("simple fibonacci calculations")
    Time.sync("one")(() => fib(45))
    Time.sync("sync")(sync)
    Time.sync("async")(async)

    // Step 2.
    // Find the sum of the four fibonacci numbers above
    // (ideally without blocking any threads).
  }
}
