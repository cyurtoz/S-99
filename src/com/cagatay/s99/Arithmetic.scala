/*
 * Created by yurtozc on 3/23/15 10:31 PM.
 */

package com.cagatay.s99


class Arithmetic(val start: Int) {

  //31
  def isPrime(num:Int):Boolean = {
    val divisors = (2 to Math.sqrt(num).toInt) toList

    def process(list:List[Int]):Boolean = {
      if (list.isEmpty) true
      else if (num % list.head == 0) false
      else process(list.tail)
      }
    process(divisors)
  }

  def isPrimeEratosthenesLazy(num:Int):Boolean = {
    val divisors = (2 to num/2) toStream

    def process(stream:Stream[Int], primes:List[Int]):Boolean = {
      val filtered = stream.tail.filter(_%stream.head != 0)
      if(stream.tail.isEmpty)
        if(primes.exists(num%_==0)) false
        else true
      else process(filtered, primes:+stream.head)
    }
    process(divisors, List())
  }

  //32
  def gcd(n:Int, m:Int):Int = {
    if(n%m == 0) m
    else gcd(m, gcd(n, n%m))
  }

  //33
  def isCoprimeTo(n:Int) = gcd(start, n) == 1

  //34
  def totient() = (for (i<-1 until start if isCoprimeTo(i)) yield i).length

  //35
  def primeFactors():List[Int] = {
    def primez(acc:Int, divisor:Int, primex:List[Int]):List[Int] = {
      if(acc == 1) primex
      else if (!isPrime(divisor)) primez(acc, divisor+1, primex)
      else if(acc%divisor == 0) primez(acc/divisor, divisor, primex:+divisor)
      else primez(acc, divisor+1, primex)
    }
    primez(start, 2, List())
  }

  //36
  def primeFactorMultiplicity():List[(Int, Int)] = new Lists().encode(primeFactors()).map(x => (x._2, x._1))

  //37
  def totientImproved() = primeFactorMultiplicity().foldLeft(1) {
    (x,y) =>  x*(y._1-1) * Math.pow(y._1, y._2-1).toInt
  }

  //38
  def compareTotient():(Long, Long) = {
    val e = System.currentTimeMillis()
    totient()
    val x = System.currentTimeMillis()
    totientImproved()
    val b = System.currentTimeMillis()
    (x-e, b-x)
  }

  //39
  def listPrimesInRange(r:Range):List[Int] = {
    val stream = r.toStream
    def primez(acc:Stream[Int], list:List[Int]):List[Int] = {
      if(acc.isEmpty) list
      else if(isPrime(acc.head)) primez(acc.tail.filter(_%acc.head!=0), list:+acc.head)
      else primez(acc.tail, list)
    }
    primez(stream, List())
  }

  //40
  def goldbach():(Int, Int) = {
    val pair = listPrimesInRange(2 to start).combinations(2).filter(x => x.head + x(1) == start).toList
    (pair.head.head, pair.head(1))
  }

  //??
  def goldbachList() = listPrimesInRange(2 to start).combinations(2).filter(x => x.head + x(1) == start).toList.map(x=>(x.head, x(1)))

  //41
  def printGoldbachList(r:Range) = {
    for (i <- r.head to r.last if i % 2 == 0)
      yield listPrimesInRange(2 to start).combinations(2).filter(x => x.head + x(1) == i)
        .toList
        .map(x => new String(i + " = " + x.head + "+" + x(1)))
  } flatten
}
