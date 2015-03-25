/*
 * Created by yurtozc on 3/23/15 10:31 PM.
 */

package com.cagatay.s99

import com.cagatay.s99.Lists

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
  def primeFactorMultiplicity() = {
    //code reuse :P
    new Lists().encode(primeFactors()).map(x => (x._2, x._1))
  }

  //37
  def totientImproved() = ???


}
