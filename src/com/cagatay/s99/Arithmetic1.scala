/*
 * Copyright 2003-2015 Monitise Group Limited. All Rights Reserved.
 *  
 * Save to the extent permitted by law, you may not use, copy, modify,
 * distribute or create derivative works of this material or any part
 * of it without the prior written consent of Monitise Group Limited.
 * Any reproduction of this material must contain this notice.
 *  
 * Created by yurtozc on 3/23/15 10:31 PM.
 */

package com.cagatay.s99

class Arithmetic1 {

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
}
