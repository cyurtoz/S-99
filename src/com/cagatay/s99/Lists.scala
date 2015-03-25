/*
 * Created by yurtozc on 3/3/15 11:19 PM.
 */

package com.cagatay.s99

import scala.util.Random

class Lists[T] {

  // 1
  def last(list:List[Any]) = list(list.size - 1)
  def last2(list:List[Any]) = if(list.size == 1) list.head else last(list.tail)
  def lastBuiltin(list:List[Any]) = list.last

  // 2
  def penultimate(list:List[Any]) = list(list.size - 2)
  def penultimate2(list:List[Any]) = if(list.size == 2) list.head else last(list.tail)
  def penultimate3(list:List[Any]) = list.init.last

  // 3
  def kTh(k:Int, list:List[Any]) = list.dropRight(k).last

  // 4
  def length(list:List[Any]) = {
    def acc(n: Int, list2: List[Any]):Int = {
      if (list2.isEmpty) n else acc(n + 1, list2.tail)
    }
    acc(0, list)
  }
  def lengthFoldLeft(list:List[Any]) = list.foldLeft(0)((x,_)=>x+1)

  // 5
  def reverse(list: List[Any]) = {
    def acc(listOriginal:List[Any], reversed: List[Any]):List[Any] = {
      if(listOriginal.isEmpty) reversed else acc(listOriginal.init, reversed :+ listOriginal.last)
    }
    acc(list, Nil)
  }
  def reverseBuiltIn(list: List[Any]) = list.reverse
  def reverseFoldLeft(list: List[Any]) = list.foldLeft(List[Any]())((x,y)=>y::x)

  //6
  def isPalindrome(list:List[Any]) = list.sameElements(list.reverse)
  def isPalindrome2(list:List[Any]):Boolean = {
    def acc(list2:List[Any]):Boolean = {
      if(list2.size < 2) true
      else if(list2.head == list2.last) acc(list2.tail.init)
      else false
    }
    acc(list)
  }

  //7
  def flatten(list:List[Any]):List[Any] = list flatMap {
    case l:List[Any] => flatten(l)
    case e => List(e)
  }

  //8
  def compressBuiltIn(list:List[Any]):List[Any] = list.distinct

  def compress(list:List[Any]):List[Any] = {
    def compress2(acc:List[Any], rem:List[Any]):List[Any] = {
      if(rem.isEmpty) acc
      else if (rem.tail.contains(rem.head)) compress2(acc:+rem.head, rem.tail.filter(_!=rem.head))
      else compress2(acc:+rem.head, rem.tail)
    }
    compress2(Nil, list)
  }

  def compressFoldRight[Any](list:List[Any]):List[Any] = {
    list.foldRight(List[Any]())((x, y)=> if(y.isEmpty||y.head != x) x::y else y)
  }

  //9
  def pack(list:List[T]):List[List[T]] = {
    if (list.isEmpty) List(List())
    else {
      val (packed, next) = list span { _ == list.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  //10
  def encode(list:List[T]) = pack(list).map(x=>(x.size, x.head))

  //11
    def encodeModified(list:List[T]) = encode(list).map(x=> if(x._1 == 1) x._2 else x)

  //12
  def decode(list:List[(Int, Any)]):List[Any] =
    if(list.isEmpty) Nil
    else list flatMap {x => List.fill(x._1)(x._2)}

  //13
  def encodeDirect(list:List[Any]):List[Any] = if(list.isEmpty) Nil else {
    val spanned = list.span(x=> x == list.head)
    (spanned._1.size, spanned._1.head) :: encodeDirect(spanned._2)
  }

  //14
  def duplicate(list:List[Any]) = list flatMap {x=>List.fill(2)(x)}

  //15
  def duplicateN(n:Int, list:List[Any]) = list flatMap {x=>List.fill(n)(x)}

  //16
  def dropNBuiltIn(n:Int, list:List[Any]):List[Any] =
    if(list.length >= n) list.slice(0, n-1) ++ dropNBuiltIn(n, list.slice(n, list.length))
    else list

  def dropNZip(n:Int, list:List[Any]) = ???

  //17
  def split(n:Int, list:List[Any]):(List[Any], List[Any]) = (list.take(n), list.drop(n))

  //18
  def slice(n:Int, m:Int, list:List[Any]) = list.drop(n).take(m-n)

  //19
  def rotate(n:Int, list:List[Any]) =
    if(n>0) split(n, list)._2 ++ split(n, list)._1
    else list.slice(Math.abs(n)+1, list.length) ++ list.take(Math.abs(n)+1)

  //20
  def removeAt(n:Int, list:List[Any]) = list.take(n) ++ list.slice(n+1, list.length)

  //21
  def insertAt(elem:Any, n:Int, list: List[Any]) = (list.take(n):+elem)++list.slice(n, list.length)

  //22
  def rangeBuiltIn(n:Int, m:Int) = List.range(n,m)
  def range(n:Int, m:Int) = for (i<-n to m) yield i

  //23
  def random(n:Int, list:List[Any]) = for (i<-0 to n-1) yield list(new Random().nextInt(list.length))

  //24
  def lotto(n:Int, all:Int) = random(n, (1 to all).toList)

  //25
  def randomPermute(list:List[Any]) = random(list.length, list)

  //26
  def combinationsBuiltIn(n:Int, list: List[Any]) = list.combinations(n).toList
  def combinations(n:Int, list: List[Any]) = list.toSet.subsets(n).toList

  //27
  def group3() = ???

}
