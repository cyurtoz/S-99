/*
 * Created by yurtozc on 3/3/15 11:19 PM.
 */

package com.cagatay.s99

class Answers {

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
}
