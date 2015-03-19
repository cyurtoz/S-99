/*
 * Created by yurtozc on 3/3/15 11:19 PM.
 */

package com.cagatay.s99

class Answers {

  def last(list:List[Any]) = list(list.size - 1)
  def last2(list:List[Any]) = if(list.size == 1) list.head else last(list.tail)
  def lastBuiltin(list:List[Any]) = list.last

  def penultimate(list:List[Any]) = list(list.size - 2)
  def penultimate2(list:List[Any]) = if(list.size == 2) list.head else last(list.tail)
  def penultimate3(list:List[Any]) = list.init.last

  def kTh(k:Int, list:List[Any]) = list.dropRight(k).last

  def length(list:List[Any]) = {
    def acc(n: Int, list2: List[Any]):Int = {
      if (list2.isEmpty) n else acc(n + 1, list2.tail)
    }
    acc(0, list)
  }

  def lengthFoldLeft(list:List[Any]) = list.foldLeft(0)((x,_)=>x+1)

  def reverse(list:List[Any])
}
