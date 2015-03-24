/*
 * Copyright 2003-2015 Monitise Group Limited. All Rights Reserved.
 *  
 * Save to the extent permitted by law, you may not use, copy, modify,
 * distribute or create derivative works of this material or any part
 * of it without the prior written consent of Monitise Group Limited.
 * Any reproduction of this material must contain this notice.
 *  
 * Created by yurtozc on 3/23/15 12:39 PM.
 */

package com.cagatay.s99

import scala.util.Random

class Lists2 {

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
