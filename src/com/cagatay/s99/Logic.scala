/*
 * Copyright 2003-2015 Monitise Group Limited. All Rights Reserved.
 *  
 * Save to the extent permitted by law, you may not use, copy, modify,
 * distribute or create derivative works of this material or any part
 * of it without the prior written consent of Monitise Group Limited.
 * Any reproduction of this material must contain this notice.
 *  
 * Created by yurtozc on 3/25/15 4:55 PM.
 */

package com.cagatay.s99

class Logic(t:Boolean) {

  //46
  def and(p:Boolean, q:Boolean) = p && q
  def or(p:Boolean, q:Boolean) = p || q
  def not(p:Boolean) = !p
  def xor(p:Boolean, q:Boolean) = or(and(not(p), q), and(p, not(q)))
  def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(not(a), not(b)))
  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))
  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))
  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

  //47
  def and(p:Boolean) = p && t
  def or(p:Boolean) = p || t
  def not = !t
  def xor(q:Boolean) = or(and(not(t), q), and(t, not(q)))
  def equ(b: Boolean): Boolean = or(and(t, b), and(not(t), not(b)))
  def nor(b: Boolean): Boolean = not(or(t, b))
  def nand(b: Boolean): Boolean = not(and(t, b))
  def impl(b: Boolean): Boolean = or(not(t), b)

  def table(f:(Boolean, Boolean)=>Boolean) = {
    List(
    new String(false + " " + false +" "+ f(false, false) +"\n"),
    new String(false + " " + true +" "+ f(false, true) +"\n"),
    new String(true + " " + false +" "+ f(true, false) +"\n"),
    new String(true + " " + true +" "+ f(true, true) +"\n")
    ) mkString
  }

  //49
  def gray(n:Int) = {
    def grayz(p:Int, s:List[Char]):Unit = {
      if (s.length == n)
        println(s mkString)
        else {
          grayz(p+1, s:+'0')
          grayz(p+1, s:+'1')
        }
      }

    grayz(0, List())
  }

}
