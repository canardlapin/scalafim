package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

object NArrayUtil:

  def ofSize[A](n: Int)(using ClassTag[A]): NArray[A] =
    narr.NArray.ofSize[A](n)

  def fromArray[A](arr: Array[A])(using ClassTag[A]): NArray[A] =
    val out = narr.NArray.ofSize[A](arr.length)
    var i = 0
    while i < arr.length do
      out(i) = arr(i)
      i += 1
    out

  def fill[A](n: Int)(elem: => A)(using ClassTag[A]): NArray[A] =
    val out = narr.NArray.ofSize[A](n)
    var i = 0
    while i < n do
      out(i) = elem
      i += 1
    out

  def fillConst[A](n: Int, elem: A)(using ClassTag[A]): NArray[A] =
    val out = narr.NArray.ofSize[A](n)
    var i = 0
    while i < n do
      out(i) = elem
      i += 1
    out

  def tabulate[A](n: Int)(f: Int => A)(using ClassTag[A]): NArray[A] =
    val out = narr.NArray.ofSize[A](n)
    var i = 0
    while i < n do
      out(i) = f(i)
      i += 1
    out

  def copyInto[A](src: NArray[A], srcPos: Int, dest: NArray[A], destPos: Int, length: Int): Unit =
    var i = 0
    while i < length do
      dest(destPos + i) = src(srcPos + i)
      i += 1
