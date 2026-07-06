package scalafim.image

import narr.NArray
import scala.scalajs.js

extension [A](arr: NArray[A])
  inline def length: Int =
    arr.asInstanceOf[js.Dynamic].selectDynamic("length").asInstanceOf[Int]

  inline def apply(i: Int): A =
    arr.asInstanceOf[js.Dynamic].selectDynamic(i.toString).asInstanceOf[A]

  inline def update(i: Int, value: A): Unit =
    arr.asInstanceOf[js.Dynamic].updateDynamic(i.toString)(value.asInstanceOf[js.Any])
