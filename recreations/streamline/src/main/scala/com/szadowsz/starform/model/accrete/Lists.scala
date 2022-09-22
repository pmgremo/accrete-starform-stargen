package com.szadowsz.starform.model.accrete

import scala.util.Random

object Lists {
  extension[T] (xs: List[T]) {
    def inject(n: T, compare: (T, T) => Boolean, merge: (T, T) => T): List[T] = xs match {
      case Nil =>
        List(n)
      case h :: t =>
        if compare(h, n) then merge(h, n) :: t
        else h :: t.inject(n, compare, merge)
    }

    def random()(using rand: Random): T = xs(rand.nextInt(xs.size))
  }
}
