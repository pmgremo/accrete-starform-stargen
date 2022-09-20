package com.szadowsz.starform.model.accrete

object Lists {
  extension[T] (xs: List[T]) {
    def inject(n: T, compare: (T, T) => Boolean, merge: (T, T) => T): List[T] = xs match {
      case Nil =>
        List(n)
      case h :: t =>
        if (compare(h, n)) merge(h, n) :: t
        else h :: t.inject(n, compare, merge)
    }
  }
}
