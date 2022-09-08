package com.szadowsz.starform.rand

import com.szadowsz.starform.rand.GilhamRandom.modulo

import java.util.Random
import java.util.concurrent.atomic.AtomicLong
import scala.math.pow

object GilhamRandom {
  private val modulo = pow(2, 32).toLong
}

/**
  * linear congruential random numbers - algorithm as per VMS MTH$RANDOM function
  *
  * Created on 11/04/2017.
  */
class GilhamRandom(seed: AtomicLong = new AtomicLong(50957)) extends Random {
  override def next(bits: Int): Int = {
    var oldseed = 0L
    var nextseed = 0L
    while ( {
      oldseed = seed.get
      nextseed = (oldseed * 69069 + 1) % modulo
      !seed.compareAndSet(oldseed, nextseed)
    }) ()
    (nextseed >>> (48 - bits)).toInt
  }

  override def setSeed(x: Long): Unit = seed.set(x)
}
