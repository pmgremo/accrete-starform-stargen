package com.szadowsz.starform

import com.szadowsz.starform.profile.accrete.DoleProfile
import com.szadowsz.starform.sim.DoleSimulation

import java.lang.System.Logger.Level.INFO

/**
  * A Modification of DoleAccretionTestCase. Simple Application to generate and print off planets to the logs.
  *
  * @author Zakski : 06/07/2015.
  */
object DoleApp {

  protected val logger: System.Logger = System.getLogger(getClass.getName)

  def main(args: Array[String]): Unit = {
    val acrete = DoleSimulation(new DoleProfile)
    val system = acrete.generateSystem()
    system.planets.zipWithIndex.foreach{case (pl,i) => logger.log(INFO,"Planet {0}: {1}", i, pl)}
  }
}