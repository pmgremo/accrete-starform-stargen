package com.szadowsz.starform

import com.szadowsz.starform.profile.starform.FoggProfile
import com.szadowsz.starform.sim.FoggSimulation

import java.lang.System.Logger.Level.INFO

/**
  * A Modification of DoleAccretionTestCase. Simple Application to generate and print off planets to the logs.
  *
  * @author Zakski : 06/07/2015.
  */
object FoggApp {

  protected val logger: System.Logger = System.getLogger(getClass.getName)

  def main(args: Array[String]): Unit = {
    val starform = FoggSimulation(new FoggProfile())
    val system = starform.generateSystem(Some(1))
    system.planets.zipWithIndex.foreach { case (pl, i) => logger.log(INFO, "Planet {0}: {1}", i, pl) }
  }
}