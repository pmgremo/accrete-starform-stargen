package com.szadowsz.starform

import com.szadowsz.starform.model.SimConstants

import java.lang.System.Logger.Level.INFO

/**
  * A Modification of DoleAccretionTestCase. Simple Application to generate and print off planets to the logs.
  *
  * @author Zakski : 06/07/2015.
  */
object StarformApp {

  protected val logger: System.Logger = System.getLogger(getClass.getName)

  def main(args: Array[String]): Unit = {
    val starform = new StarformSimulation(SimConstants(None, None, None, None, None))
    val system = starform.generateSystem(Some(1))
    system.planets.zipWithIndex.foreach { case (pl, i) => logger.log(INFO, "Planet {0}: {1}", i, pl) }
  }
}