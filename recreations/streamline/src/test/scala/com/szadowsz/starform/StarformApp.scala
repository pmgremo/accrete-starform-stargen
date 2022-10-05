package com.szadowsz.starform

import com.szadowsz.starform.model.SimConstants

import scala.sys.SystemProperties

/**
  * A Modification of DoleAccretionTestCase. Simple Application to generate and print off planets to the logs.
  *
  * @author Zakski : 06/07/2015.
  */
object StarformApp {

  sys.props.put("java.util.logging.SimpleFormatter.format", "%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS %4$s %3$s %5$s%6$s%n")

  def main(args: Array[String]): Unit = {
    val starform = new StarformSimulation(SimConstants(None, None, None, None, None))
    val system = starform.generateSystem(Some(1_662_642_772_940L))
    system.planets.zipWithIndex.foreach { case (pl, i) => printf("Planet %s: %s%n", i, pl) }
  }
}