package com.szadowsz.starform.model.accrete

import com.szadowsz.starform.model.accrete.Lists.random
import com.szadowsz.starform.system.bodies.DustBand

import scala.util.Random

object InsertionStrat {
  private val defaultRandomCount: Int = 20
}

/**
  * Strategy to insert new planetismals into the system. Randomly inserts up to N nuclei, then only considers Bands between the min and max that
  * have dust. Introduced by Folkins.
  *
  * @author zakski : 30/09/2016.
  */
case class InsertionStrat(aConst: AccreteConstants, randomisedCount: Int = InsertionStrat.defaultRandomCount) {

  /**
    * Function to produce the semi major axis of a planetesimal. Formula taken from "Formation of Planetary Systems by Aggregation: A Computer Simulation" in
    * "III. Experimental Simulation, section b) The Planetary Nuclei".
    *
    * @note made minor cosmetic change from 50Yi, where Yi is a random number and 50 is outer limit. Innermost limit is
    *       mentioned separately originally.
    * @note unit of return value is AU
    * @see pp. 15-17, Formation of Planetary Systems by Aggregation: A Computer Simulation - Stephen H. Dole
    * @see method RandomPlanetismal, line 58 in Planetismal.java - Ian Burrell (accrete)
    * @see method distribute_planetary_masses, line 405 in accrete.c - Mat Burdick (accrete)
    * @see method ComputePlanetStats, lines 156-163 in Dole.c - Andrew Folkins (accretion)
    * @see method CreatePlanet, lines 211-223 in dole.c - Keris (accretion v1)
    * @see method CreatePlanet, line 301 in dole.cc - Keris (accretion v2)
    * @see method dist_planetary_masses, line 433 in accrete.c - Keris (starform)
    * @see method dist_planetary_masses, line 403 in accrete.c - Mat Burdick (starform)
    * @see constructor Protoplanet, line 150 in  Protoplanet.java - Carl Burke (starform)
    * @param rand      - a random number generator to supply a value between 0.0 and 1.0
    * @param innermost the closest distance to the parent star that a planet can form at.
    * @param outermost the furthest distance from the parent star that a planet can form at.
    * @param bands     the current state of the accretion disc that the new planetismal will be inserted into.
    * @return the semi major axis of a planetismal orbiting a star in AU.
    */
  protected def semiMajorAxis(nucleiCount: Int, innermost: Double, outermost: Double, bands: List[DustBand])(using rand: Random): Double = {
    if (randomisedCount >= nucleiCount) {
      rand.between(innermost, outermost)
    } else {
      val band = bands.filter(db => db.hasDust && db.outerEdge > innermost && db.innerEdge < outermost).random()
      val min = Math.max(band.innerEdge, innermost)
      val max = Math.min(band.outerEdge, outermost)
      rand.between(min, max)
    }
  }

  /**
    * Method to produce the eccentricity of a planetesimal. Formula taken from "Formation of Planetary Systems
    * by Aggregation: A Computer Simulation" in "III. Experimental Simulation, section b) The Planetary Nuclei".
    *
    * @note made minor cosmetic change from 1 - (1 - Yj)Q, where Yj is a random number and Q is the coefficient.
    * @see p. 15, Formation of Planetary Systems by Aggregation: A Computer Simulation - Stephen H. Dole
    * @see method RandomEccentricity, line 128 in DoleParams.java - Ian Burrell (accrete)
    * @see method distribute_planetary_masses, line 406 in accrete.c - Mat Burdick (accrete)
    * @see method ComputePlanetStats, line 148 in Dole.c - Andrew Folkins (accretion)
    * @see method CreatePlanet, lines 203 in dole.c - Keris (accretion v1)
    * @see method CreatePlanet, line 288 in dole.cc - Keris (accretion v2)
    * @see method dist_planetary_masses, line 434 in accrete.c - Keris (starform)
    * @see method dist_planetary_masses, line 404 in accrete.c - Mat Burdick (starform)
    * @see constructor Protoplanet, line 151 in  Protoplanet.java - Carl Burke (starform)
    * @param rand - a random number generator to supply a value between 0.0 and 1.0
    * @return eccentricity between 0.0 and 1.0, essential for a planetesimal to stay in the system.
    */
  def eccentricity(using rand: Random): Double = 1.0 - Math.pow(rand.nextDouble(), aConst.ECCENTRICITY_COEFF)

  /**
    * Function to produce the semi major axis of a planetesimal. Formula taken from "Formation of Planetary Systems by Aggregation: A Computer Simulation" in
    * "III. Experimental Simulation, section b) The Planetary Nuclei".
    *
    * @note made minor cosmetic change from 50Yi, where Yi is a random number and 50 is outer limit. Innermost limit is
    *       mentioned separately originally.
    * @note unit of return value is AU
    * @see pp. 15-17, Formation of Planetary Systems by Aggregation: A Computer Simulation - Stephen H. Dole
    * @see method RandomPlanetismal, line 58 in Planetismal.java - Ian Burrell (accrete)
    * @see method distribute_planetary_masses, line 405 in accrete.c - Mat Burdick (accrete)
    * @see method ComputePlanetStats, lines 156-163 in Dole.c - Andrew Folkins (accretion)
    * @see method CreatePlanet, lines 211-223 in dole.c - Keris (accretion v1)
    * @see method CreatePlanet, line 301 in dole.cc - Keris (accretion v2)
    * @see method dist_planetary_masses, line 433 in accrete.c - Keris (starform)
    * @see method dist_planetary_masses, line 403 in accrete.c - Mat Burdick (starform)
    * @see constructor Protoplanet, line 150 in  Protoplanet.java - Carl Burke (starform)
    * @param rand  the pseudo-random number generator to use.
    * @param bands the current state of the accretion disc that the new planetismal will be inserted into.
    * @return the semi-major axis of the newly introduced planetismal.
    */
  def semiMajorAxis(nucleiCount: Int, bands: List[DustBand])(using rand: Random): Double = {
    semiMajorAxis(nucleiCount, aConst.INNERMOST_PLANET, aConst.OUTERMOST_PLANET, bands)
  }
}
