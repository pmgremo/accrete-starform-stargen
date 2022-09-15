package com.szadowsz.starform.model.accrete

import com.szadowsz.starform.model.SimulationStats
import com.szadowsz.starform.system.StarSystem
import com.szadowsz.starform.system.bodies.{DustBand, Planet, ProtoPlanet}

import java.lang.Math.*
import java.lang.System.Logger.Level.{DEBUG, INFO}
import scala.util.Random

/**
  * Abstract Simulation Class that attempts to recreated the procedures detailed by Stephen H. Dole in "Formation of Planetary Systems by Aggregation: A
  * Computer Simulation".
  *
  */
abstract class AccreteSimulation(protected val aConsts: AccreteConstants) {

  protected val logger: System.Logger = System.getLogger(getClass.getName)

  /**
    * the placement strategy to use when inserting new planetismals.
    */
  protected lazy val iStrat: InsertionStrat = InsertionStrat(aConsts)

  /**
    * calculations innately tied to the protoplanets
    */
  protected lazy val pCalc: PlanetesimalCalc = PlanetesimalCalc(aConsts)

  /**
    * calculations to work out new protoplanet info after a collision.
    */
  protected lazy val colCalc: CollisionCalc = CollisionCalc(pCalc)

  /**
    * The random number generator to use throughout the process.
    */
  protected val rand: Random

  /**
    * the accretion code to use when hoovering up dust.
    */
  protected def accCalc: AccreteCalc


  /**
    * The statistics recorder for the accretion process.
    */
  protected var stats: SimulationStats = _

  /**
    * Representation of the dust cloud that the planetismals form from.
    */
  protected var dust: List[DustBand] = _

  /**
    * Function to initialise a new instance at the beginning of each run.
    *
    * @return a new [[SimulationStats]] instance.
    */
  protected def initStats(): SimulationStats


  /**
    * Function to generate the Planets via accretion. Separated out to allow for the generation of an atmosphere introduced in Martyn J. Fogg's paper.
    *
    * @return a new list of [[Planet]] instances.
    */
  protected def generatePlanets(): List[Planet]

  /**
    * Function to initialise a new solar system instance at the end of each run.
    *
    * @return a new [[StarSystem]] instance.
    */
  protected def createSystem(seed: Long, stats: SimulationStats, planets: List[Planet]): StarSystem

  /**
    * Method to generate a new [[ProtoPlanet]]. Varies from implementation to implementation.
    *
    * @param mass starting mass of the protoplanet in solar mass.
    * @param axis semi-major axis of the orbit in AU.
    * @param ecc  eccentrisity on a scale of 0 to 1
    * @return a freshly made proto-planet.
    */
  final protected def createProtoplanet(mass: Double, axis: Double, ecc: Double): ProtoPlanet = new ProtoPlanet(pCalc, mass, axis, ecc)

  /**
    * Steps through list of dust bands checking to see if any of those that bands that overlap the given range have dust present.
    *
    * This is used in two situations:
    *
    * 1: Checking the end conditions of the experiment, that all dust between two arbitrary radii is swept away. This is taken from "III. Experimental
    * Simulation" in "Formation of Planetary Systems by Aggregation: A Computer Simulation"; The inner and outer bounds of where a planet can spawn are
    * used by Dole as these radii, which has been adopted by several subsequent implementations.
    *
    * 2: To check whether an injection at a given AU is possible, based on the curretn dust banding situation.
    *
    * @param inner the inner limit of the range in AU.
    * @param outer the outer limit of the range in AU.
    * @return whether or not there is still dust between inner and outer limits in this current accretion process.
    */
  final protected def isDustAvailable(inner: Double, outer: Double): Boolean = dust.exists(d => d.hasDust && d.outerEdge > inner && d.innerEdge < outer)


  /**
    * Function to turn all available DustBands into mass for the Planetismal
    *
    * @see method AccreteDust, line 154 in Accrete.java - Ian Burrell (accrete)
    * @see method accrete_dust, line 269 in accrete.c - Mat Burdick (accrete)
    * @see method EvolvePlanet, line 347 in Dole.c - Andrew Folkins (accretion)
    * @see method EvolvePlanet, line 421 in dole.c - Keris (accretion v1)
    * @see method EvolvePlanet, line 515 in dole.cc - Keris (accretion v2)
    * @see method accrete_dust, line 294 in accrete.c - Keris (starform)
    * @see method accrete_dust, line 268 in accrete.c - Mat Burdick (starform)
    * @see method accrete_dust, line 190 in  DustDisc.java - Carl Burke (starform)
    * @param proto newly coalesced proto-planet
    * @param bands the current band list.
    * @return the new calculated mass
    */
  final protected def accreteDust(bands: List[DustBand], proto: ProtoPlanet): Double = {
    val innerSweep = max(proto.innerLimit, 0.0)
    val outerSweep = proto.outerLimit

    bands
      .filter(x => x.outerEdge > innerSweep && x.innerEdge < outerSweep)
      .map(x => {
        var density = if (x.hasDust) accCalc.dustDensity(proto.axis) else 0.0
        if (x.hasGas && proto.isGasGiant) density = accCalc.dustAndGasDensity(density, proto.criticalMass, proto.mass)
        density * accCalc.bandVolume(proto.mass, proto.axis, proto.ecc, innerSweep, outerSweep, x.innerEdge, x.outerEdge)
      })
      .sum
  }

  /**
    * Function to calculate the mass for the Planetismal. Sucks mass from all dust bands in range of its gravitational pull during its elliptical orbit.
    *
    * @see method AccreteDust, line 154 in Accrete.java - Ian Burrell (accrete)
    * @see method accrete_dust, line 269 in accrete.c - Mat Burdick (accrete)
    * @see method EvolvePlanet, line 347 in Dole.c - Andrew Folkins (accretion)
    * @see method EvolvePlanet, line 421 in dole.c - Keris (accretion v1)
    * @see method EvolvePlanet, line 515 in dole.cc - Keris (accretion v2)
    * @see method accrete_dust, line 294 in accrete.c - Keris (starform)
    * @see method accrete_dust, line 268 in accrete.c - Mat Burdick (starform)
    * @see method accrete_dust, line 190 in  DustDisc.java - Carl Burke (starform)
    * @param proto newly coalesced proto-planet
    */
  final protected def accreteDust(proto: ProtoPlanet): ProtoPlanet = {
    while ( {
      val lastMass = proto.mass
      val nextMass = accreteDust(dust, proto)
      proto.mass = max(nextMass, lastMass)
      accCalc.shouldAccreteContinue(lastMass, nextMass)
    }) ()
    proto
  }

  /**
    * Function merges neighbouring dust lanes that have the same characteristics after an inserted planet has accreted dust/gas.
    *
    * @note Folkins' code line does not merge bands.
    * @see method CompressDustLanes, line 285 in Accrete.java - Ian Burrell (accrete)
    * @see method update_dust_lanes, line 96 in accrete.c - Mat Burdick (accrete)
    * @see method update_dust_lanes, line 120 in accrete.c - Keris (starform)
    * @see method update_dust_lanes, line 95 in accrete.c - Mat Burdick (starform)
    * @see method update_dust_lanes, line 141 in  DustDisc.java - Carl Burke (starform)
    * @param xs un merged dust bands.
    * @return the updated dust band list.
    */
  def merge(xs: List[DustBand]): List[DustBand] = {
    xs match {
      case Nil => Nil
      case _ :: Nil => xs
      case f :: s :: r if f.canMerge(s) =>
        merge(DustBand(f.innerEdge, s.outerEdge, f.hasDust, f.hasGas) :: r)
      case h :: t =>
        h :: merge(t)
    }
  }

  def split(ds: List[DustBand], p: ProtoPlanet, retainGas: Boolean): List[DustBand] = {
    ds match {
      case Nil => Nil
      case h :: t if p.innerLimit > h.outerEdge || p.outerLimit < h.innerEdge =>
        h :: split(t, p, retainGas)
      case h :: t if p.innerLimit > h.innerEdge =>
        DustBand(h.innerEdge, p.innerLimit, h.hasDust, h.hasGas) :: split(DustBand(p.innerLimit, h.outerEdge, h.hasDust, h.hasGas) :: t, p, retainGas)
      case h :: t if p.outerLimit < h.outerEdge =>
        DustBand(h.innerEdge, p.outerLimit, false, hasGas = h.hasGas && retainGas) :: DustBand(p.outerLimit, h.outerEdge, h.hasDust, h.hasGas) :: t
      case h :: t =>
        DustBand(h.innerEdge, h.outerEdge, false, hasGas = h.hasGas && retainGas) :: split(t, p, retainGas)
    }
  }

  /**
    * Function to Update (Split/Merge) dust bands after an inserted planet has accreted dust/gas.
    *
    * @see method UpdateDustLanes, line 217 in Accrete.java - Ian Burrell (accrete)
    * @see method update_dust_lanes, line 96 in accrete.c - Mat Burdick (accrete)
    * @see method update_bands, line 305 in Dole.c - Andrew Folkins (accretion)
    * @see method update_bands, line 367 in dole.c - Keris (accretion v1)
    * @see method update_bands, line 460 in dole.cc - Keris (accretion v2)
    * @see method update_dust_lanes, line 120 in accrete.c - Keris (starform)
    * @see method update_dust_lanes, line 95 in accrete.c - Mat Burdick (starform)
    * @see method update_dust_lanes, line 141 in  DustDisc.java - Carl Burke (starform)
    * @param proto newly coalesced proto-planet
    */
  final protected def updateDustLanes(proto: ProtoPlanet): Unit = {
    logger.log(DEBUG, "Updating Dust Lanes")
    dust = split(dust, proto, !proto.isGasGiant)
    dust = merge(dust)
  }

  private def coalesce(existing: ProtoPlanet, newcomer: ProtoPlanet): ProtoPlanet = {
    logger.log(INFO, "Collision between planetesimals {0} AU and {1} AU", newcomer.axis, existing.axis)
    stats = stats.mergeNuclei
    val new_mass: Double = existing.mass + newcomer.mass
    val new_axis: Double = colCalc.coalesceAxis(existing.mass, existing.axis, newcomer.mass, newcomer.axis)
    val new_ecc: Double = colCalc.coalesceEccentricity(existing.mass, existing.axis, existing.ecc, newcomer.mass, newcomer.axis, newcomer.ecc, new_axis)

    accreteDust(new ProtoPlanet(pCalc, new_mass, new_axis, new_ecc))
  }

  private def toClose(p: ProtoPlanet, newcomer: ProtoPlanet) = {
    (p.axis > newcomer.axis && (p.innerGravLimit < newcomer.axis || newcomer.outerGravLimit > p.axis)) ||
      (p.axis <= newcomer.axis && (p.outerGravLimit > newcomer.axis || newcomer.innerGravLimit < p.axis))
  }

  def inject[T](xs: List[T], x: T, compare: (T, T) => Boolean, merge: (T, T) => T): List[T] = xs match {
    case Nil => List(x)
    case h :: t => if (compare(h, x)) merge(h, x) :: t else h :: inject(t, x, compare, merge)
  }

  /**
    * Function to form protoplanets by accretion. Main accretion loop.
    *
    * @see method DistributePlanets, line 89 in Accrete.java - Ian Burrell (accrete)
    * @see method dist_planetary_masses, line 393 in accrete.c - Mat Burdick (accrete)
    * @see method CreatePlanet, line 137 & method CreateSystem, line 457 in Dole.c - Andrew Folkins (accretion)
    * @see method CreatePlanet, line 190 & method CreateSystem, line 560 in dole.c - Keris (accretion v1)
    * @see method CreatePlanet, line 278 & method CreateSystem, line 664 in dole.cc - Keris (accretion v2)
    * @see method dist_planetary_masses, line 419 in accrete.c - Keris (starform)
    * @see method dist_planetary_masses, line 392 in accrete.c - Mat Burdick (starform)
    * @see method dist_planetary_masses, line 145 in  Protosystem.java - Carl Burke (starform)
    */
  final protected def accrete(): List[ProtoPlanet] = {
    logger.log(DEBUG, "Initialising Statistics Recorder")
    var planetismals: List[ProtoPlanet] = Nil
    dust = List(DustBand(0.0, accCalc.outerDustLimit(1.0))) // TODO outerDustLimit function goes against the spirit of the base sim and needs to be refactored.

    while (isDustAvailable(aConsts.INNERMOST_PLANET, aConsts.OUTERMOST_PLANET)) {
      val axis = iStrat.semiMajorAxis(rand, stats.injectedNuclei, dust)
      val ecc = iStrat.eccentricity(rand)
      val proto = createProtoplanet(aConsts.PROTOPLANET_MASS, axis, ecc)
      stats = stats.injectNuclei

      logger.log(INFO, "Injecting protoplanet at {0} AU.", proto.axis)

      accreteDust(proto)

      if (proto.mass > aConsts.PROTOPLANET_MASS) {
        logger.log(DEBUG, "Checking for collisions.")
        planetismals = inject(planetismals, proto, toClose, coalesce).sortWith(_.axis < _.axis)
        updateDustLanes(proto)
      } else {
        logger.log(DEBUG, "Injection of protoplanet at {0} AU failed due to large neighbor.", proto.axis)
      }
    }
    planetismals
  }

  /**
    * Function to form a star system by accretion. The main entry point of the generation engine.
    *
    * @see method DistributePlanets, line 89 in Accrete.java - Ian Burrell (accrete)
    * @see method generate_stellar_system, line 84 in main.c - Mat Burdick (accrete)
    * @see method CreateSystem, line 457 in Dole.c - Andrew Folkins (accretion)
    * @see method GenerateSystem, line 164 in main.c - Keris (accretion v1)
    * @see method GenerateSystem, line 514 in main.cc - Keris (accretion v2)
    * @see method generate_stellar_system, line 47 in gensys.c - Keris (starform)
    * @see method generate_stellar_system, line 76 in starform.c - Mat Burdick (starform)
    * @see method Initialize, line 57 in  StarSystem.java - Carl Burke (starform)
    * @param seedOpt optional seed
    * @return the generated solar system.
    */
  final def generateSystem(seedOpt: Option[Long] = None): StarSystem = {
    logger.log(DEBUG, "Initialising Statistics Recorder")
    stats = initStats()

    val seed = seedOpt.getOrElse(System.currentTimeMillis())
    rand.setSeed(seed)
    logger.log(DEBUG, "Setting Star System Seed to {0}", seed)

    logger.log(INFO, "Beginning Protoplanet Generation for {0}", seed)

    val planets = generatePlanets()
    stats = stats.finished

    logger.log(INFO, "Finished Protoplanet Generation for {0} in {1}ms", seed, stats.timeElapsed)
    createSystem(seed, stats, planets)
  }
}