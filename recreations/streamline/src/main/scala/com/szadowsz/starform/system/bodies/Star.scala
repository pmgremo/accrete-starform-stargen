package com.szadowsz.starform.system.bodies

import scala.util.Random

/**
  * @author Zakski : 10/09/2016.
  */
object Star {


  val MINMAG: Int = -7

  /**
    * This table gives the approximate relative numbers of stars of the various spectral types and luminosity classes, the units are stars per million cubic
    * parsecs. The program totals this information and computes a cumulative distribution function from it for actual use.  The spectral classes range from O
    * on the left to M on the right, the luminosities from an absolute magnitude of -7 at the top to +16 at the bottom.  Thus, the table looks roughly like
    * the traditional Hertzsprung-Russell diagram.
    *
    * One thing you'll notice about this, there's a *lot* of red dwarfs in a realistic distribution, a star like the sun is in the top 10% of the population.
    * This makes the occurrence of habitable planets pretty low.
    *
    * Most of this information is from a message I recived from John Carr <athena.mit.edu!jfc> on April 19/88, he didn't mention his source though he did
    * make the comment that "the birthrate function is much flatter at high luminosities than the luminosity function, due to the short lifetime of high-mass
    * stars.  This is important for young areas."  I don't think that idea is accounted for here.
    */
  val starCounts: Array[Array[Double]] = Array(
    Array(2E-4, 5E-4, 3E-4, 3E-4, 3E-5, 0, 0),
    Array(5E-4, 2.5E-3, 1E-3, 1E-3, 1E-4, 4E-4, 4E-4),
    Array(.001, .025, .010, .006, .008, .004, .010),
    Array(.003, .16, .01, .016, .025, .012, .012),
    Array(.01, .50, .05, .08, .08, .1, .06),
    Array(.01, 2.5, .08, .2, .3, .6, .4),
    Array(.01, 12.5, 1, 1.6, 1, 2.5, 3),
    Array(.001, 20, 20, 2, 8, 25, 10),
    Array(0.0, 30, 100, 30, 30, 120, 10),
    Array(0.0, 20, 200, 160, 50, 110, 0),
    Array(0.0, 10, 80, 700, 150, 100, 0),
    Array(0.0, 0, 30, 1200, 700, 100, 0),
    Array(0.0, 0, 0, 600, 2000, 300, 0),
    Array(0.0, 0, 0, 200, 1500, 1500, 10),
    Array(0.0, 0, 0, 100, 800, 3000, 100),
    Array(0.0, 0, 0, 10, 400, 2500, 1000),
    Array(0.0, 0, 0, 0, 200, 1500, 3000),
    Array(0.0, 10, 0, 0, 0, 400, 8000),
    Array(0.0, 100, 30, 10, 0, 200, 9000),
    Array(0.0, 200, 400, 100, 0, 100, 10000),
    Array(0.0, 400, 600, 300, 100, 400, 10000),
    Array(0.0, 800, 1000, 1000, 600, 800, 10000),
    Array(0.0, 1500, 2000, 1000, 1500, 1200, 8000),
    Array(0.0, 3000, 5000, 3000, 3000, 0, 6000)
  )

  /**
    * Absolute magnitude - anything of number or below is that class e.g Class G, mag 5.0 is class V.  Most of this is guesstimates from a H-R diagram.
    */
  val magLumClass: Array[Array[Double]] = Array(
    /* Ia     Ib      II      III      IV      V      VI    VII */
    Array(-6.5, -6.0, -5.0, -4.0, -3.0, -1.0, 1.0, 20.0), /* O */
    Array(-6.5, -6.0, -3.5, -3.0, -2.0, 2.0, 4.0, 20.0), /* B */
    Array(-6.5, -5.0, -3.0, -0.5, 0.5, 2.5, 5.0, 20.0), /* A */
    Array(-6.5, -5.0, -2.0, 1.5, 2.5, 5.0, 9.0, 20.0), /* F */
    Array(-6.5, -5.0, -2.0, 2.5, 3.5, 7.0, 10.0, 20.0), /* G */
    Array(-6.5, -5.0, -2.5, 3.0, 5.5, 10.0, 20.0, 20.0), /* K */
    Array(-6.5, -5.0, -2.5, 2.0, 2.0, 14.0, 20.0, 20.0) /* M */
  )

  val spectralClasses: Array[Char] = Array('O', 'B', 'A', 'F', 'G', 'K', 'M')

  val luminosityClasses: Array[String] = Array("Ia", "Ib", "II", "III", "IV", "V", "VI", "VII")


  /**
    * Optimistic constant specified by Fogg in "3. Characteristics of The Primary Star" in "Extra-solar Planetary Systems: A Microcomputer Simulation", based
    * on greenhouse models by Michael H. Hart.
    *
    * @see p. 503, Extra-solar Planetary Systems: A Microcomputer Simulation - Martyn J. Fogg
    * @see var GREENHOUSE_EFFECT_CONST, line 50 in const.h - Mat Burdick (accrete)
    * @see method GenStar, line 168 in Genstar.c - Andrew Folkins (accretion)
    * @see method GenStar, line 215 in genstar.c - Keris (accretion v1)
    * @see method GenStar, lines 292 in genstar.cc - Keris (accretion v2)
    * @see var GREENHOUSE_EFFECT_CONST, line 51 in const.h - Keris (starform)
    * @see var GREENHOUSE_EFFECT_CONST, line 48 in const.h - Mat Burdick (starform)
    * @see var GREENHOUSE_EFFECT_CONST, line 97 in PhysicalConstants.java - Carl Burke (starform)
    *
    */
  val GREENHOUSE_EFFECT_CONST: Double = 0.93

  protected val cumulStarCounts: Array[(Array[(Double, Int)], Int)] = computeProbabilities()

  /**
    * Compute a cumulative distribution from the values in StarCounts[][]
    * by adding up all the values, then dividing each entry in the array
    * by the total.
    */
  protected def computeProbabilities(): Array[(Array[(Double, Int)], Int)] = {
    val total = starCounts.foldLeft(0.0) { case (tot, r) => tot + r.foldLeft(0.0) { case (rTot, cell) => rTot + cell } }
    var cumulative = 0.0
    starCounts.map(r => r.map { cell =>
      cumulative += cell
      cumulative / total
    }.zipWithIndex
    ).zipWithIndex
  }


  /**
    * function to randomise the star's age as noted in "3. Characteristics of The Primary Star" in Extra-solar Planetary Systems: A Microcomputer Simulation.
    *
    * @see Extra-solar Planetary Systems: A Microcomputer Simulation - Martyn J. Fogg
    * @see method generate_stellar_system, lines 94-97 in main.c - Mat Burdick (accrete)
    * @see method getLifetime, line 179 in genstar.cc - Keris (accretion v2)
    * @see method generate_stellar_system, line 69-74 in gensys.c - Keris (starform)
    * @see method generate_stellar_system, lines 86-89 in starform.c - Mat Burdick (starform)
    * @see method commonConstructor, lines 113-116 in  StarSystem.java - Carl Burke (starform)
    * @note unit is Byr (1.0E9 years)
    * @param rand     pseudo-random number generator interface
    * @param lifespan the lifespan the star can expect on the main sequence
    * @return the approximate age of the star in Byr
    */
  def stellarAge(rand: Random, lifespan: Double): Double = {
    if (lifespan >= 6.0) {
      rand.nextDouble() * 5.0 + 1.0
    } else {
      rand.nextDouble() * (lifespan - 1.0) + 1.0
    }
  }

  /**
    * Method to approximate the star's lifespan on the main sequence. eq. 4 in "3. Characteristics of The Primary Star" from "Extra-solar Planetary Systems: A
    * Microcomputer Simulation".
    *
    * @note unit is Byr (1.0E9 years)
    * @see p. 502, Extra-solar Planetary Systems: A Microcomputer Simulation - Martyn J. Fogg
    * @see method generate_stellar_system, line 93 in main.c - Mat Burdick (accrete)
    * @see method getLifetime, line 179 in genstar.cc - Keris (accretion v2)
    * @see method critical_limit, line 283 in accrete.c - Keris (starform)
    * @see method generate_stellar_system, line 85 in starform.c - Mat Burdick (starform)
    * @see method commonConstructor, line 112 in  Star.java - Carl Burke (starform)
    * @param stellarMass       - star's mass in terms of solar mass
    * @param stellarLuminosity - star's luminosity in terms of solar luminosity
    * @return star's Main Sequence Lifespan in terms of billon years (Byr)
    */
  def stellarMSLifespan(stellarMass: Double, stellarLuminosity: Double): Double = 10 * (stellarMass / stellarLuminosity)

  /**
    * function to calculate the mean habitable orbit around the star from "3. Characteristics of The Primary Star" in "Extra-solar Planetary Systems:
    * A Microcomputer Simulation".
    *
    * @see eq.5, p.502, Extra-solar Planetary Systems: A Microcomputer Simulation - Martyn J. Fogg
    * @see method generate_stellar_system, line 91 in main.c - Mat Burdick (accrete)
    * @see method GenStar, line 167 in Genstar.c - Andrew Folkins (accretion)
    * @see method GenStar, line 214 in genstar.c - Keris (accretion v1)
    * @see method generate_stellar_system, line 75 in gensys.c - Keris (starform)
    * @see method generate_stellar_system, line 90 in starform.c - Mat Burdick (starform)
    * @see method commonConstructor, line 118 in  Star.java - Carl Burke (starform)
    * @param stellarLuminosity star's luminosity in terms of solar luminosity
    * @return mean habitable radius in AU.
    */
  def ecosphereRadius(stellarLuminosity: Double): Double = Math.sqrt(stellarLuminosity)

  /**
    * function to calculate the closest possible habitable orbit around the star from "3. Characteristics of The Primary Star" in "Extra-solar Planetary
    * Systems: A Microcomputer Simulation".
    *
    * @see eq.6, p.503, Extra-solar Planetary Systems: A Microcomputer Simulation - Martyn J. Fogg
    * @see method generate_stellar_system, line 92 in main.c - Mat Burdick (accrete)
    * @see method GenStar, line 168 in Genstar.c - Andrew Folkins (accretion)
    * @see method GenStar, line 215 in genstar.c - Keris (accretion v1)
    * @see method generate_stellar_system, line 76 in gensys.c - Keris (starform)
    * @see method generate_stellar_system, line 91 in starform.c - Mat Burdick (starform)
    * @see method commonConstructor, line 119 in  Star.java - Carl Burke (starform)
    * @param ecosphereRadius mean habitable radius in AU.
    * @return inner habitable radius in AU.
    */
  def greenhouseRadius(ecosphereRadius: Double): Double = ecosphereRadius * GREENHOUSE_EFFECT_CONST


  def spectralClass(rand: Random): (Char, Int, Double) = {
    val rnd = rand.nextDouble()
    val (specs, mag) = cumulStarCounts.find { case (s, _) => s.exists { case (prob, spec) => prob >= rnd } }.get
    val (_, spec) = specs.find { case (prob, _) => prob >= rnd }.get

    val spcClass = spectralClasses(spec)
    val rnd2 = rand.nextDouble()
    val subClass = Math.min(9, Math.round(rnd2 * 10).toInt)
    // TODO check if approps
    val magnitude = MINMAG + mag + rnd2
    (spcClass, subClass, magnitude)
  }

  def stellarLuminosity(spcClass: Char, magnitude: Double): (Double, String) = {
    val lum = Math.pow(2.5118, 4.7 - magnitude)
    val lumIndex = magLumClass(spectralClasses.indexOf(spcClass)).indexWhere(l => l >= magnitude)
    (lum, luminosityClasses(lumIndex))
  }

  def stellarMass(rand: Random, lum: Double, lumClass: String): Double = {
    lumClass match {
      case "Ia" | "Ib" | "II" | "III" =>
        /* Supergiants & giants */
        val rnd = Math.log(lum) + (rand.nextDouble() / 5.0)
        Math.exp(rnd / 3.0)
      case "IV" | "V" | "VI" =>
        /* subgiants, dwarfs, subdwarfs */
        val rnd = Math.log(lum) + 0.1 + (rand.nextDouble() / 5.0 - 0.1)
        Math.exp(rnd / 4.1)
      case "VII" =>
        /* white dwarfs */
        0.7 * rand.nextDouble() + 0.6;
    }
  }

  //    s->r_ecos = sqrt(s->luminosity);
  //    s->r_inner = 0.93 * s->r_ecos;
  //    s->r_outer = 1.1 * s->r_ecos;  /* approximately */
  //  }
  def apply(rand: Random): Star = {
    val (specClass, specSubClass, magnitude) = spectralClass(rand)
    val (luminosity, lumClass) = stellarLuminosity(specClass, magnitude)
    val mass = stellarMass(rand, luminosity, lumClass)
    val lifespan: Double = stellarMSLifespan(mass, luminosity)
    val age: Double = stellarAge(rand, lifespan)
    val meanHabitableRadius: Double = ecosphereRadius(luminosity)
    val innerHabitableRadius: Double = greenhouseRadius(meanHabitableRadius)
    new Star(specClass, specSubClass, lumClass, magnitude, mass, luminosity, lifespan, age, innerHabitableRadius, meanHabitableRadius)
  }
}

/**
  * Abstract Outline Class to represent a star. Defines the basic members required for the starform simulation.
  *
  * @see pp. 502-503, 3. Characteristics Of The Primary Star, Extra-solar Planetary Systems: A Microcomputer Simulation - Martyn J. Fogg
  * @see struct Star, line 21 in system.h - Andrew Folkins (accretion)
  * @see struct Star, line 33 in system.h - Keris (accretion v1)
  * @see struct Star, line 72 in system.h - Keris (accretion v2)
  * @see struct StellarTypeS, in steltype.h - Keris (starform)
  * @see Star.java - Carl Burke (starform)
  * @author Zakski : 06/07/2015.
  */

case class Star(
                 spectralClass: Char,
                 spectralSubclass: Int,
                 luminosityClass: String,
                 magnitude: Double,
                 mass: Double,
                 luminosity: Double,
                 lifespan: Double,
                 age: Double,
                 innerHabitableRadius: Double,
                 meanHabitableRadius: Double
               )
