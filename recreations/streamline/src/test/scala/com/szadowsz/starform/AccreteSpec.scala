package com.szadowsz.starform

import com.szadowsz.starform.model.SimConstants
import com.szadowsz.starform.model.accrete.PlanetesimalCalc
import com.szadowsz.starform.model.star.StarCalc
import com.szadowsz.starform.system.bodies.ProtoPlanet
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class AccreteSpec extends AnyFunSpec with Matchers {

  describe("planets should be predictable") {
    val seed = 1_662_642_772_940L
    val rand = new Random(seed)
    val star = new StarCalc().initStar(rand)
    val constants = SimConstants(None, None, None, None, None)
    val pCalc = PlanetesimalCalc(constants)
    pCalc.setStar(star)

    val expected = List(
      new ProtoPlanet(pCalc, 8.371749837659404E-8, 0.3390933249317554, 0.10106651621130835),
      new ProtoPlanet(pCalc, 4.863505541842775E-7, 0.44376793676709037, 0.053680867341699856),
      new ProtoPlanet(pCalc, 8.817900257131791E-7, 0.601365192342602, 0.20244380342674162),
      new ProtoPlanet(pCalc, 3.095709568613821E-6, 1.1645157524247152, 0.022041420423541958),
      new ProtoPlanet(pCalc, 1.629925834187789E-7, 1.5795588835068475, 0.16386246013588868),
      new ProtoPlanet(pCalc, 6.068714320468268E-6, 2.5707989626461885, 0.20451189448099338),
      new ProtoPlanet(pCalc, 6.831980022686899E-5, 3.876887054369008, 0.042606769835431435),
      new ProtoPlanet(pCalc, 3.124965068750245E-6, 6.4997216987196325, 0.15050692253165865),
      new ProtoPlanet(pCalc, 1.3276774618659919E-5, 9.245692171694333, 0.06173891990099745),
      new ProtoPlanet(pCalc, 1.2655082517252537E-4, 16.55078999459937, 0.18685203105346337),
      new ProtoPlanet(pCalc, 1.1421007704574817E-6, 31.92654280400159, 0.011145973784941532),
      new ProtoPlanet(pCalc, 3.418401578181737E-7, 46.58307264270429, 0.08947556749976769)
    )

    val starform = new StarformSimulation(constants)
    val system = starform.generateSystem(Some(seed))
    expected.zip(system.planets).foreach {
      case (e, a) =>
        e.axis should be(a.axis)
        e.mass should be(a.mass)
        e.ecc should be(a.ecc)
    }
  }
}
