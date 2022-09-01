package com.szadowsz.starform.sim

import com.szadowsz.starform.profile.accrete.DoleProfile
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * 03/10/2016.
  */
class DoleSimFlatSpec extends AnyFlatSpec with Matchers {

  behavior of "The Dole Implementation of Accrete Simulation"

  it should "be able to replicate results" in {
    val acrete = DoleSimulation(new DoleProfile())
    val system1 = acrete.generateSystem()
    val system2 = acrete.generateSystem(Option(system1.seed))

    system2.seed should be (system1.seed)

    system2.planets.length should be (system1.planets.length)

    system2.planets should contain theSameElementsAs system1.planets
  }
}
