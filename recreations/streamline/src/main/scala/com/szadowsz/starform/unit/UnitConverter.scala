package com.szadowsz.starform.unit

/**
  * Utility method to convert from one unit of measurement to another.
  *
  * Function names description what is being convert to/from.
  *
  * @author Zakski : 03/07/2015.
  */
object UnitConverter extends UnitConstants {

  def cmToKm(centimeters: Double): Double = centimeters / CM_PER_KM

  def cmToEarthRadius(centimeters: Double): Double = centimeters / EARTH_RADIUS_IN_CM

  def mToKm(meters: Double): Double = meters / METERS_PER_KM

  def kmToM(kilograms: Double): Double = kilograms * METERS_PER_KM

  def kmToCm(kilometers: Double): Double = kilometers * CM_PER_KM

  def earthMassToSolarMass(earthMass: Double): Double = earthMass * (1/SOLAR_MASS_IN_EARTH_MASS)

  def solarMassToEarthMass(sunMass: Double): Double = sunMass * SOLAR_MASS_IN_EARTH_MASS

  def solarMassToKg(sunMass: Double): Double = sunMass * SOLAR_MASS_IN_KG

  def solarMassToGrams(sunMass: Double): Double = sunMass * SOLAR_MASS_IN_GRAMS

  def kgToSolarMass(kg: Double): Double = kg / SOLAR_MASS_IN_KG

  def daysToHours(days: Double): Double = days * HOURS_PER_DAY

  def byrToYears(byrs : Double): Double = byrs * BILLION_YEARS

  def radSecToHoursPerRotation(radSec: Double): Double = (2 * Math.PI) / (SECONDS_PER_HOUR * radSec)

  def mSec2ToGs(acceleration: Double): Double = acceleration / EARTH_GRAVITY

  def gsToMetreSec2(acceleration: Double): Double = acceleration * EARTH_GRAVITY

  def millibarsToBars(millibars : Double): Double  = millibars / MILLIBARS_PER_BAR

  def barsToMillibars(bars : Double): Double  = bars * MILLIBARS_PER_BAR

  def kelvinToCelsius(kelvin : Double) : Double = kelvin - KELVIN_CELSIUS_DIFFERENCE

  def celsiusToKelvin(celsius : Double) : Double = celsius + KELVIN_CELSIUS_DIFFERENCE
}
