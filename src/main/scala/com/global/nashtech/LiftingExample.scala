package com.global.nashtech

object LiftingExample extends App {
  // Define a partial function to calculate the reciprocal of a non-zero Double
  val reciprocal: PartialFunction[Double, Double] = {
    case x if x != 0 => 1 / x
  }

  // we would have to check if the function is defined for the value we want to calculate reciprocal for.
  def getReciprocalMessagePartialFunction(value: Double) = {
    if (reciprocal.isDefinedAt(value)) {
      s"Square root of $value is ${reciprocal(value)}"
    } else {
      s"Cannot calculate square root for $value"
    }
  }

  //To restrain from doing this work, what we can do is we can extend the domain of our partial function to
  // accept a whole Double and would simply return a None for values for which the method is not defined.
  // Lifting the partial function to operate on the entire domain of Doubles
  val liftedReciprocal: Double => Option[Double] = reciprocal.lift

  //  Lifting the reciprocal function will extend its domain to the whole Double.
  //  From a PartialFunction[Double, Double] it will become a Function[Double, Option[Double]].
  val someValue: Double = 4
  val result: Option[Double] = liftedReciprocal(someValue)
  println(result.getOrElse(s"Cannot calculate reciprocal for $someValue")) // 0.25

  //passing invalid value
  println(liftedReciprocal(0)) // None


  //Methods To Functions

  def square(value: Int): Int = value * value

  def cube(value: Int): Int = value * value * value

  cube(square(11))

  //Lifting methods to functions explicitly
  val liftedSquare: Int => Int = square _
  val liftedCube: Int => Int = cube _

  val composedFunc: Int => Int = (liftedSquare andThen liftedCube)

  println(composedFunc(11))

}