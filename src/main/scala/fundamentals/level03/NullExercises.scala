package fundamentals.level03

import fundamentals.level02.TypesExercises.{Person, TrafficLight, changeName, flashing}
import TrafficLight._

/**
  * These exercises are intended to show the problems that come with programming with `null`s.
  *
  * After these exercises, we will learn the alternative to using `null`s.
  */
object NullExercises {

  /**
    * scala> mkTrafficLightOrNull("red")
    * = Red
    *
    * scala> mkTrafficLightOrNull("bob")
    * = null
    **/
  def mkTrafficLightOrNull(str: String): TrafficLight = str match {
    case "red"          => Red
    case "yellow"       => Yellow
    case "green"        => Green
    case flashing(freq) => Flashing(freq.toInt)
    case _              => null
  }

  /**
    * scala> mkTrafficLightOrNullThenShow("red")
    * = "Traffic light is red"
    *
    * scala> mkTrafficLightOrNullThenShow("bob")
    * = "Traffic light is invalid"
    *
    * Hint: Use `mkTrafficLightOrNull` and pattern matching
    */
  def mkTrafficLightOrNullThenShow(str: String): String = mkTrafficLightOrNull(str) match {
    case colour@(Red | Yellow | Green) => s"Traffic light is ${colour.toString.toLowerCase}"
    case Flashing(freq) => s"Traffic light is flashing with a frequency of $freq"
    case _ => "Traffic light is invalid"
  }

  /**
    * scala> mkPersonOrNull("Bob", 20)
    * = Person("Bob", 20)
    *
    * If `name` is blank:
    *
    * scala> mkPersonOrNull("", 20)
    * = null
    *
    * If `age` < 0:
    *
    * scala> mkPersonOrNull("Bob", -1)
    * = null
    **/
  def mkPersonOrNull(name: String, age: Int): Person = {
    if (name.trim.isEmpty || age < 0) null
    else Person(name, age)
  }

  /**
    * scala> mkPersonOrNullThenChangeName("Bob", 20, "John")
    * = Person("John", 20)
    *
    * scala> mkPersonOrNullThenChangeName("Bob", -1, "John")
    * = null
    *
    * For simplicity, let's allow changing to an empty name, like:
    *
    * scala> mkPersonOrNullThenChangeName("Bob", 20, "")
    * = Person("", 20)
    *
    * Hint: Use `mkPersonOrNull` and `changeName`
    **/
  def mkPersonOrNullThenChangeName(oldName: String, age: Int, newName: String): Person = {
    val person = mkPersonOrNull(oldName, age)
    if (person != null) changeName(newName, person)
    else person
  }

  /**
    * Does the following function return a `null`?
    * Don't know!
    */
  def mean(nums: List[Int]): Double = ???

}
