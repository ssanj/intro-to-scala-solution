package fundamentals.level02

/**
  * These exercises introduce data types and also algebraic data types (ADTs). ADTs are a huge part of typed functional programming.
  * You will also be introduced to a very useful technique for working with ADTs, i.e. pattern matching.
  */
object TypesExercises {

  /**
    * A simple data type
    *
    * Here is an example of a `Person` type, which is a wrapper on `String` and `Int`.
    *
    * This is a "product type", i.e. a `Person` is a "product" of `String` and `Int`.
    */
  case class Person(name: String, age: Int)

  /**
    * scala> val person = Person("Bob", 50)
    * scala> showPerson1(person)
    * = "Bob is 50 years old"
    *
    * This uses a technique called pattern matching. You will see more of this later.
    **/
  def showPerson1(person: Person): String =
    person match {
      case Person(name, age) => s"${name} is ${age} years old"
    }

  /**
    * Same as `showPerson1`, but using string interpolation only.
    *
    * Hint: Navigate the Person class' fields using the "." operator
    */
  def showPerson2(person: Person): String =
    s"${person.name} is ${person.age} years old"

  /**
    * scala> val person = Person("Bob", 50)
    * scala> changeName("Bobby", person)
    * = Person("Bobby", 50)
    *
    * `person` is immutable! This function returns a new instance of `Person` with the `name` changed.
    * Check out the corresponding test in `TypesExercisesTest` to understand why.
    *
    * Hint: Use the .copy method
    */
  def changeName(newName: String, person: Person): Person = person.copy(name = newName)

  /**
    * Let's look at another data type.
    *
    * `Wallet` is a tiny type on `Double` to represent the amount of money someone has.
    */

  case class Wallet(amount: Double)

  /**
    * scala> val wallet = Wallet(20.5)
    * scala> showWallet(wallet)
    * = "The wallet amount is 20.5"
    *
    * You can solve this like how you solved `showPerson1` or `showPerson2`.
    */
  def showWallet(wallet: Wallet): String = s"The wallet amount is ${wallet.amount}"

  /**
    * Here is another example of working with immutable values.
    *
    * scala> val wallet = Wallet(100)
    * scala> purchase(80, wallet)
    * = Wallet(20)
    **/
  def purchase(cost: Double, wallet: Wallet): Wallet = wallet.copy(amount = wallet.amount - cost)

  /**
    * scala> showTrafficLightStr("red")
    * = "The traffic light is red"
    *
    * scala> showTrafficLightStr("yellow")
    * = "The traffic light is yellow"
    *
    * scala> showTrafficLightStr("green")
    * = "The traffic light is green"
    *
    * What if `trafficLight` is not "red", "yellow" or "green"?
    **/
  def showTrafficLightStr(trafficLight: String): String = trafficLight match {
    case colour@("red" | "yellow" | "green") => s"The traffic light is $colour"
    case light =>
      val values = light.split(" ")
        values match {
          case Array("flashing", freq) => s"The traffic light is flashing with a frequency of ${freq}"
          case _ => s"unknown trafficLight colour"
        }
  }

  val flashing = """^flashing\s(\d+)$""".r

  /**
    * We have a new traffic light called Flashing, with a frequency, e.g. "flashing 20", "flashing 100"
    *
    * Extend `showTrafficLightStr` that you have just implemented above to support this new functionality.
    *
    * Use a test driven approach to implement this new functionality.
    *
    * scala> showTrafficLightStr("flashing 20")
    * = "The traffic light is flashing with a frequency of 20"
    *
    * scala> showTrafficLightStr("flashing 100")
    * = "The traffic light is flashing with a frequency of 100"
    **/

  /**
    * A "sum type" represents more than one possible value.
    *
    * You can read the following as a `TrafficLight` is either `Red` or `Yellow` or `Green`.
    *
    * A sealed trait can only be extended in the same file that it is defined.
    */
  sealed trait TrafficLight

  object TrafficLight {

    case object Red extends TrafficLight

    case object Yellow extends TrafficLight

    case object Green extends TrafficLight

    case class Flashing(frequency: Int) extends TrafficLight

  }

  /**
    * scala> showTrafficLight(Red)
    * = "The traffic light is red"
    *
    * scala> showTrafficLight(Yellow)
    * = "The traffic light is yellow"
    *
    * scala> showTrafficLight(Green)
    * = "The traffic light is green"
    *
    * It is impossible to get an invalid TrafficLight as input
    *
    * Hint: Use pattern matching
    **/
    def showTrafficLight(trafficLight: TrafficLight): String = trafficLight match {
      case TrafficLight.Red            => s"The traffic light is red"
      case TrafficLight.Yellow         => s"The traffic light is yellow"
      case TrafficLight.Green          => s"The traffic light is green"
      case TrafficLight.Flashing(freq) => s"The traffic light is flashing with a frequency of $freq"
    }

  /**
    * Now introduce a new type of `TrafficLight` called `Flashing` that has an additional parameter, `frequency: Int`
    *
    * What happens when you try to compile now?
    */

  /**
    * Bonus: How would you create an ADT that represents the commands from our Toy Robot code test?
    *
    * As a refresher, the possible commands are:
    *
    * PLACE X,Y,DIR (where X and Y are Ints and DIR is one of NORTH, SOUTH, EAST and EAST)
    * MOVE
    * LEFT
    * RIGHT
    * REPORT
    */
   sealed trait DIR
   case object NORTH extends DIR
   case object SOUTH extends DIR
   case object EAST  extends DIR
   case object WEST  extends DIR

   sealed trait Command
   case class PLACE(x: Int, y: Int, dir: DIR) extends Command
   case object MOVE   extends Command
   case object LEFT   extends Command
   case object RIGHT  extends Command
   case object REPORT extends Command

}
