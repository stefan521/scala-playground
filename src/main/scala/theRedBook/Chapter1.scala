package theRedBook

object Chapter1 extends App {
  case class Coffee(flavour: String)

  def buyCoffee(): Coffee = Coffee("Strong")
}
