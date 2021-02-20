// page 83
case class Cat(name: String, colour: String, food: String)

object ChipShop {

  def willServe(cat: Cat): Boolean =
    cat.food match {
      case "chips" => true
      case _ => false
    }
}

case class Film(directorName: String)

object Dad {

  def rate(film: Film): Double =
    film.directorName match {
      case "Clint Eastwood" => 10
      case "John McTiernan" => 7
      case _ => 3
    }
}