package learningScala

object Chapter3ExpressionsAndConditionals {
  def checkName(name: String): String = name match {
    case null => "n/a"
    case n if n.isEmpty => "n/a"
    case n => n
  }

  def compareWithZero(amount: Double): String = {
    if (amount == 0.0) "same"
    else if (amount > 0.0) "greater"
    else "less"
  }

  def convertColor(colorName: String): String = {
    require(Set("magenta", "cyan", "yellow").contains(colorName))
    colorName match {
      case "magenta" => "#FF00FF"
      case "cyan" => "#00FFFF"
      case "yellow" => "#FFFF00"
    }
  }

  def printToHundred(): Unit = {
    for { x <- 1 to 100 by 5 } {
      for { y <- x until x + 5 } {
        print(s"$y, ")
      }

      println()
    }
  }

  def makeHundredTypeSafe(): IndexedSeq[String] = {
    for(i <- 1 to 100)
      yield if(i % 15 == 0) {
        "typeSafe"
      } else if(i % 3 == 0) {
        "type"
      } else if (i % 5 == 0) {
        "safe"
      } else {
        i.toString
      }
  }
}
