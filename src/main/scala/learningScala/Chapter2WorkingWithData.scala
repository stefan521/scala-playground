package learningScala

object Chapter2WorkingWithData {
  def payMeBack(amount: Double): String = {
      if(amount <= 0) "You don't owe me anything."
      else s"You owe me $$${amount}"
  }

  /**
   * Finds Frank's phone number in a string representing his address.
   * The String must separated my commas and contain the following:
   * Name, Street Name and Street Number, Phone Number, Postcode
   *
   * @param franksAddress Frank's full address
   * @return Frank's phone number
   */
  def callFrank(franksAddress: String): String = {
    val pattern = """[\w ]* *, *[\w ]* *, *([\d-]+) *, *[\w]*""".r
    val pattern(phone) = franksAddress

    phone
  }
}
