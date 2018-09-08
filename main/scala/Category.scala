case class Category(name: String) {
  var total: BigDecimal = BigDecimal(0.0)

  def addAmount(amount: BigDecimal)={
    total.+(amount)
  }
}
