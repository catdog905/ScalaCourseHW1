package converter

case class Money private (amount: BigDecimal, currency: String) {
  def +(other: Money): Money = {
    if (!isSameCurrency(other))
      throw new WrongCurrencyException()
    Money(amount + other.amount, currency)
  }

  def -(other: Money): Money = {
    if (!isSameCurrency(other))
      throw new WrongCurrencyException()
    Money(amount - other.amount, currency)
  }

  def isSameCurrency(other: Money): Boolean = currency == other.currency
}

object Money {
  import Currencies.SupportedCurrencies

  def apply(amount: BigDecimal, currency: String): Money = {
    if (amount < 0)
      throw new MoneyAmountShouldBeNonNegativeException()
    if (!SupportedCurrencies.contains(currency))
      throw new UnsupportedCurrencyException
    new Money(amount, currency)
  }
}
