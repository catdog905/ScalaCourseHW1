package converter

import converter.Currencies.SupportedCurrencies
import converter.Currencies.SupportedCurrencies.contains
import org.scalatest._
import flatspec._
import org.scalatest.matchers.should.Matchers

class CurrencyConverterSpec extends AnyFlatSpec with Matchers {

  "exchange" should "convert money for supported currencies" in {
    val currency1 = SupportedCurrencies.head
    val currency2 = SupportedCurrencies.tail.head
    val rates = Map(
      currency1 -> Map(currency2 -> BigDecimal(72.5)),
      currency2 -> Map(currency1 -> BigDecimal(1 / 72.5))
    )
    val converter = CurrencyConverter(rates)
    val exchangedRub = converter.exchange(Money(2, currency1), currency2)
    val exchangedUsd = converter.exchange(Money(10, currency2), currency1)
    exchangedRub.amount shouldEqual 145
    exchangedRub.currency shouldEqual currency2
    exchangedUsd.amount shouldEqual BigDecimal(1 / 7.25)
    exchangedUsd.currency shouldEqual currency1
  }

  "converted constructor" should "throw UnsupportedCurrencyException if rates dictionary contains wrong currency" in {
    val fakeCurrency = "GBP"
    val realCurrency = SupportedCurrencies.head
    val rates1 = Map(
      fakeCurrency -> Map(realCurrency -> BigDecimal(85))
    )
    val rates2 = Map(
      realCurrency -> Map(fakeCurrency -> BigDecimal(85))
    )
    assertThrows[UnsupportedCurrencyException] {
      CurrencyConverter(rates1)
    }
    assertThrows[UnsupportedCurrencyException] {
      CurrencyConverter(rates2)
    }
  }

  "exchange" should "reject to convert money with the same currency" in {
    val currency1 = SupportedCurrencies.head
    val currency2 = SupportedCurrencies.tail.head
    val rates = Map(
      currency1 -> Map(currency2 -> BigDecimal(72.5)),
      currency2 -> Map(currency1 -> BigDecimal(1 / 72.5))
    )
    val converter = CurrencyConverter(rates)
    assertThrows[WrongCurrencyException] {
      converter.exchange(Money(2, currency1), currency1)
    }
  }
}
