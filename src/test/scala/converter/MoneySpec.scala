package converter

import org.scalatest._
import flatspec._
import org.scalatest.matchers.should.Matchers
class MoneySpec extends AnyFlatSpec with Matchers {
  import Currencies.SupportedCurrencies

  "money" should "instantiates with correct arguments" in {
    Money(42, SupportedCurrencies.head)
  }

  "money" should "throw MoneyAmountShouldBeNonNegativeException if amount is below 0 in constructor" in {
    assertThrows[MoneyAmountShouldBeNonNegativeException] {
      Money(-1, SupportedCurrencies.head)
    }
  }

  "money" should "throw MoneyAmountShouldBeNonNegativeException if amount is below 0 after subtraction" in {
    val money1 = Money(22, SupportedCurrencies.head)
    val money2 = Money(42, SupportedCurrencies.head)
    assertThrows[MoneyAmountShouldBeNonNegativeException] {
      money1 - money2
    }
  }

  "money" should "throw UnsupportedCurrencyException if it obtains unsupported currency" in {
    val fakeCurrency = "USDT"
    SupportedCurrencies.contains(fakeCurrency) shouldEqual false
    assertThrows[UnsupportedCurrencyException] {
      Money(42, fakeCurrency)
    }
  }

  "money" should "instantiates successfully if amount is 0" in {
    Money(0, SupportedCurrencies.head)
  }

  "money value" should "increases after summation" in {
    val money1 = Money(42, SupportedCurrencies.head)
    val money2 = Money(22, SupportedCurrencies.head)
    val result = money1 + money2
    result.amount shouldEqual 42 + 22
  }

  "money" should "reject summation of different currencies" in {
    val money1 = Money(42, SupportedCurrencies.head)
    val money2 = Money(22, SupportedCurrencies.tail.head)
    assertThrows[WrongCurrencyException] {
      money1 + money2
    }
  }

  "money value" should "decreases after subtraction" in {
    val money1 = Money(42, SupportedCurrencies.head)
    val money2 = Money(22, SupportedCurrencies.head)
    val result = money1 - money2
    result.amount shouldEqual 42 - 22
  }

  "money" should "reject subtraction of different currencies" in {
    val money1 = Money(42, SupportedCurrencies.head)
    val money2 = Money(22, SupportedCurrencies.tail.head)
    assertThrows[WrongCurrencyException] {
      money1 - money2
    }
  }

  "money" should "check currency equality correctly" in {
    val money1 = Money(42, SupportedCurrencies.head)
    val money2 = Money(42, SupportedCurrencies.head)
    val money3 = Money(256, SupportedCurrencies.tail.head)
    money1.isSameCurrency(money2) shouldEqual true
    money2.isSameCurrency(money3) shouldEqual false
    money3.isSameCurrency(money1) shouldEqual false
  }
}
