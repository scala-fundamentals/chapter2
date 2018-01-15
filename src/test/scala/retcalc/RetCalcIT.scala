package retcalc

import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.{Matchers, WordSpec}

class RetCalcIT extends WordSpec with Matchers with TypeCheckedTripleEquals {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.0001)

  val params = RetCalcParams(
    nbOfMonthsInRetirement = 40 * 12,
    netIncome = 3000,
    currentExpenses = 2000,
    initialCapital = 10000)


  "simulate a retirement plan with real market data" in {
    val returns = Returns.fromEquityAndInflationData(
      equities = EquityData.fromResource("sp500.tsv"),
      inflations = InflationData.fromResource("cpi.tsv")).fromUntil("1997.09", "2017.10")

    val (capitalAtRetirement, capitalAfterDeath) = RetCalc.simulatePlan(returns, params = params, nbOfMonthsSavings = 25 * 12)
    capitalAtRetirement should ===(500945.3646)
    capitalAfterDeath should ===(746172.4565)
  }
}
