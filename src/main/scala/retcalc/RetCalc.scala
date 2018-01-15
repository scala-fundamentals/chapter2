package retcalc

import scala.annotation.tailrec

case class RetCalcParams(nbOfMonthsInRetirement: Int,
                         netIncome: Int,
                         currentExpenses: Int,
                         initialCapital: Double)


case class MultiSimResults(successCount: Int,
                           simCount: Int,
                           minCapitalAfterDeath: Double,
                           maxCapitalAfterDeath: Double) {
  def successProbability: Double = successCount.toDouble / simCount
}

object RetCalc {

  def simulatePlan(returns: Returns, params: RetCalcParams, nbOfMonthsSavings: Int,
                   monthOffset: Int = 0): (Double, Double) = {
    import params._
    val capitalAtRetirement = futureCapital(
      returns = OffsetReturns(returns, monthOffset),
      nbOfMonths = nbOfMonthsSavings, netIncome = netIncome, currentExpenses = currentExpenses,
      initialCapital = initialCapital)

    val capitalAfterDeath = futureCapital(
      returns = OffsetReturns(returns, monthOffset + nbOfMonthsSavings),
      nbOfMonths = nbOfMonthsInRetirement,
      netIncome = 0, currentExpenses = currentExpenses,
      initialCapital = capitalAtRetirement)

    (capitalAtRetirement, capitalAfterDeath)
  }


  def nbOfMonthsSaving(params: RetCalcParams, returns: Returns): Int = {
    import params._
    @tailrec
    def loop(months: Int): Int = {
      val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(returns, params, months)

      if (capitalAfterDeath > 0.0)
        months
      else
        loop(months + 1)
    }

    if (netIncome > currentExpenses)
      loop(0)
    else
      Int.MaxValue
  }

  def futureCapital(returns: Returns, nbOfMonths: Int, netIncome: Int, currentExpenses: Int,
                    initialCapital: Double): Double = {
    val monthlySavings = netIncome - currentExpenses
    (0 until nbOfMonths).foldLeft(initialCapital) {
      case (accumulated, month) =>
        accumulated * (1 + Returns.monthlyRate(returns, month)) + monthlySavings
    }
  }

  def multiSim(params: RetCalcParams, nbOfMonthsSavings: Int, variableReturns: VariableReturns): MultiSimResults = {
    variableReturns.returns.indices.foldLeft(MultiSimResults(0, 0, Double.PositiveInfinity, Double.NegativeInfinity)) {
      case (acc, i) =>
        val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(variableReturns, params, nbOfMonthsSavings, i)
        MultiSimResults(
          successCount = if (capitalAfterDeath > 0) acc.successCount + 1 else acc.successCount,
          simCount = i + 1,
          minCapitalAfterDeath = if (capitalAfterDeath < acc.minCapitalAfterDeath) capitalAfterDeath else acc.minCapitalAfterDeath,
          maxCapitalAfterDeath = if (capitalAfterDeath > acc.maxCapitalAfterDeath) capitalAfterDeath else acc.maxCapitalAfterDeath)
    }
  }
}
