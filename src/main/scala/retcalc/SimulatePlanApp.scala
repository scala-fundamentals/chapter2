package retcalc

import scala.util.control.NonFatal

object SimulatePlanApp extends App {
  println(strMain(args))

  def strMain(args: Array[String]): String = {
    val allReturns = Returns.fromEquityAndInflationData(
      equities = EquityData.fromResource("sp500.tsv"),
      inflations = InflationData.fromResource("cpi.tsv"))

    try {
      val parsedArgs = ExceptionParseArgs(args)
      import parsedArgs._
      RetCalc.simulatePlan(
        returns = allReturns.fromUntil(fromMonth, untilMonth),
        params = retCalcParams,
        nbOfMonthsSavings = parsedArgs.nbOfMonthsSavings) match {

        case Some((capitalAtRetirement, capitalAfterDeath)) =>
          s"""
             |Capital after ${nbOfMonthsSavings / 12} years of savings:    ${capitalAtRetirement.round}
             |Capital after ${retCalcParams.nbOfMonthsInRetirement / 12} years in retirement: ${capitalAfterDeath.round}
        """.stripMargin

        case None => "Cannot calculate plan. Not enough returns for the duration considered"
      }
    } catch {
      case NonFatal(e) =>
        // comment: catches everything, hides the underlying problem
        """Please specify the arguments in the following order:
          |  from until nbOfYearsSaving nbOfYearsInRetirement netIncome currentExpenses initialCapital
        """.stripMargin
    }
  }
}

case class SimulatePlanArgs(fromMonth: String,
                            untilMonth: String,
                            retCalcParams: RetCalcParams,
                            nbOfMonthsSavings: Int)


object ExceptionParseArgs {
  def apply(args: Array[String]): SimulatePlanArgs = {
    val (from +: until +: Nil) = args(0).split(",").toList
    SimulatePlanArgs(
      from, until,
      RetCalcParams(
        nbOfMonthsInRetirement = args(2).toInt * 12,
        netIncome = args(3).toInt,
        currentExpenses = args(4).toInt,
        initialCapital = args(5).toInt),
      nbOfMonthsSavings = args(1).toInt * 12)
  }

}

object OptionParseArgs {
  def getArg(args: Array[String], i: Int): Option[String] =
    if (args.isDefinedAt(i))
      Some(args(i))
    else
      None

  def apply(args: Array[String]): Option[SimulatePlanArgs] = ???


}
