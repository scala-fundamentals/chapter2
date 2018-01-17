package retcalc

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{Matchers, WordSpec}

class SimulatePlanAppIT extends WordSpec with Matchers with TypeCheckedTripleEquals {
  "SimulatePlanApp.strMain" should {
    "simulate a retirement plan using market returns" in {
      val actualResult = SimulatePlanApp.strMain(
        Array("1952.09,2017.09", "25", "40", "3000", "2000", "10000"))

      val expectedResult =
        s"""
          |Capital after 25 years of savings:    468925
          |Capital after 40 years in retirement: 2958842
        """.stripMargin
      actualResult should === (expectedResult)
    }

    "return an error message if one of the arguments is missing or is not a number" in {
      val actualResult1 = SimulatePlanApp.strMain(
        Array("1997.09,2017.09", "25", "40", "3000", "2000"))
      val actualResult2 = SimulatePlanApp.strMain(
        Array("1997.09,2017.09", "not_a_number", "40", "3000", "2000", "10000"))

      val expectedResult =
        """Please specify the arguments in the following order:
          |  from until nbOfYearsSaving nbOfYearsInRetirement netIncome currentExpenses initialCapital
        """.stripMargin
      actualResult1 should === (expectedResult)
      actualResult2 should === (expectedResult)
    }
  }
}
