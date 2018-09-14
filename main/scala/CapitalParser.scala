import java.time.LocalDate
import java.time.format.DateTimeFormatterBuilder

import RuleParser.{toBigDecimal}

class CapitalParser {
  val dfCapital = new DateTimeFormatterBuilder().appendPattern("yyyy-MM-dd").toFormatter

  def parseTransactions(lines: Seq[String]) : Seq[Transaction] = {
    lines.flatMap(getTransactionCapital)
  }

  private def getTransactionCapital(line: String) : Option[Transaction] = {
    val ss: Seq[String] = line.trim.split(",")
    if (ss.length == 6 && !ss(0).contains("First"))
      Some(Transaction(ss(3), toBigDecimal(ss(5)), toLocalDateCapital(ss(1)),"Capital One"))
    else
      None
  }

  def toLocalDateCapital(str: String): LocalDate = {
    LocalDate.parse(str,dfCapital)

  }
}
