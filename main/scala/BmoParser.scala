import java.time.LocalDate
import java.time.format.DateTimeFormatterBuilder

import RuleParser.{toBigDecimal}

class BmoParser {

  val df = new DateTimeFormatterBuilder().appendPattern("yyyyMMdd").toFormatter

  def parseTransactions(lines: Seq[String]) : Seq[Transaction] = {
    lines.flatMap(getTransactionBmo)
  }

  def getTransactionBmo(line: String) : Option[Transaction] = {
    val ss: Seq[String] = line.trim.split(",")
    if (ss.length == 5 && !ss(0).contains("First"))
      Some(Transaction(ss(4), toBigDecimal(ss(3)), toLocalDate(ss(2)),"BMO"))
    else
      None
  }

  def toLocalDate(str: String): LocalDate = {
    LocalDate.parse(str,df)

  }

}
