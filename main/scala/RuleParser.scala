import java.time.LocalDate

import scala.collection.Map

object RuleParser{

  val sd : Option[Map[String,String]] = None



  val rulesText : String = {
    """ Costco, Epicerie
        Metro,Epicerie
        WAL-MART, Shopping
        Ford, Voiture
        Honda, Voiture
        Intact, Assurance
        Capital One, Credit Card
        SoupeSoup, Fabio Lunch
        Les Grillades, Fabio Lunch
        Five Guys, Fabio Lunch
        MARCHE M C GAGN, Le Lunch Gagnon
        OPTION SANTE, Le tratamento
        QUATTRO PIATTI, Fabio Lunch
        TON SUSHI, Le Lunch
        VIDEOTRON, Videotron
        TEJANO, Fabio Lunch
        Brunet Pharma, Fabio Transport
        MTG/HYP, Hypotheque,
        Hydro Quebec, Hydro Quebec
        Ultramar, Essence
        PETRO-CANADA, Essence
        Manuvie, Manuvie
        Manulife, Manuvie
        Morgan Stanley, Salaire
        PAIE GOUV, Salaire

    """    }
  val rules = parseRules(rulesText)

//  def categories() : Map[String,Category] = {
//    parseRules( getLine )_2
//
//  }

  def whatCategory(transaction: Transaction): Option[String] = {
    println(s"Check cat ${transaction.description} ${transaction.date}, ${transaction.amount})")
    val t = rules.find(r=>contRule(transaction,r))
    t.map(_.replace)
  }

  def contRule( t: Transaction, r: Rule ) : Boolean = {
    t.description.toLowerCase.contains(r.maStr.toLowerCase)
  }

  def calculate() = {
    //val cats = getTransactions(transLine)
    //onlyGroup(fileToString())
    doMore(fileToString())
  }

  def doMore(trans : Seq[Transaction]) = {
    val cats = trans.groupBy(whatCategory)
    cats.foreach( t=> {
      val total = t._2.map(_.amount).sum
      println(s"${t._1.getOrElse("no category")} = total is $total  ")
    })
  }

  def onlyGroup(trans : Seq[Transaction]) = {
    val cats = trans.groupBy(_.description.replaceAll("\\d", "")trim)
    cats.foreach( t=> {
      val total = t._2.map(_.amount).sum
      println(s" By cat ${t._1} = total is $total  ")
    })
  }
  def fileToString() :Seq[Transaction] ={
    val bufFile = io.Source.fromFile("/Users/fabiopadilha/Downloads/statement-2.csv")

    val lines = bufFile.getLines.toList
    bufFile.close()
    lines.flatMap(getTransaction)
  }

  def parseRules(text: String): Seq[Rule] = {
    val line = splitLines(text.trim)
    val list = line.map( l=> l.trim.split(",") )
    //val categories = list.map(Category(_)).distinct.map( t=> t.name -> t).toMap
    list.map( i =>  Rule(i(0).trim,i(1).trim))

  }

  val transLine: String = """  Costco Corp, -123.55, 20180303
Metro Comp, -30.43, 20180403
Some Other, 22.43, 20180302
Costco Bla, -350.34, 20180604
Ford Finance, -123.55, 20180505
Honda Finance, -155.23, 20180808
  """

  def start() = {
    calculate()
  }
  def main(args: Array[String]): Unit = {

    start()
//    println(" agsd ")
//
//    val rules = parseRules( getLine() )
//    println(s"Rule is ${rules} ")
//
//    val trans = getTransactions(transLine)
//    println(s"Trans is $trans " )
//
//    trans.foreach( t => {
//      rules.foreach( r => {
//        if (t.description.contains(r.maStr))
//          println(s"Replace ${r.replace}" )
//      })
//
//    })

  }

  def toBigDecimal(str: String): BigDecimal = BigDecimal(str.trim)

  def toLocalDate(str: String): LocalDate = LocalDate.now()

  def splitLines(text: String) : Seq[String] = {
    text.split("\n")
  }
  def getTransaction(line: String) : Option[Transaction] = {
    val ss: Seq[String] = line.trim.split(",")
    if (ss.length == 5 && !ss(0).contains("First"))
      Some(Transaction(ss(4), toBigDecimal(ss(3)), toLocalDate(ss(2))))
    else
      None
  }

//  def getTransactions(text: String) : Seq[Transaction] = {
//    val line = splitLines(text.trim)
//    line.map( getTransaction )
//  }

}
