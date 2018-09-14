

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
        PAIE GOUV, Salaire,
        Gouv. QUEBEC, Gouv Quebec
        Amazon, Shopping
        SOUAD, Le
        Esso, Essence
        ROBERT JR, Coiffure
        Jo Fashions, Shopping
        Chaussures Panda, Shopping
        Best Buy, Shopping


    """    }
  val rules = parseRules(rulesText)


  def whatCategory(transaction: Transaction): Option[String] = {
    //println(s"Check cat ${transaction.description} ${transaction.date}, ${transaction.amount})")
    val t = rules.find(r=>matchRule(transaction,r))
    t.map(_.replace)
  }

  def matchRule(t: Transaction, r: Rule ) : Boolean = {
    t.description.toLowerCase.contains(r.maStr.toLowerCase)
  }

  def calculate() = {
    //val cats = getTransactions(transLine)

    val file = FileIndexer.nextBmoFile
    //val cp = new CapitalParser()

    //val trans = cp.parseTransactions(file)

    val bmo = new BmoParser()
    val trans = bmo.parseTransactions(file)

    //printGroup(onlyGroupByDescription(trans))
    //printGroup(groupByCategory(trans))

    printGroup(groupMonth(trans))
  }

  def groupMonth(trans: Seq[Transaction]) :Map[Option[String], Seq[Transaction]] = {
    trans.groupBy(d => Some("" + d.date.getYear() + " " + d.date.getMonth()) )
  }

  def groupByCategory(trans : Seq[Transaction]) : Map[Option[String],Seq[Transaction]] = trans.groupBy(whatCategory)

  def onlyGroupByDescription(trans : Seq[Transaction]) : Map[Option[String],Seq[Transaction]]= {
    trans.groupBy(g => Some( g.description.replaceAll("\\d", "")trim ))
  }

  def printGroup(group: Map[Option[String],Seq[Transaction]]) ={
    group.foreach( t=> {
            val total = t._2.map(_.amount).sum
            println(s"${t._1.getOrElse("no category")} = total is $total  ")
            if (total > 100)
              println(" --------------- Big ---------------")
          })
  }



  def parseRules(text: String): Seq[Rule] = {
    val line = splitLines(text.trim)
    val list = line.map( l=> l.trim.split(",") )
    //val categories = list.map(Category(_)).distinct.map( t=> t.name -> t).toMap
    list.map( i =>  Rule(i(0).trim,i(1).trim))
  }


  def start() = {
    calculate()
  }
  def main(args: Array[String]): Unit = {

    start()


  }

  def toBigDecimal(str: String): BigDecimal = BigDecimal(str.trim)








  def splitLines(text: String) : Seq[String] = {
    text.split("\n")
  }




}
