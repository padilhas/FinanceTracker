import java.io.File
import java.nio.file.{Files, Path}

import scala.collection.JavaConverters._

object FileIndexer {

  val bmoFile = "/Users/fabiopadilha/Downloads/statement-2.csv"
  val capitalFile = "/Users/fabiopadilha/Downloads/07-09-2018_transactions_charger.csv"

  def indexFiles = {
    Files.list(new File("/Users/fabiopadilha/Documents/financeIndex").toPath)
    val bla = Files.newDirectoryStream(new File("/Users/fabiopadilha/Documents/financeIndex").toPath)
    
    bla.close()
  }

  def fileToString(file: String) :Seq[String] ={
    //val bufFile = io.Source.fromFile()
    val bufFile = io.Source.fromFile(file, "ISO-8859-1")

    val lines = bufFile.getLines.toList
    bufFile.close()
    lines
  }

  def nextBmoFile() = {
    fileToString(bmoFile)
  }

  def nextCapitalOneFile() = {
    fileToString(capitalFile)
  }
}
