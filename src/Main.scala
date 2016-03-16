import scala.io.Source

object Main {
  
  trait ABanimal
  case class Animal(nom:String) extends ABanimal
  case class Question(q:String,oui:ABanimal,non:ABanimal)
  
  def fichierToABanimal(nomf:String):ABanimal = {
    val l = Source.fromFile(nomf).getLines.toList
    def convert(l:List[String]): ABanimal = l match {
      case _ => new Animal("bonjour")
    }
    convert(l)
  }
  
}