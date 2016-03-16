import scala.io.Source

object Main {
  
  trait ABanimal
  case class Animal(nom:String) extends ABanimal
  case class Question(q:String,oui:ABanimal,non:ABanimal) extends ABanimal
  
  def fichierToABanimal(nomf:String):ABanimal = {
    val l = Source.fromFile(nomf).getLines.toList
    def convert(l:List[String]): (ABanimal,List[String]) = l match {
      case t::l if t contains "q:" => { 
        val (oui,l2)= convert(l)
        val (non,l3)= convert(l2)
        (new Question(t,oui,non),l3)
      }
      case t::q => (new Animal(t),q)
    }
    convert(l)._1
  }
 
  
  def main(args:Array[String]){
    println(fichierToABanimal("Source"))
  }
}