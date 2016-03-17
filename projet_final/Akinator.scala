import scala.io.Source
import java.io._

object Akinator {

	trait ABanimal
case class Animal(nom:String) extends ABanimal
case class Question(q:String,oui:ABanimal,non:ABanimal) extends ABanimal

def fichierToABanimal(nomf:String):ABanimal = {
		val l = Source.fromFile(nomf).getLines.toList
				def convert(l:List[String]): (ABanimal,List[String]) = l match {
				case t::q if t contains "q:" => {
					val (oui, l2) = convert(q)
							val (non, l3) = convert(l2)
							(Question(t.split(":")(1),oui,non),l3)
				}
				case t::q => (Animal(t),q)
		}
		convert(l)._1
	}

	def ABanimalToFichier(nomf:String, a:ABanimal) {
		val writer = new FileWriter(new File(nomf))
		def register(a:ABanimal):Unit = a match {
		case Question(t,oui,non) => {
			writer.write("q:"+t+"\n")
			register(oui)
			register(non)
		}
		case Animal(t) => writer.write(t+"\n")
		}
		register(a)
		writer.close()
	}

	def jeuSimple(a:ABanimal, it:Iterator[String]):Boolean = {
			print("Pensez à un animal - ")
			def aux(a:ABanimal):Boolean = a match {
			case Question(t,oui,non) => {
				println(t)
				val rep =/*readLine()*/ it.next()
				if (rep.equals("o")){
					aux(oui)
				} else {
					aux(non)
				}
			}
			case Animal(t) => {
				println("Pensez-vous à l'animal "+t+" ?")
				val rep = /*readLine()*/it.next()
				rep.equals("o")
			}
			}
			aux(a)
	}

	def jeuLog(a:ABanimal, it:Iterator[String]):List[String] = {
			print("Pensez à un animal - ")
			def aux(a:ABanimal,l:List[String]):List[String] = a match {
			case Question(t,oui,non) => {
				println(t)
				val rep = it.next()
				if (rep.equals("o")){
					aux(oui,adjqlis(l,rep))
				} else {
					aux(non,adjqlis(l,rep))
				}
			}
			case Animal(t) => {
				println("Pensez-vous à l'animal "+t+" ?")
				val rep = it.next()
				adjqlis(l,rep)
			}
			}
			val l = List()
					aux(a,l)
	}

	def adjqlis(l:List[String], value:String):List[String] = l match {
	case Nil => List(value)
	case t::q => t::(adjqlis(q,value))
	}

	def jeuApprentissage(a:ABanimal,it:Iterator[String]):ABanimal = {
			print("Pensez à un animal - ")
			def aux(a:ABanimal):ABanimal = a match {
			case Question(t,oui,non) => {
				println(t)
				val rep = it.next()
				if (rep.equals("o")){
					Question(t,aux(oui),non)
				} else {
					Question(t,oui,aux(non))
				}
			}
			case Animal(t) => {
				println("Pensez-vous à l'animal "+t+" ?")
				val rep = it.next()
				if (rep.equals("o")) {
					println("J'ai gagné")
					a
				}
				else {
					println("J'ai perdu - quelle était la bonne réponse ?")
					val nAnimal = it.next()
					println("Quelle question permet de différencier "+nAnimal+" et "+t+" ?")
					val nQuestion = it.next()
					println("Quelle est la réponse pour cette question pour "+nAnimal+" ?")
					val nRep = it.next()
					if(nRep.equals("o")) Question(nQuestion,Animal(nAnimal),Animal(t))
					else if(nRep.equals("n")) Question(nQuestion,Animal(t),Animal(nAnimal))
					else throw new Exception("Erreur : entrez 'o' ou 'n'")
				}
			}
			}
			aux(a)
	}

	def jeuSimpleJNSP(a:ABanimal,it:Iterator[String]):Boolean = {
			print("Pensez à un animal - ")
			def aux(a:ABanimal):Boolean = a match {
			case Question(t,oui,non) => {
				println(t)
				val rep =/*readLine()*/ it.next()
				if (rep.equals("o")) aux(oui)
				else if(rep.equals("n")) aux(non)
				else if(rep.equals("x")) {
					val trouve = aux(oui)
							if(!trouve) aux(non)
							else trouve
				} else throw new Exception("Erreur: entrez 'o', 'n' ou 'x'")

			}
			case Animal(t) => {
				println("Pensez-vous à l'animal "+t+" ?")
				val rep = /*readLine()*/it.next()
				rep.equals("o")
			}
			}
			aux(a)
	}



	def main(args : Array[String]){
		try {
			//println(fichierToABanimal("Source"))
			//ABanimalToFichier("Destination",fichierToABanimal("Source"))
			//val rep = jeuSimple(fichierToABanimal("Source"),Source.stdin.getLines)
			//if (rep) println("J'ai gagné")
			//else println("J'ai perdu")
			//val lRep = jeuLog(fichierToABanimal("Source"),Source.stdin.getLines)
			//if (lRep.last.equals("o")) println("J'ai gagné")
			//else println("J'ai perdu")
			//println(lRep)
			val animalRep = jeuApprentissage(fichierToABanimal("Destination2"),Source.stdin.getLines())
					println(animalRep)
					ABanimalToFichier("Destination2",animalRep)
					//val repJNSP = jeuSimpleJNSP(fichierToABanimal("Source"),Source.stdin.getLines)
					//if (repJNSP) println("J'ai gagné")
					//else println("J'ai perdu")
		} catch {
		case ex: Exception => {
			println(ex.getMessage)
		}
		}
	}

}

