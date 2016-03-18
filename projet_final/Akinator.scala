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
				case Nil => throw new Exception("Erreur lors de la conversion : fichier incomplet")
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
				if (rep.equals("o")) aux(oui)
				else if(rep.equals("n"))aux(non)
				else {
					println("Entrez 'o' ou 'n' !")
					aux(a)
				}
			}
			case Animal(t) => {
				println("Pensez-vous à l'animal "+t+" ?")
				val rep = /*readLine()*/it.next()
				if (rep.equals("o")) true
				else if (rep.equals("n")) false
				else {
					println("Entrez 'o' ou 'n' !")
					aux(a)
				}
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
				if (rep.equals("o")) aux(oui,adjqlis(l,rep))
				else if (rep.equals("n")) aux(non,adjqlis(l,rep))
				else {
					println("Entrez 'o' ou 'n' !")
					aux(a,l)
				}
			}
			case Animal(t) => {
				println("Pensez-vous à l'animal "+t+" ?")
				val rep = it.next()
				if (rep.equals("o") || rep.equals("n")) adjqlis(l,rep)
				else {
					println("Entrez 'o' ou 'n' !")
					aux(a,l)
				}
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
				} else if (rep.equals("n")){
					Question(t,oui,aux(non))
				} else {
				  println("Entrez 'o' ou 'n' !")
				  aux(a)
				}
			}
			case Animal(t) => {
				println("Pensez-vous à l'animal "+t+" ?")
				val rep = it.next()
				if (rep.equals("o")) {
					println("J'ai gagné")
					a
				}
				else if (rep.equals("n")) {
					println("J'ai perdu - quelle était la bonne réponse ?")
					val nAnimal = it.next()
					println("Quelle question permet de différencier "+nAnimal+" et "+t+" ?")
					val nQuestion = it.next()
					println("Quelle est la réponse à cette question pour "+nAnimal+" ?")
					def boucleVerif(res:String):String = res match {
					  case ret if (res.equals("o")||res.equals("n")) => ret
					  case _ => {
					    println("Entrez 'o' ou 'n' !")
					    boucleVerif(it.next())
					  }
					}
					val nRep = boucleVerif(it.next())
					if(nRep.equals("o")) Question(nQuestion,Animal(nAnimal),Animal(t))
					else Question(nQuestion,Animal(t),Animal(nAnimal))
				} else {
				  println("Entrez 'o' ou 'n' !")
				  aux(a)
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
				} else {
				  println("Entrez 'o', 'n' ou 'x' !")
				  aux(a)
				}

			}
			case Animal(t) => {
				println("Pensez-vous à l'animal "+t+" ?")
				val rep = /*readLine()*/it.next()
				if (rep.equals("o")) true
				else if (rep.equals("n")) false
				else {
				  println("Entrez 'o' ou 'n' !")
				  aux(a)
				}
			}
			}
			aux(a)
	}
}

