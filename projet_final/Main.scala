import scala.io.Source
import Akinator._

object Main {

	def simpleGame(it:Iterator[String]) {
		val arbreSimple = fichierToABanimal("Source")
				val resultat = jeuSimple(arbreSimple, it)
				if (resultat) println("J'ai gagné !\n")
				else println("J'ai perdu !\n")
	}

	def learnGame(it:Iterator[String]) {
		val arbreAnimal = fichierToABanimal("Source")
				val arbreResultat = jeuApprentissage(arbreAnimal,it)
				ABanimalToFichier("Source", arbreResultat)
	}

	def jnspGame(it:Iterator[String]) {
		val arbreSimpleJNSP = fichierToABanimal("Source")
				val resultat = jeuSimpleJNSP(arbreSimpleJNSP,it)
				if(resultat) println("J'ai gagné !\n")
				else println("J'ai perdu !\n")
	}


	def main(args: Array[String]) {
		println("Bienvenue sur le projet Akinator en Scala !\n")

		def launch(it:Iterator[String]) {
			println("Choisissez un mode de jeu parmi les suivants (entrez 1, 2 ou 3)")
			println("1 - Jeu simple")
			println("2 - Jeu avec apprentissage")
			println("3 - Jeu simple \"Je ne sais pas\"")
			def boucleVerifDebut(res:String):String = res match {
			case ret if (res.equals("1")||res.equals("2")||res.equals("3")) => ret
			case _ => {
				println("Entrez 1, 2 ou 3 !")
				boucleVerifDebut(it.next())
			}
			}
			val jeu = boucleVerifDebut(it.next())
					jeu match {
					case j if j.equals("1") => simpleGame(it)
					case j if j.equals("2") => learnGame(it)
					case j if j.equals("3") => jnspGame(it)
			}
			println("\nVoulez-vous refaire une partie ?")
			def boucleVerifRejouer(res:String):String = res match {
			case ret if (res.equals("o")||res.equals("n")) => ret
			case _ => {
				println("Entrez 'o' ou 'n' !")
				boucleVerifRejouer(it.next())
			}
			}
			val replay = boucleVerifRejouer(it.next())
					if (replay.equals("o")) launch(it)
		}
		try {
		launch(Source.stdin.getLines)
		} catch {
		  case ex:Exception => ex.getMessage()
		}
		println("\nAu revoir !")
	}
}