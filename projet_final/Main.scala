import scala.io.Source
import Akinator._

object Main {
	def main(args: Array[String]) {
		println("Bienvenue sur le projet Akinator en Scala !\n")
		val arbreAnimal = fichierToABanimal("Source")
		def jeu(a:ABanimal,it:Iterator[String]):ABanimal = {
			val arbreRetour = jeuApprentissage(a, it)
			println("\nVoulez-vons refaire une partie ?")
			def boucleVerif(res:String):String = res match {
			case ret if (res.equals("o")||res.equals("n")) => ret
			case _ => {
				println("Entrez 'o' ou 'n' !")
				boucleVerif(it.next())
			}
			}
			val rep = boucleVerif(it.next())
					if (rep.equals("o")) jeu(arbreRetour,it)
					else arbreRetour
		}
		ABanimalToFichier("Source", jeu(arbreAnimal,Source.stdin.getLines))
		println("\nAu revoir !")
	}
}