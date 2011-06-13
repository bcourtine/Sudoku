package controllers

import play._
import mvc._

/**
 * Contrôleur de la page d'accueil par défaut de l'application.
 */
object Application extends Controller {

    import views.Application._

    def index = {
        html.index("Your Scala application is ready!")
    }
}

/**
 * Contrôleur de la grille de Sudoku.
 */
object Sudoku extends Controller {

    import views.Sudoku._

    /** Affichage d'une grille de Sudoku vide. */
    def solver = {
        html.show("Resolver de Sudoku")
    }

    /** Affiche la solution du Sudoku fourni. */
    def solution = {
        html.show("Solution du Sudoku")
    }
}
