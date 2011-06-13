package controllers

import models.SudokuBoard
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

    val EMPTY_CASE:Either[Char, Set[Char]] = Right(Set())
    val EMPTY_SUDOKU:Array[Either[Char, Set[Char]]] = Array(
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE
    )

    import views.Sudoku._

    /** Affichage d'une grille de Sudoku vide. */
    def solver = {
        html.show(title = "Resolver de Sudoku", sudoku = new SudokuBoard[Char](EMPTY_SUDOKU))
    }

    /** Affiche la solution du Sudoku fourni. */
    def solution = {
        html.show(title = "Solution du Sudoku", sudoku = new SudokuBoard[Char](EMPTY_SUDOKU))
    }
}
