package controllers

import _root_.models.{SudokuBoard, SudokuHelper}
import play._
import mvc._
import mvc.Scope.Flash
import scala.collection.JavaConversions._

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

    val EMPTY_CASE: Either[String, Set[String]] = Right(Set())
    val EMPTY_SUDOKU: SudokuBoard[String] = new SudokuBoard[String](Array(
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE,
        EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE, EMPTY_CASE
    ))

    import views.Sudoku._

    /** Affichage d'une grille de Sudoku vide. */
    def solver = {
        html.show(title = "Resolver de Sudoku", sudoku = EMPTY_SUDOKU)
    }

    /** Affiche la solution du Sudoku fourni. */
    def solution = {
        // Nettoyage des messages précédents.
        Flash.current().clear()
        // On récupère les 81 paramètres correspondants aux cases.
        val caseParams: Set[(String, String)] = mvc.Scope.Params.current().allSimple().toSet.filter(_._1.matches("c\\d{2}"))
        // On filtre pour ne garder que les cases valuées.
        val valParams: Set[(String, String)] = caseParams.filter(!_._2.matches("\\s*"))
        // On ne garde que les cases ayant une valeur.
        val values: Set[String] = valParams.map(_._2).toSet
        // Calcul des valeurs possibles pour ce Sudoku.
        val poss: Either[String, Set[String]] = SudokuHelper.possibleValues(values)
        // Création d'un tableau de 9*9 avec l'ensemble des valeurs possibles.
        val sudoku = new SudokuBoard[String](Array(
            poss, poss, poss, poss, poss, poss, poss, poss, poss,
            poss, poss, poss, poss, poss, poss, poss, poss, poss,
            poss, poss, poss, poss, poss, poss, poss, poss, poss,
            poss, poss, poss, poss, poss, poss, poss, poss, poss,
            poss, poss, poss, poss, poss, poss, poss, poss, poss,
            poss, poss, poss, poss, poss, poss, poss, poss, poss,
            poss, poss, poss, poss, poss, poss, poss, poss, poss,
            poss, poss, poss, poss, poss, poss, poss, poss, poss,
            poss, poss, poss, poss, poss, poss, poss, poss, poss
        ))

        // Remplacement des cases connues par leurs valeurs.
        val sudokuToSolve = SudokuHelper.prepareSudoku(sudoku, valParams.toList)

        if(sudokuToSolve.containsDuplicateBoard) {
            Flash.current().error("Cette grille n'est pas correcte : elle contient des doublons.")
            html.show(title = "Resolver de Sudoku", sudoku = sudokuToSolve)
        } else {
            // Calcul de la solution.
            val solution = SudokuHelper.solve(sudokuToSolve)

            solution match {
                case None =>
                    Flash.current().error("Cette grille n'a pas de solution")
                    html.show(title = "Resolver de Sudoku", sudoku = sudokuToSolve)
                case Some(sol) => html.show(title = "Solution du Sudoku", sudoku = sol)
            }
        }
    }
}
