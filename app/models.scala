package models

/**
 * Représentation d'un Sudoku.
 */
class SudokuBoard[T](aboard: Array[Either[T, Set[T]]]) {
    type Board = Array[Either[T, Set[T]]]
    val width = Math.sqrt(aboard.length).toInt
    val squareWidth = Math.sqrt(width).toInt
    val board: Board = cleanBoard(aboard)

    override def toString = {
        def mkSep =
            List.make(squareWidth, List.make(squareWidth * 3, '-').mkString("")).mkString("+").substring(1, width * 3 + squareWidth - 2)
        def chunk[U](n: Int, seq: Seq[U]): Seq[Seq[U]] =
            (0 until seq.length by n).toList.zip((n to seq.length by n).toList).map {
                case (start, end) => seq.slice(start, end)
            }
        chunk(width * squareWidth, board.map {
            case Left(n) => n.toString
            case _ => "."
        }).map {
            chunk(width, _).map {
                chunk(squareWidth, _).map {
                    _.mkString("  ")
                }.mkString(" | ")
            }.mkString("\n")
        }.mkString("\n" + mkSep + "\n")
    }

    private def removeFromRow(row: Int, n: T, b: Board): Board =
        removeFromIndices((row * width) to ((row + 1) * width - 1), n, b)

    private def removeFromCol(col: Int, n: T, b: Board): Board =
        removeFromIndices(col until width * width by width, n, b)

    private def removeFromSquare(square: Int, n: T, b: Board): Board = {
        val start = (square / squareWidth) * (width * squareWidth) + square % squareWidth * squareWidth
        val indices = Stream.from(start, width).take(squareWidth).flatMap(Stream.from(_).take(squareWidth))
        removeFromIndices(indices, n, b)
    }

    private def removeFromIndices(is: Seq[Int], n: T, brd: Board): Board =
        is.foldLeft(brd) {
            (b, i) =>
                if (b(i).isRight) {
                    b(i) = Right(b(i).right.get - n)
                }
                b
        }

    private def cleanBoard(brd: Board): Board =
        brd.zipWithIndex.foldLeft(brd) {
            case (b, (Left(n), i)) =>
                removeFromSquare(i / width / squareWidth * squareWidth + i % width / squareWidth, n,
                    removeFromRow(i / width, n,
                        removeFromCol(i % width, n, b)))
            case (b, _) => b
        }

    def setCell(row: Int, col: Int, value: T): SudokuBoard[T] = {
        val newBoard = board.map(c => c)
        newBoard(row * width + col) = Left(value)
        new SudokuBoard(newBoard)
    }

    /** Vérification qu'une partie du tableau ne contient pas de valeurs en double. */
    private def containsDuplicate(is: List[Int]): Boolean = {
        is match {
            case Nil => false
            case head :: tail =>
                board(head) match {
                    case Right(_) => containsDuplicate(tail)
                    case Left(v) => containsDuplicateInternal(tail, v) || containsDuplicate (tail)
                }
        }
    }

    /** Vérification que la partie du tableau sélectionnée ne contient pas la valeur 'n'. */
    private def containsDuplicateInternal(is: List[Int], n: T): Boolean = {
        is match {
            case Nil => false
            case head :: tail =>
                board(head) match {
                    case Right(_) => containsDuplicateInternal(tail, n)
                    case Left(v) => (n == v) || containsDuplicateInternal(tail, v)
                }
        }
    }

    /** Vérification qu'une ligne contient des doublons. */
    private def containsDuplicateRow(row: Int): Boolean = {
        containsDuplicate(((row * width) to ((row + 1) * width - 1)).toList)
    }

    /** Vérification qu'une colonne contient des doublons. */
    private def containsDuplicateCol(col: Int): Boolean = {
        containsDuplicate((col until width * width by width).toList)
    }

    /** Vérification qu'un bloc contient des doublons. */
    private def containsDuplicateSquare(square: Int): Boolean = {
        val start = (square / squareWidth) * (width * squareWidth) + square % squareWidth * squareWidth
        val indices = Stream.from(start, width).take(squareWidth).flatMap(Stream.from(_).take(squareWidth))
        containsDuplicate(indices.toList)
    }

    /** Vérification du tableau complet pour vérifier si une ligne, une colonne, ou un carré contient deux fois la même valeur. */
    def containsDuplicateBoard(): Boolean = {
        containsDuplicateRec(0)
    }

    private def containsDuplicateRec(i: Int): Boolean = {
        i match {
            case n if (n == width) => false
            case n => containsDuplicateRow(n) || containsDuplicateCol(n) || containsDuplicateSquare(n) || containsDuplicateRec(n+1)
        }
    }

    /**
     * Affichage d'une case donnée du Sudoku si elle est définie, "" sinon.
     * Contrairement aux autres méthodes, les indices de cette méthode commencent à 1.
     */
    def print(row: Int, col: Int): String = {
        board((row-1)*9 + col-1) match {
            case Left(a) => a.toString
            case Right(_) => ""
        }
    }
}

object SudokuHelper {
    final def solve[T](b: SudokuBoard[T]): Option[SudokuBoard[T]] =
        b.board.zipWithIndex.find(_._1.isRight) match {
            case None => Some(b)
            case Some((Right(ns), _)) if ns.isEmpty => None
            case Some((Right(ns), i)) => ns.projection.map {
                n => solve(b.setCell(i / b.width, i % b.width, n))
            }.find(_.isDefined) match {
                case None => None
                case Some(ans) => ans
            }
            case _ => throw new Exception // Placate the compiler.
        }

    /** Liste des valeurs possibles pour le Sudoku. */
    def possibleValues(s: Set[String]): Either[String, Set[String]] = {
        if (s.filter(_.matches("\\d")).size == s.size) {
            // Sudoku classique ne contenant que des chiffres.
            Right(Set("1", "2", "3", "4", "5", "6", "7", "8", "9"))
        } else {
            // Sudoku contenant d'autres caractères (lettres).
            Right(s)
        }
    }

    /** Affectation des valeurs connues du Sudoku. */
    def prepareSudoku(sudokuToSolve: SudokuBoard[String], values: List[(String, String)]): SudokuBoard[String] = {
        // Expression régulière permettant de connaître la ligne et la colonne de la case.
        val cazeRE = """c(\d)(\d)""".r

        values match {
            case Nil => sudokuToSolve
            case (cazeRE(row, col), value) :: tail =>
                prepareSudoku(sudokuToSolve.setCell(row.toInt-1, col.toInt-1, value), tail)
        }
    }
}
