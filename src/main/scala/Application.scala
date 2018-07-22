import Application.Board

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

sealed trait PieceType

case object King extends PieceType

case object Queen extends PieceType

case object Bishop extends PieceType

case object Rook extends PieceType

case object Knight extends PieceType

object Application extends App {

  implicit val ec = ExecutionContext.global

  type Board = IndexedSeq[IndexedSeq[Option[PieceType]]]

  def boardAsString(board: Board): String =
    board
      .map(_.map(_.map {
        case King   => "K"
        case Queen  => "Q"
        case Bishop => "B"
        case Rook   => "R"
        case Knight => "N"
      } getOrElse "-").mkString(""))
      .mkString("\n")

  case class ProblemInput(width: Int, height: Int, pieces: Map[PieceType, Int])

  case class Coord(x: Int, y: Int)

  def put(board: Board, coord: Coord, piece: PieceType): Board = board.zipWithIndex map {
    case (line, indexLine) if indexLine == coord.y => line.updated(coord.x, Some(piece))
    case (line, indexLine) if indexLine != coord.y => line
  }

  def mapToRepeatSeq[A](data: Map[A, Int]): Seq[A] =
    data.flatMap {
      case (value, times) => Seq.fill[A](times)(value)
    }.toSeq

  def solve(problemInput: ProblemInput)(implicit ec: ExecutionContext): Future[Set[Board]] = {
    backtrack(
      mkBoard(problemInput.width, problemInput.height),
      Some(Coord(0, 0)),
      mapToRepeatSeq(problemInput.pieces)
    ).map(_.toSet)
  }

  def mkBoard(width: Int, height: Int): Board =
    Array
      .tabulate[Option[PieceType]](width, height)((_, _) => Option.empty[PieceType])
      .map(_.toIndexedSeq)
      .toIndexedSeq

  def nextCoord(board: Board, currentCoord: Coord): Option[Coord] = {
    val inlineCoord    = currentCoord.y * board.head.size + currentCoord.x
    val nextInline     = inlineCoord + 1
    val coordCandidate = Coord(nextInline % board.head.size, nextInline / board.head.size)
    Some(coordCandidate).filter(validCoord(board, _))
  }

  def backtrack(board: Board, maybeCoord: Option[Coord], remainingPieces: Seq[PieceType])(
      implicit ec: ExecutionContext): Future[Seq[Board]] = {

    val validBoard     = valid(board)
    val maybeNextCoord = maybeCoord.flatMap(c => nextCoord(board, c))

    //    val depth: String = maybeCoord.map(c => "" + c.y * board.head.size + c.x).getOrElse("None")
    //    println(
    //      s"backtracking at $maybeCoord (depth $depth) with pieces $remainingPieces board:\n" + boardAsString(
    //        board))
    //    if (!validBoard) println("Invalid board")

    (validBoard, remainingPieces, maybeCoord) match {
      // invalid board : stop here
      case (false, _, _) => Future.successful(Nil)
      // valid board and no more pieces to put : found one solution
      case (true, Nil, _) => Future.successful(Seq(board))
      // still pieces to put but at end of board : stop here
      case (true, remaining, None) if remaining.nonEmpty => Future.successful(Nil)
      // not finished yet. Keep searching deeper
      case (true, _ :: _, Some(coord)) =>
        Future
          .sequence(
            backtrack(board, maybeNextCoord, remainingPieces) +:
              remainingPieces.indices
              .map(pieceIndex =>
                Future {
                  backtrack(
                    put(board, coord, remainingPieces(pieceIndex)),
                    maybeCoord = maybeNextCoord,
                    remainingPieces.patch(pieceIndex, Nil, 1)
                  )
                }.flatten))
          .map(_.flatten)
    }
  }

  /*
  Valid if no piece range intersect another piece
   */
  def valid(board: Board): Boolean = {
    for (y <- board.indices) {
      for (x <- board.head.indices) {
        val currentCell = board(y)(x)
        if (currentCell.isDefined) {
          for (coordInRange <- listCoordsInRange(board, currentCell.get, Coord(x, y))) {
            if (board(coordInRange.y)(coordInRange.x).isDefined) return false
          }
        }
      }
    }
    true
  }

  def validCoord(board: Board, coord: Coord) =
    coord.x >= 0 && coord.x < board.head.size && coord.y >= 0 && coord.y < board.size
  def validCoordCurried(board: Board) = (validCoord _).curried(board)

  def getKingCoordsInRange(board: Board, coord: Coord): Seq[Coord] =
    Seq(Coord(-1, -1),
        Coord(0, -1),
        Coord(1, -1),
        Coord(-1, 0),
        Coord(1, 0),
        Coord(-1, 1),
        Coord(0, 1),
        Coord(1, 1))
      .map(offset => Coord(coord.x + offset.x, coord.y + offset.y))
      .filter(validCoordCurried(board))

  def getRookCoordsInRange(board: Board, coord: Coord): Seq[Coord] = {
    val left  = (coord.x - 1 to 0 by -1) map (Coord(_, coord.y))
    val right = (coord.x + 1 until board.head.size by 1) map (Coord(_, coord.y))
    val up    = (coord.y - 1 to 0 by -1) map (Coord(coord.x, _))
    val down  = (coord.y + 1 until board.size by 1) map (Coord(coord.x, _))
    left ++ right ++ up ++ down
  }

  def getBishopCoordsInRange(board: Board, coord: Coord): Seq[Coord] = {
    val buffer = ListBuffer.empty[Coord]
    for {
      y <- (coord.y - 1) to 0 by -1
      x <- (coord.x - 1) to 0 by -1
    } buffer += Coord(x, y)

    for {
      y <- (coord.y - 1) to 0 by -1
      x <- (coord.x + 1) until board.head.size by 1
    } buffer += Coord(x, y)

    for {
      y <- (coord.y + 1) until board.size by 1
      x <- (coord.x - 1) to 0 by -1
    } buffer += Coord(x, y)

    for {
      y <- (coord.y + 1) until board.size by 1
      x <- (coord.x + 1) until board.head.size by 1
    } buffer += Coord(x, y)
    buffer
  }

  def getQueenCoordsInRange(board: Board, coord: Coord): Seq[Coord] =
    listCoordsInRange(board, Rook, coord) ++ listCoordsInRange(board, Bishop, coord)

  def getKnightCoordsInRange(board: Board, coord: Coord): Seq[Coord] =
    Seq(Coord(-1, -2),
        Coord(1, -2),
        Coord(2, -1),
        Coord(2, 1),
        Coord(1, 2),
        Coord(-1, 2),
        Coord(-2, 1),
        Coord(-2, -1))
      .map(offset => Coord(coord.x + offset.x, coord.y + offset.y))
      .filter(validCoordCurried(board))

  def listCoordsInRange(board: Board, pieceType: PieceType, coord: Coord): Seq[Coord] =
    (pieceType match {
      case King   => getKingCoordsInRange _
      case Rook   => getRookCoordsInRange _
      case Bishop => getBishopCoordsInRange _
      case Queen  => getQueenCoordsInRange _
      case Knight => getKnightCoordsInRange _
    })(board, coord)

  val exampleProblem  = ProblemInput(3, 3, Map(King -> 2, Rook   -> 1))
  val exampleProblem2 = ProblemInput(4, 4, Map(Rook -> 2, Knight -> 4))
  val evalProblem     = ProblemInput(7, 7, Map(King -> 2, Queen  -> 2, Bishop -> 2, Knight -> 1))

  val problems = Seq(exampleProblem, exampleProblem2, evalProblem)

  problems.foreach(problem => {
    val f = solve(problem).map(solutions => {
      println(s"Problem $problem")
      println(s"Solutions (${solutions.size}):")
      println(solutions map ("\n" + boardAsString(_)) mkString ("\n"))
      println()
    })

    Await.result(f, 5 minutes)
  })
}
