import scala.collection.immutable
sealed trait PieceType

case object King extends PieceType

case object Queen extends PieceType

case object Bishop extends PieceType

case object Rook extends PieceType

case object Knight extends PieceType

object Application extends App {

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

  def solve(problemInput: ProblemInput): Seq[Board] = backtrack(
    mkBoard(problemInput.width, problemInput.height),
    Coord(0, 0),
    mapToRepeatSeq(problemInput.pieces)
  )

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

  def backtrack(board: Board, coord: Coord, remainingPieces: Seq[PieceType]): Seq[Board] = {
    println(s"backtracking at $coord with pieces $remainingPieces board:\n" + boardAsString(board))
    val validBoard     = valid(board)
    val maybeNextCoord = nextCoord(board, coord)
    (validBoard, remainingPieces, maybeNextCoord) match {
      // invalid board : stop here
      case (false, _, _) => Nil
      // still pieces to put but at end of board : stop here
      case (true, remaining, None) if remaining.nonEmpty => Nil
      // valid board and no more pieces to put : found one solution
      case (true, Nil, _) => Seq(board)
      // not finished yet. Keep searching deeper
      case (true, _ :: _, Some(nextCoord)) =>
        remainingPieces.indices.flatMap(
          pieceIndex =>
            backtrack(
              put(board, coord, remainingPieces(pieceIndex)),
              coord = nextCoord,
              remainingPieces.patch(pieceIndex, Nil, 1)
          ))
    }
  }

  /*
  Valid if no piece range intersect another piece
   */
  def valid(board: Board): Boolean =
    (for {
      y <- board.indices
      x <- board.head.indices
      if board(y)(x).isDefined
    } yield Coord(x, y)).forall(pieceCoord =>
      listCoordsInRange(board, board(pieceCoord.y)(pieceCoord.x).get, pieceCoord)
        .forall(coordInRange => board(coordInRange.y)(coordInRange.x).isEmpty))

  def validCoord(board: Board, coord: Coord) =
    coord.x >= 0 && coord.x < board.head.size && coord.y >= 0 && coord.y < board.size
  def validCoordCurried(board: Board) = (validCoord _).curried(board)

  def listCoordsInRange(board: Board, pieceType: PieceType, coord: Coord): Seq[Coord] =
    pieceType match {
      case King =>
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
      case Rook =>
        val left  = (coord.x - 1 to 0 by -1) map (Coord(_, coord.y))
        val right = (coord.x + 1 until board.head.size by 1) map (Coord(_, coord.y))
        val up    = (coord.y - 1 to 0 by -1) map (Coord(coord.x, _))
        val down  = (coord.y + 1 until board.size by 1) map (Coord(coord.x, _))
        left ++ right ++ up ++ down
      case Bishop =>
        val upperLeft = for {
          y <- (coord.y - 1) to 0 by -1
          x <- (coord.x - 1) to 0 by -1
        } yield Coord(x, y)
        val upperRight = for {
          y <- (coord.y - 1) to 0 by -1
          x <- (coord.x + 1) until board.head.size by 1
        } yield Coord(x, y)
        val lowerLeft = for {
          y <- (coord.y + 1) until board.size by 1
          x <- (coord.x - 1) to 0 by -1
        } yield Coord(x, y)
        val lowerRight = for {
          y <- (coord.y + 1) until board.size by 1
          x <- (coord.x + 1) until board.head.size by 1
        } yield Coord(x, y)
        upperLeft ++ upperRight ++ lowerLeft ++ lowerRight
      case Queen => listCoordsInRange(board, Rook, coord) ++ listCoordsInRange(board, Bishop, coord)
      case Knight =>
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
    }

  val exampleProblem  = ProblemInput(3, 3, Map(King -> 2, Rook   -> 1))
  val exampleProblem2 = ProblemInput(4, 4, Map(Rook -> 2, Knight -> 4))
  val evalProblem     = ProblemInput(7, 7, Map(King -> 2, Queen  -> 2, Bishop -> 2, Knight -> 1))

  val solutions = solve(exampleProblem)
  println("Solutions:")
  println(solutions map boardAsString)
}
