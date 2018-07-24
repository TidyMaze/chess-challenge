import scala.collection.mutable.ListBuffer
import scala.collection.immutable.{IndexedSeq, Seq}

sealed trait CellValue

sealed trait PieceType extends CellValue

case object King extends PieceType

case object Queen extends PieceType

case object Bishop extends PieceType

case object Rook extends PieceType

case object Knight extends PieceType

case object Blocked extends CellValue

case object EmptyCell extends CellValue

object Application extends App {

  type Board = IndexedSeq[IndexedSeq[CellValue]]

  type TodoList = Map[PieceType, Int]

  def boardAsString(board: Board): String =
    board
      .map(_.map {
        case King      => "K"
        case Queen     => "Q"
        case Bishop    => "B"
        case Rook      => "R"
        case Knight    => "N"
        case EmptyCell => "-"
        case Blocked   => "*"
      }.mkString(""))
      .mkString("\n")

  case class ProblemInput(width: Int, height: Int, pieces: TodoList)

  case class Coord(x: Int, y: Int)

  def put(board: Board, coord: Coord, cell: CellValue): Board = board.zipWithIndex map {
    case (line, indexLine) if indexLine == coord.y => line.updated(coord.x, cell)
    case (line, indexLine) if indexLine != coord.y => line
  }

  def putWithBlock(board: Board, coord: Coord, piece: PieceType): Board = {
    val ranges = listCoordsInRange(board, piece, coord)

    board.zipWithIndex map {
      case (line, indexLine) =>
        line.zipWithIndex map {
          case (EmptyCell, indexColumn) if indexLine == coord.y && indexColumn == coord.x =>
            piece
          case (EmptyCell, indexColumn) if ranges.contains(Coord(indexColumn, indexLine)) =>
            Blocked
          case (cell, _) => cell
        }
    }
  }

  def solve(problemInput: ProblemInput): Int =
    backtrack(
      mkBoard(problemInput.width, problemInput.height),
      Some(Coord(0, 0)),
      problemInput.pieces
    )

  def mkBoard(width: Int, height: Int): Board =
    Array
      .tabulate[CellValue](width, height)((_, _) => EmptyCell)
      .map(_.toIndexedSeq)
      .toIndexedSeq

  def nextCoord(board: Board, currentCoord: Coord): Option[Coord] = {
    val inlineCoord    = currentCoord.y * board.head.size + currentCoord.x
    val nextInline     = inlineCoord + 1
    val coordCandidate = Coord(nextInline % board.head.size, nextInline / board.head.size)
    Some(coordCandidate).filter(validCoord(board, _))
  }

  def removeOne(list: Application.TodoList, pieceType: PieceType): TodoList = list.collect {
    case (`pieceType`, size: Int) if size > 1     => (pieceType, size - 1)
    case entry @ (piece, _) if piece != pieceType => entry
  }

  var count = 0

  def backtrack(board: Board, maybeCoord: Option[Coord], remainingPieces: TodoList): Int = {

    val validBoard     = valid(board)
    val maybeNextCoord = maybeCoord.flatMap(c => nextCoord(board, c))

//    val depth: String = maybeCoord.map(c => "" + (c.y * board.head.size + c.x)).getOrElse("None")
//    println(
//      s"backtracking at $maybeCoord (depth $depth) with pieces $remainingPieces board:\n" + boardAsString(
//        board))

    (validBoard, remainingPieces, maybeCoord) match {
      // invalid board : stop here
      case (false, _, _) => 0
      // valid board and no more pieces to put : found one solution
      case (true, remaining, _) if remaining.isEmpty => {
        count += 1
        if (count % 1000 == 0) println(count)
//        println("SOLUTION:")
//        println(boardAsString(board))
//        println("remaining:" + remainingPieces)
//        println()
        1
      }
      // still pieces to put but at end of board : stop here
      case (true, remaining, None) if remaining.nonEmpty => 0

      case (true, remaining, Some(coord))
          if remaining.nonEmpty && board(coord.y)(coord.x).isInstanceOf[Blocked.type] =>
        backtrack(board, maybeNextCoord, remainingPieces)

      // not finished yet. Keep searching deeper
      case (true, remaining, Some(coord)) if remaining.nonEmpty =>
        remainingPieces.keys.par
          .map(
            piece =>
              backtrack(
                putWithBlock(board, coord, piece),
                maybeCoord = maybeNextCoord,
                removeOne(remainingPieces, piece)
            ))
          .sum +
          backtrack(board, maybeNextCoord, remainingPieces)

    }
  }

  def anyIntersects(board: Board, currentCell: PieceType, x: Int, y: Int): Boolean =
    listCoordsInRange(board, currentCell, Coord(x, y))
      .exists(coordInRange => board(coordInRange.y)(coordInRange.x).isInstanceOf[PieceType])

  /*
  Valid if no piece range intersect another piece
   */
  def valid(board: Board): Boolean = {
    var y = 0
    var x = 0

    while (y < board.size) {
      while (x < board.head.size) {
        val currentCell = board(y)(x)
        currentCell match {
          case pieceType: PieceType if anyIntersects(board, pieceType, x, y) =>
            return false
          case _ =>
        }
        x += 1
      }
      y += 1
      x = 0
    }
    true
  }

  def validCoord(board: Board, coord: Coord) =
    coord.x >= 0 && coord.x < board.head.size && coord.y >= 0 && coord.y < board.size
  def validCoordCurried(board: Board) = (validCoord _).curried(board)

  def getKingCoordsInRange(board: Board, coord: Coord): Vector[Coord] =
    Vector((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))
      .collect {
        case offset if validCoord(board, Coord(coord.x + offset._1, coord.y + offset._2)) =>
          Coord(coord.x + offset._1, coord.y + offset._2)
      }

  def getRookCoordsInRange(board: Board, coord: Coord): Vector[Coord] = {
    val buffer = ListBuffer.empty[Coord]

    var y = 0
    var x = 0

    x = coord.x - 1
    while (x >= 0) {
      buffer += Coord(x, coord.y)
      x -= 1
    }

    x = coord.x + 1
    while (x < board.head.size) {
      buffer += Coord(x, coord.y)
      x += 1
    }

    y = coord.y - 1
    while (y >= 0) {
      buffer += Coord(coord.x, y)
      y -= 1
    }

    y = coord.y + 1
    while (y < board.size) {
      buffer += Coord(coord.x, y)
      y += 1
    }

    buffer.to[Vector]
  }

  def getBishopCoordsInRange(board: Board, coord: Coord): Vector[Coord] = {
    val buffer = ListBuffer.empty[Coord]

    var y = 0
    var x = 0

    y = coord.y - 1
    x = coord.x - 1
    while (x >= 0 && y >= 0) {
      buffer += Coord(x, y)
      y -= 1
      x -= 1
    }

    y = coord.y - 1
    x = coord.x + 1
    while (x < board.head.size && y >= 0) {
      buffer += Coord(x, y)
      y -= 1
      x += 1
    }

    y = coord.y + 1
    x = coord.x - 1
    while (x > 0 && y < board.size) {
      buffer += Coord(x, y)
      y += 1
      x -= 1
    }

    y = coord.y + 1
    x = coord.x + 1
    while (x < board.head.size && y < board.size) {
      buffer += Coord(x, y)
      y += 1
      x += 1
    }
    buffer.to[Vector]
  }

  def getQueenCoordsInRange(board: Board, coord: Coord): Vector[Coord] =
    listCoordsInRange(board, Rook, coord) ++ listCoordsInRange(board, Bishop, coord)

  def getKnightCoordsInRange(board: Board, coord: Coord): Vector[Coord] =
    Vector((-1, -2), (1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1))
      .collect {
        case offset if validCoord(board, Coord(coord.x + offset._1, coord.y + offset._2)) =>
          Coord(coord.x + offset._1, coord.y + offset._2)
      }

  def listCoordsInRange(board: Board, pieceType: PieceType, coord: Coord): Vector[Coord] =
    (pieceType match {
      case King   => getKingCoordsInRange _
      case Rook   => getRookCoordsInRange _
      case Bishop => getBishopCoordsInRange _
      case Queen  => getQueenCoordsInRange _
      case Knight => getKnightCoordsInRange _
    })(board, coord)

  def timeMs[R](block: => R): (R, Float) = {
    val t0     = System.nanoTime()
    val result = block // call-by-name
    val t1     = System.nanoTime()
    val delta  = (t1 - t0).toFloat / 1000000.toFloat
    (result, delta)
  }

  val exampleProblem  = ProblemInput(3, 3, Map(King -> 2, Rook   -> 1))
  val exampleProblem2 = ProblemInput(4, 4, Map(Rook -> 2, Knight -> 4))
  val evalProblem     = ProblemInput(7, 7, Map(King -> 2, Queen  -> 2, Bishop -> 2, Knight -> 1))

  val problems = Seq(evalProblem)

  var totTime     = 0.0f
  var samplesSize = 0

//  while (true) {
  problems.foreach(problem => {
    val (solutions, timeSpent: Float) = timeMs(solve(problem))
    totTime += timeSpent
    samplesSize += 1
    println(
      s"Problem $problem : Solutions (${solutions}) in $timeSpent ns (avg ${totTime / samplesSize}):")
//      println(solutions map ("\n" + boardAsString(_)) mkString ("\n"))
//      println()
  })
//  }
}
