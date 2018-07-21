sealed trait PieceType

case object King extends PieceType

case object Queen extends PieceType

case object Bishop extends PieceType

case object Rook extends PieceType

case object Knight extends PieceType

object Application extends App {

  type Board = IndexedSeq[IndexedSeq[Option[PieceType]]]

  case class ProblemInput(width: Int, height: Int, pieces: Map[PieceType, Int])

  val exampleProblem  = ProblemInput(3, 3, Map(King -> 2, Rook   -> 1))
  val exampleProblem2 = ProblemInput(4, 4, Map(Rook -> 2, Knight -> 4))
  val evalProblem     = ProblemInput(7, 7, Map(King -> 2, Queen  -> 2, Bishop -> 2, Knight -> 1))

}
