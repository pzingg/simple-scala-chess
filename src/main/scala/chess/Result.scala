package chess

import pieces.King

trait Result

case object NoOp extends Result
case object Finished extends Result

case class InvalidMove(move: Move) extends Result {
  override def toString = String.format("Invalid move: %s", move.toString)
}

case class AmbiguousMove(move: Move) extends Result {
  override def toString = String.format("Ambiguous move: %s", move.toString)
}

case class MoveCausesCheck(move: Move) extends Result {
  override def toString = String.format("Invalid move: %s leaves %s in check position!", move.toString, Piece(King, move.piece.color))
}

case class Moved(move: Move, promotedPiece: Option[Piece]) extends Result {
  override def toString = promotedPiece match {
    case Some(pc) => String.format("%s: promoted to %s", move.toString, pc.pieceType.toString)
    case None => move.toString
  }
}

case class Captured(piece: Piece, move: Move, promotedPiece: Option[Piece]) extends Result {
  override def toString = promotedPiece match {
    case Some(pc) => String.format("%s: promoted to %s, captured %s", move.toString, pc.pieceType.toString, piece.toString)
    case None => String.format("%s: captured %s", move.toString, piece.toString)
  }
}

case class Check(move: Move, promotedPiece: Option[Piece]) extends Result {
  override def toString = promotedPiece match {
    case Some(pc) => String.format("%s: promoted to %s, check for the %s!", move.toString, pc.pieceType.toString, Piece(King, move.piece.color))
    case None => String.format("%s: check for the %s!", move.toString, Piece(King, move.piece.color))
  }
}

case class CheckMate(move: Move, promotedPiece: Option[Piece]) extends Result {
  override def toString = promotedPiece match {
    case Some(pc) => String.format("%s: promoted to %s, CHECK and MATE for the %s!", move.toString, pc.pieceType.toString, Piece(King, move.piece.color.complement))
    case None => String.format("%s: CHECK and MATE for the %s!", move.toString, Piece(King, move.piece.color.complement))
  }
}
