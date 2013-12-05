package chess.pieces

import chess.{Position, Move, Board, Castle}

case object King extends PieceType {
  val mnemonic: Char = 'K'
  override val maxMoveDistance = Option(1)

  /** 
   * Castling can only be done if the king has never moved, the rook 
   * involved has never moved, the squares between the king and the rook 
   * involved are unoccupied, the king is not in check, and the king does 
   * not cross over or end on a square in which it would be in check. 
   *
   * Castling is one of the rules of chess and is technically a king move.
   *
   * Castling consists of moving the king two squares towards a rook on the 
   * player's first rank, then moving the rook to the square over which the 
   * king crossed.
   */ 
  def validateCastle(board: Board, cstl: Castle) : Boolean = 
    !board.moved(cstl.kingSrc) && !board.moved(cstl.rookSrc) &&
      !cstl.mustBeEmpty.exists(pos => !board(pos).isEmpty) &&
      !board.inCheckPosition(cstl.color)

  // For kings we also need to verify king-to-king movement cases
  override def validate(board: Board, move: Move) : Boolean = {
    move.castle match {
      case Some(cstl) => validateCastle(board, cstl)
      case None => {
        val validated = super.validate(board, move)
        
        if (validated) {
          val src = move.src.get
          val dir = Position.direction(move.dst, src).get

          board.nearestPiece(src, dir) match {
            case Some((piece, pos)) if piece.pieceType == King =>
              pos.distance(src) match {
                case Some(d) => (d > 1)
                case _ => true
              }
            case _ => true
          }
        } else validated
      }
    }
  }
}
