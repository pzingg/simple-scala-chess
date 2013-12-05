package chess.commands

import chess.pieces.{PieceType, King}
import chess._

/**
 * Check to see if user typed in 0-0-0, etc.
 * If so, create a Castle case and add it to the MoveCommand
 */

object CastleMoveCommand extends Parser {
  val pattern = """^(?i)(O-O-O|0-0-0|O-O|0-0)"""

  def matches(expr : String) = expr.matches(pattern)

  def parse(expr: String, color: Color) = {
    val matcher = pattern.r
    val matcher(side) = expr
    val cstl = Castle(side, color)

    Option(new MoveCommand(Move(Piece(King, color), cstl.kingDst, Option(cstl.kingSrc), None, Option(cstl))))
  }
}

/**
 * Normal, un-castling move.
 * Optional "="
 */

object MoveCommand extends Parser {
  val pattern = """^(?i)([PRNBQK])?([A-H][1-8])?([A-H][1-8])(([=])([RNBQ]?))?"""

  def matches(expr : String) = expr.matches(pattern)

  def parse(expr: String, color: Color) = {
    val matcher = pattern.r
    val matcher(p, s, d, prt, pr, pt) = expr
    val promotion: Option[Char] = (pr, pt) match {
      case ("=", "") => Some('Q')
      case ("=", _) => Some(pt(0).toUpper)
      case (_, _) => None
    }

    (PieceType(p), Position(s), Position(d)) match {
      case (piece, Some(src), Some(dst)) =>
        Option(new MoveCommand(Move(Piece(piece, color), dst, Option(src), promotion, None)))

      case (piece, None, Some(dst)) =>
        Option(new MoveCommand(Move(Piece(piece, color), dst, None, promotion, None)))

      case _ => None
    }
  }
}

class MoveCommand(move: Move) extends Command {
  def apply(board: Board) = move.src match {
    case Some(src) if !board(src).isEmpty && board(src).get == move.piece =>
      if (move.piece.validate(board, move))
        Board(move)
      else
        (board, InvalidMove(move))

    case _ =>
      val suitablePos = board.positions(move.piece).filter((pos) =>
        move.piece.validate(board, Move(move.piece, move.dst, Option(pos), move.promotion, move.castle)))

      if (!suitablePos.isEmpty)
        if(suitablePos.count((_) => true) == 1) {
          Board(Move(move.piece, move.dst, suitablePos.headOption, move.promotion, move.castle))
        } else {
          (board, AmbiguousMove(move))
        }
      else
        (board, InvalidMove(move))
  }
}