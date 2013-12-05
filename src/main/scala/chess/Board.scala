package chess

import pieces._
import collection.mutable.{HashMap, Set}
import collection.mutable

/**
 * Board represents current state of the chessboard
 * Class exposes only R/O functionality for analysing current position
 *
 * Board state mutation are all in the object part of the board
 */
class Board extends Function[Position, Option[Piece]] {
  val horizontal = ('A' to 'H')
  val vertical = (1 to 8)

  protected var pos2piece = HashMap[Position, Piece]()
  protected var piece2pos = HashMap[Piece, Set[Position]]()
  /* check to see if non-pawn piece was moved (for castling) */
  protected var pos2moved = HashMap[Position, Boolean]()

  def positions(piece: Piece) : Set[Position] = {
    piece2pos.get(piece) match {
      case Some(positions) => positions
      case _ => Set.empty
    }
  }

  def apply(pos: Position) = pos2piece.get(pos)
  
  def moved(pos: Position) : Boolean = pos2moved.getOrElse(pos, false)

  /**
   * Find nearest piece from the given position on the given direction
   *
   * @param pos
   * @param dir
   * @return
   */
  def nearestPiece(pos: Position, dir: Direction) : Option[(Piece, Position)] =
    pos.traverse(dir).find((pos) => !this(pos).isEmpty) match {
      case Some(pos) => Option((this(pos).get, pos))
      case _ => None
    }

  /**
   * Prints current board state to the console in the ASCII mnemonic format
   */
  def print() {
    val filesRow = """    a   b   c   d   e   f   g   h"""
    val separator = """  +---+---+---+---+---+---+---+---+"""

    println(filesRow)
    println(separator)

    vertical.reverse.foreach((rank) => {
      val line =
        "%d |".format(rank) +
          horizontal.map((file) => " %s |".format(
            pos2piece.get(Position(file, rank)) match {
              case Some(piece) => piece.mnemonic
              case None => " "
            })).reduceLeft(_+_) +
          " %d".format(rank)

      println(line)
      println(separator)
    })

    println(filesRow)
    println()
  }

  def inCheckPosition(color: Color, pos: Position) : Boolean =
    kingAttackingFoes(color, pos).count((_) => true) > 0

  def inCheckPosition(color: Color) : Boolean = {
    val pos = piece2pos(Piece(King, color)).head
    inCheckPosition(color, pos)
  }

  def blockingPieces(color: Color, allPieces: Set[(Piece, Set[Position])], dst: Position) = {
    allPieces.count((p) => p match {
      case (piece, piecePositions) =>
        piecePositions.count((src) => piece.validate(this, Move(piece, dst, Option(src), None, None))) > 0
      case _ => false
    })
  }  
  
  def canBlock(color: Color, allPieces: Set[(Piece, Set[Position])], attackingLine: PositionIterator) = {
    val blocksOnLine = attackingLine.map((dst) => blockingPieces(color, allPieces, dst)).count((c) => c > 0)
    blocksOnLine > 0
  }

  /**
   * Tests whether the king is able to escape from check.
   * Player can:
   *  - move around in a position not under check (the only protection against knight attack)
   *  - move piece in front of attacking line
   *
   * @param color color to check
   * @return true if board in mate position for the king of the specified color
   */
  def inMatePosition(color: Color) : Boolean = {
    val king = Piece(King, color)
    val position = piece2pos(king).head

    // Check all the positions around
    val positionsToEscape = Directions.all.map((dir) => position.shift(dir))
      .filter((opt) =>
      opt match {
        case Some(pos) => King.validate(this, Move(king, position, Option(pos), None, None))
        case _ => false
      })

    if (positionsToEscape.count((pos) => !inCheckPosition(king.color, pos.get)) == 0) {
      // Cannot escape to anywhere - try to move pieces onto attacking lines

      /*
       * The algorithm:
       *  - iterate through positions on each attacking line
       *  - for each position:
       *    - grab all the pieces currently on the board
       *    - count number of pieces that could be moved to this position
       *    - return true if count is > 0
       */
      val allPieces =
        Set(Piece(Pawn, color), Piece(Rook, color), Piece(Knight, color), Piece(Bishop, color), Piece(Queen, color))
        .map((piece) => (piece, piece2pos.get(piece).get))

      val blockedLines = kingAttackingFoes(king.color, position).flatten.map(
        (pair) => pair match { // Test whether attack on this line could be blocked by piece moving
        case (piece, pos) => position.traverse(pos) match {
          case Some(attackingLine) => canBlock(color, allPieces, attackingLine)
          case _ => false
        }
      })
      val allAttacksBlocked = blockedLines.reduceLeft(_&&_)
      !allAttacksBlocked // should escape all the attacking lines
    } else {
      false // king can escape attack
    }


  }

  def kingAttackingFoes(color: Color, position: Position) = {
    /*
     * Look around from king's position checking to see there's some foe figures in attacking position
     */
    val foeKnight = Piece(Knight, color.complement)

    Directions.all.map((dir) => nearestPiece(position, dir))
      .union(piece2pos(foeKnight).map((pos) => Option((foeKnight, pos)))) // also mixin foe knights currently on the board
      .filter((t) => t match { // only leave pieces that are able to attack the king
        case Some((piece, pos))
          if piece.color == color.complement &&
             piece.validate(this, Move(piece, position, Option(pos), None, None)) =>
          true
        case _ => false
      })
  }
}

object Board extends Board {
  /**
   * Resets board to its initial state
   */
  def reset() {
    def piecesRank(color : Color) = {
      val rank = color match {
        case White => vertical.start
        case Black => vertical.end
      }

      List(
        (Piece(Rook, color),   Position('A', rank)),
        (Piece(Knight, color), Position('B', rank)),
        (Piece(Bishop, color), Position('C', rank)),
        (Piece(Queen, color),  Position('D', rank)),
        (Piece(King, color),   Position('E', rank)),
        (Piece(Bishop, color), Position('F', rank)),
        (Piece(Knight, color), Position('G', rank)),
        (Piece(Rook, color),   Position('H', rank))
      )
    }

    def pawnsRank(color : Color) = {
      val rank = color match {
        case White => vertical.start + 1
        case Black => vertical.end - 1
      }

      horizontal.map((c) => (Piece(Pawn, color), Position(c, rank)))
    }

    piece2pos.clear()
    pos2piece.clear()
    pos2moved.clear()

    List(White, Black).foreach((color) => {
      piecesRank(color).foreach((t) => {
        addTo(t._2, t._1)
      })
      pawnsRank(color).foreach((t) => {
        addTo(t._2, t._1)
      })
    })
  }

  /**
   * Removes piece from specified board position
   *
   * @param pos
   */
  def removeAt(pos: Position) : Piece = {
    val piece = this(pos).get
    val set = positions(piece)

    piece2pos += (piece -> (set - pos))
    pos2piece.remove(pos)
    pos2moved.put(pos, true)
    
    piece
  }

  /**
   * Adds piece at specified board position
   *
   * @param pos
   * @param piece
   */
  def addTo(pos: Position, piece: Piece) : Piece = {
    val set = positions(piece)

    piece2pos += (piece -> (set + pos))
    pos2piece += (pos -> piece)
    
    piece
  }

  /**
   * Reverts board position back to the previous state
   */
  def undoMove(move: Move, captured: Option[Piece]) : Unit = {
    addTo(move.src.get, removeAt(move.dst)) // Undo move

    if (!captured.isEmpty)
      addTo(move.dst, captured.get) // Return captured piece back to the board
  }

  /**
   * Verifies that last move did not cause check. If not - undoes move
   */
  def lastMoveCausesCheck(last: Move, capture: Option[Piece]) : Boolean = {
    if(inCheckPosition(last.piece.color)) {
      undoMove(last, capture)
      true
    } else false
  }
  
  def promotedPawn(move: Move) : Piece = {
    val promotedType = PieceType(move.promotion.getOrElse('Q').toString)
    val pawn = removeAt(move.dst)
    val promotedPiece = Piece(promotedType, pawn.color)
    addTo(move.dst, promotedPiece)
    promotedPiece
  }
  
  def promote(move: Move) : Option[Piece] = {
    (move.piece.pieceType, move.piece.color, move.dst.rank) match {
      case (Pawn, White, 8) => Some(promotedPawn(move))
      case (Pawn, Black, 1) => Some(promotedPawn(move))
      case (_, _, _) => None
    }
  }
  
  def castledRook(move: Move) : Option[Piece] = move.castle match {
    case Some(cstl) => Some(addTo(cstl.rookDst, removeAt(cstl.rookSrc)))
    case _ => None
  }
  
  def validMove(move: Move, captured: Option[Piece]) = {
    val promotedPiece = promote(move)
  
    if (inCheckPosition(move.piece.color.complement))
      if (inMatePosition(move.piece.color.complement))
        (this, CheckMate(move, promotedPiece))
      else
        (this, Check(move, promotedPiece))
    else
      captured match {
        case Some(piece) => (this, Captured(piece, move, promotedPiece))
        case None => (this, Moved(move, promotedPiece))
      }
  }

  /**
   * Moves piece to the specified location
   * WARNING! All piece-specific move validation should be done before this call.
   *
   * @param move move definition
   * @return Result of the move: Moved, Captured or InvalidMove if source position does not contain piece
   */
  def apply(move: Move) : (Board, Result) =
    move.src match {
      case Some(pos) if !this(pos).isEmpty =>
        val sourcePiece = this(pos).get

        this(move.dst) match {
          case None =>
            addTo(move.dst, removeAt(pos))

            if (lastMoveCausesCheck(move, None))
              (this, MoveCausesCheck(move))
            else {
              castledRook(move)
              validMove(move, None)
            }

          case Some(piece) if piece.color != sourcePiece.color =>
            val captured = removeAt(move.dst)
            removeAt(pos)
            addTo(move.dst, sourcePiece)

            if (lastMoveCausesCheck(move, Option(captured)))
              (this, MoveCausesCheck(move))
            else
              validMove(move, Option(captured))

          case _ => (this, InvalidMove(move))
        }
      case _ => (this, InvalidMove(move))
    }
}
