package chess

case class Move(piece: Piece, dst: Position, src: Option[Position], promotion: Option[Char], castle: Option[Castle]) {
  override def toString = (castle, src) match {
    case (Some(cstl), _) => cstl.toString
    case (None, Some(pos)) => String.format("%s, %s -> %s", piece.toString, pos.toString, dst.toString)
    case (None, _) => String.format("%s, to %s", piece.toString, dst.toString)
  }
}
