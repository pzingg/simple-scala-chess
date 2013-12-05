package chess

case class Move(piece: Piece, dst: Position, src: Option[Position], promotion: Option[Char], castle: Option[Castle]) {
  override def toString = (castle, src, promotion) match {
    case (Some(cstl), _, _) => cstl.toString
    case (None, Some(pos), Some(pr)) => String.format("%s, %s -> %s (becomes %s)", piece.toString, pos.toString, dst.toString, pr.toString)
    case (None, Some(pos), None) => String.format("%s, %s -> %s", piece.toString, pos.toString, dst.toString)
    case (None, _, Some(pr)) => String.format("%s, to %s (becomes %s)", piece.toString, dst.toString, pr.toString)
    case (None, _, None) => String.format("%s, to %s", piece.toString, dst.toString)
  }
}
