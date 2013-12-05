package chess

/**
 * Class to package up all the information needed to do a castle move.
 * Assumes notation is either 5 or 3 characters long
 * 0-0-0, o-o-o, etc.
 */

case class Castle(notation: String, color: Color) {
  val str = notation.length match {
    case 5 => "O-O-O"
    case _ => "O-O"
  }

  val kingSrc = color match {
    case White => Position('E', 1)
    case Black => Position('E', 8)
  }

  val kingDst = (notation.length, color) match {
    // queenside
    case (5, White) => Position('C', 1)
    case (5, Black) => Position('C', 8)
    // kingside
    case (_, White) => Position('G', 1)
    case (_, Black) => Position('G', 8)
  }

  val rookSrc = (notation.length, color) match {
    // queenside
    case (5, White) => Position('A', 1)
    case (5, Black) => Position('A', 8)
    // kingside
    case (_, White) => Position('H', 1)
    case (_, Black) => Position('H', 8)
  }

  val rookDst: Position = (notation.length, color) match {
    // queenside
    case (5, White) => Position('D', 1)
    case (5, Black) => Position('D', 8)
    // kingside
    case (_, White) => Position('F', 1)
    case (_, Black) => Position('F', 8)
  }

  val mustBeEmpty: Set[Position] = (notation.length, color) match {
    // queenside
    case (5, White) => Set(Position('B', 1), Position('C', 1), Position('D', 1))
    case (5, Black) => Set(Position('B', 8), Position('C', 8), Position('D', 8))
    // kingside
    case (_, White) => Set(Position('F', 1), Position('G', 1))
    case (_, Black) => Set(Position('F', 8), Position('G', 8))
  }
  
  override def toString = str
}
