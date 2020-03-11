import $file.fields.Dir2D

object Neighborhood {
  def moore[C <: AutomatonCell[C]](cell: C): Map[Dir2D, C] =
    Dir2D.dirs8.map(dir => dir -> cell.findCell(cell.pos.move(dir))).toMap
}
