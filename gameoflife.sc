import $ivy.`de.h2b.scala.lib:simgraf_2.12:1.3.0`
import $ivy.`com.storm-enroute:scalameter_2.10:0.8.2`
import $ivy.`com.storm-enroute:scalameter-core_2.10:0.8.2`
import $ivy.`org.scalactic:scalactic_2.10:3.3.0-SNAP2`


import scala.util.Random

import $file.engine.{Automaton, Board, AutomatonCell, Neighborhood}
import $file.fields.Pos2D

case class GameOfLife(life: Boolean,
                      override val pos: Pos2D,
                      override val findCell: Pos2D => GameOfLife)
extends AutomatonCell[GameOfLife] {
  override def update: Option[GameOfLife] = moore(this).count { case (_, c) => c.life } match {
    case 3 if !life => Some(copy(life=true))
    case n if life && (n < 2 || n > 3) => Some(copy(life=false))
    case _ => None
  }
}

object GameOfLife {
  def apply(pos: Pos2D, findCell: Pos2D => GameOfLife): GameOfLife = GameOfLife(life=false, pos, findCell)

  def automaton(dim: Int): Automaton[GameOfLife] = new Automaton[GameOfLife](dim, apply, Board.apply)
}
