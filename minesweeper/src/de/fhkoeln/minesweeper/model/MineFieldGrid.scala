package de.fhkoeln.minesweeper.model

import scala.util.Random

case class MineFieldGrid(val xsize: Int, val ysize: Int, val minecount: Int, val initial_field: (Int, Int) ) {
	
	if ( minecount > xsize * ysize) throw new IllegalArgumentException("Too many mines for grid size")
	
	if ( minecount < 0) throw new IllegalArgumentException("minecount must be positive")
	
	if ( xsize < 0 || ysize < 0 ) throw new IllegalArgumentException("xsize and ysize must be positive")
	
	if ( initial_field._1 < 0 || initial_field._2 < 0) throw new IllegalArgumentException("Initial field coordinates must be positive")

	populateField()
	
	def uncoverField(pos: (Int, Int)) {
		try grid(pos._1)(pos._2).uncover()
		catch {
			case iae: IllegalArgumentException => throw new MineGridException("Trying to uncover marked field at: " + pos.toString())
		}
		
	}
	
	def markField(pos: (Int, Int)) {
		try grid(pos._1)(pos._2).marked_=(true)
		catch {
			case iae: IllegalArgumentException => throw new MineGridException("Trying to mark uncovered field at: " + pos.toString())
		}
		
	}
	
	def unmarkField(pos: (Int, Int)) = grid(pos._1)(pos._2).marked = false
			
	private def populateField() {
		placeMines()
		for( i <- 0 until ysize; j <- 0 until xsize) {
			if (fieldEmpty(i, j)) {
				grid(i)(j) = MineField(adjacent = countAdjacents(i, j))
			}
		}
	}
	
	private def placeMines() {
	  var placed = 0
	  var randy = new Random()
	  while (placed != minecount) {
		  val row = randy.nextInt(ysize)
		  val col = randy.nextInt(xsize)
		  if (fieldEmpty(row,col) && (row, col) != initial_field ) {
			  grid(row)(col) = MineField(armed = true)
			  placed += 1
		  }
	  }
	}
	
	private def countAdjacents(y: Int, x: Int):Int = {
		 val positions = List(
				 (y + 1, x ),
				 (y + 1, x -1),
				 (y, x - 1),
				 (y-1, x - 1),
				 (y - 1, x),
				 (y - 1, x +1),
				 (y, x + 1),
				 (y + 1, x + 1)
		 )
		 positions.count(isAdjacentMine)
	}
	
	private def isAdjacentMine(pos: (Int, Int)):Boolean = {
	  val y = pos._1
	  val x = pos._2
	  grid(y)(x) != null && 0 <= y && y < ysize && 0 <= x && x < xsize && grid(y)(x).armed  
	}
	
	private def fieldEmpty(y: Int, x: Int) = grid(y)(x) == null
	
	private var grid = Array.ofDim[MineField](ysize, xsize)
	
}

case class MineGridException(s: String) extends Exception