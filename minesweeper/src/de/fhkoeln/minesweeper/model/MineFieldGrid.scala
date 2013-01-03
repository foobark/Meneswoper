package de.fhkoeln.minesweeper.model

import scala.util

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
	  
	}
	
	private def placeMines() {
	  var placed = 0
	  while (placed != minecount) {
	    
	  }
	}
	
	private var grid = Array.ofDim[MineField](ysize, xsize)
}

case class MineGridException(s: String) extends Exception