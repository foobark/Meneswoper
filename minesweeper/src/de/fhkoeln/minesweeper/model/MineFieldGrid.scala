package de.fhkoeln.minesweeper.model

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
	
	private def populateField() {
	  
	}
	
	private var grid = Array.ofDim[MineField](ysize, xsize)
}

case class MineGridException(s: String) extends Exception