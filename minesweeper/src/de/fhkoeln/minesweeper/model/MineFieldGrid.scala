package de.fhkoeln.minesweeper.model

import scala.util.Random

case class MineFieldGrid(val xsize: Int, val ysize: Int, val minecount: Int, val initial_field: (Int, Int) ) {
	
	if ( minecount > xsize * ysize) throw new IllegalArgumentException("Too many mines for grid size")
	
	if ( minecount < 0) throw new IllegalArgumentException("minecount must be positive")
	
	if ( xsize < 0 || ysize < 0 ) throw new IllegalArgumentException("xsize and ysize must be positive")
	
	if ( initial_field._1 < 0 || initial_field._2 < 0) throw new IllegalArgumentException("Initial field coordinates must be positive")

	populateField()
	
	//get a copy of the internal grid
	def getGrid(): Array[Array[MineField]] = grid.clone()
	
	/*
	 * 
	 * @Return: tupel containing updated grid, Boolean indicating whether a mine was uncovered and finally a Boolean
	 * if everything besides the mines was uncovered.
	 */
	def uncoverField( pos: (Int, Int) ): ( Array[Array[MineField]], Boolean, Boolean ) =  {
		val field = grid(pos._1)(pos._2)
		try	field.uncover()
		catch {
		  case iae: IllegalArgumentException => throw new MineGridException("Trying to uncover marked field at: " + pos.toString())
		}
		field match {
		  //Uncover armed mine indicate lost game
		  case MineField(_, true, _, _) => (grid.clone(), true, false)
		  //If field has adjacent mines simply uncover it
		  case MineField(adjacent, _, _, _) if adjacent > 0 => (grid.clone(), false, false)
		  //If field has no adjacent mines uncover all adjacent fields until a field with adjacent mines is discovered
		  case MineField(0,_,_,_) => {
		    val positions = getAdjacentPos(pos._1, pos._2)
		    positions.init foreach(uncoverField)
		    uncoverField(positions.last) 
		  }
		}
	}
	
	//mark the Field at specified position. Throws Exception if said field is already uncovered
	def markField(pos: (Int, Int)) {
		try grid(pos._1)(pos._2).marked_=(true)
		catch {
			case iae: IllegalArgumentException => throw new MineGridException("Trying to mark uncovered field at: " + pos.toString())
		}
	}
	
	//unmark the Field at specified position
	def unmarkField(pos: (Int, Int)) = grid(pos._1)(pos._2).marked = false
	
	//setup the mine grid with, i.e. initialize all fields.
	private def populateField() {
		//place mines first...
		placeMines()
		//...then fill the rest and calculate no. of adjacent mines for fields
		for( i <- 0 until ysize; j <- 0 until xsize) {
			//make sure no double placements happen (and mines get overwritten)
			if (fieldEmpty(i, j)) {
				grid(i)(j) = MineField(adjacent = countAdjacentMines(i, j))
			}
		}
	}
	
	//Position mines randomly on the grid. Avoid initial position
	private def placeMines() {
	  var placed = 0
	  var randy = new Random()
	  while (placed != minecount) {
		  val row = randy.nextInt(ysize)
		  val col = randy.nextInt(xsize)
		  //make sure same field doesn't get populated twice
		  if (fieldEmpty(row,col) && (row, col) != initial_field ) {
			  grid(row)(col) = MineField(armed = true)
			  placed += 1
		  }
	  }
	}
	
	//Count the number of Mines adjacent to a field
	private def countAdjacentMines(y: Int, x: Int):Int =  getAdjacentPos(y,x) count(isMineField)
	
	//get adjacent, valid positions to a field
	private def getAdjacentPos(y: Int, x:Int): List[(Int, Int)] = {
			List(
				 (y + 1, x ),
				 (y + 1, x -1),
				 (y, x - 1),
				 (y-1, x - 1),
				 (y - 1, x),
				 (y - 1, x +1),
				 (y, x + 1),
				 (y + 1, x + 1)
			).filter( isValidPos )  
	}
	
	//check whether the field is an armed mine
	private def isMineField(pos: (Int, Int)):Boolean =  grid(pos._1)(pos._2).armed
	
	//Check if position is within boundaries
	private def isValidPos(pos: (Int, Int)):Boolean = !fieldEmpty(pos._1, pos._2) && yboundaries.contains(pos._1) && xboundaries.contains(pos._2)
	
	//check whether the position has already been populated with a field
	private def fieldEmpty(y: Int, x: Int):Boolean = grid(y)(x) == null
	
	private var grid = Array.ofDim[MineField](ysize, xsize)
	
	private val xboundaries = 0 to xsize
	
	private val yboundaries = 0 to ysize
}

case class MineGridException(s: String) extends Exception