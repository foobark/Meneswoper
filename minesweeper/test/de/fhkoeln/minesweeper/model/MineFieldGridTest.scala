package de.fhkoeln.minesweeper.model

import org.specs.SpecificationWithJUnit

class MineFieldGridTest extends SpecificationWithJUnit {
	"A MineField Grid of size 1 without mines" should {
	    
	    val grid = MineFieldGrid(1, 1, 1)
	    
	    "not indicate an uncovered mine when uncovering the only Field" in {
	        grid.uncoverField((0,0))._2 must beFalse
	    }
	    
	    "indicate i won the game" in {
	        grid.uncoverField((0,0))._3 must beTrue
	    }
	    
	    "return a grid of size 1 with 1 uncovered unarmed Field with no adjacent mines" in {
	        val newgrid = grid.uncoverField((0,0))._1
	        newgrid.size must be_== ( 1 )
	        newgrid(0).size must be_== ( 1 )
	        newgrid(0)(0).armed must beFalse
	        newgrid(0)(0).adjacent must be_== ( 0 )
	        newgrid(0)(0).uncovered must beTrue
	    }
	}
 
}