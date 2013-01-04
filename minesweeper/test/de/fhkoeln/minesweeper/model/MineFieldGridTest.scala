package de.fhkoeln.minesweeper.model

import org.specs.SpecificationWithJUnit

class MineFieldGridTest extends SpecificationWithJUnit {
    "A MineField Grid of size 1 without mines" should {

        val grid = MineFieldGrid( 1, 1, 0 )

        "not indicate an uncovered mine when uncovering the only Field" in {
            grid.uncoverField( ( 0, 0 ) )._2 must beFalse
        }

        "indicate i won the game" in {
            grid.uncoverField( ( 0, 0 ) )._3 must beTrue
        }

        "return a grid of size 1 with 1 uncovered unarmed Field with no adjacent mines" in {
            val newgrid = grid.uncoverField( ( 0, 0 ) )._1
            newgrid.size must be_==( 1 )
            newgrid( 0 ).size must be_==( 1 )
            newgrid( 0 )( 0 ).armed must beFalse
            newgrid( 0 )( 0 ).adjacent must be_==( 0 )
            newgrid( 0 )( 0 ).uncovered must beTrue
        }
        
        "return a grid of size 1 with 1 marked field" in {
            val newgrid = grid.markField((0,0))
            newgrid(0)(0).marked must beTrue
        }
        
        "return a grid of size 1 with 1 unmarked field" in {
            grid.markField((0,0))
            val newgrid = grid.unmarkField((0,0))
            newgrid(0)(0).marked must beFalse
        }
        
        "throw an Exception when uncovering a marked field" in {
            grid.markField((0,0))
            grid.uncoverField((0,0)) must throwA[MineGridException]
        }
        
        "throw an Exception when marking an uncovered field" in {
            grid.uncoverField((0,0))
            grid.markField((0,0)) must throwA[MineGridException]
        }
        
        

    }

    "A MineFieldGrid of size 1 with 1 mine" should {
        "not be able to be constructed" in {
            val grid = MineFieldGrid( 1, 1, 1, ( 0, 0 ) ) must throwA[ IllegalArgumentException ]
        }
    }
    
    "A MineFieldGrid of size 1 * 2 with 1 mine" should {
        
        val grid = MineFieldGrid(1, 2, 1, (0,0) )
        
        "not have a mine on the initial field" in {
            val newgrid = grid.uncoverField((0,0))
            newgrid._1(0)(0).armed must beFalse
            newgrid._2 must beFalse
            newgrid._3 must beTrue
        }
        
        
    }
}