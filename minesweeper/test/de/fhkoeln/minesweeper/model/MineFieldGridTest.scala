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

        "return a grid of size 1 with 1 uncovered untriggered Field with no adjacent mines" in {
            val newgrid = grid.uncoverField( ( 0, 0 ) )._1
            newgrid.size must be_==( 1 )
            newgrid( 0 ).size must be_==( 1 )
            newgrid( 0 )( 0 ).triggered must beFalse
            newgrid( 0 )( 0 ).adjacent must be_==( 0 )
            newgrid( 0 )( 0 ).uncovered must beTrue
        }

        "return a grid of size 1 with 1 marked field" in {
            val newgrid = grid.markField( ( 0, 0 ) )
            newgrid( 0 )( 0 ).marked must beTrue
        }

        "return a grid of size 1 with 1 unmarked field" in {
            grid.markField( ( 0, 0 ) )
            val newgrid = grid.unmarkField( ( 0, 0 ) )
            newgrid( 0 )( 0 ).marked must beFalse
        }

        "Not uncover a marked field" in {
            grid.markField( ( 0, 0 ) )
            val newgrid = grid.uncoverField( (0,0) )
            newgrid._1(0)(0).marked must beTrue
            newgrid._1(0)(0).uncovered must beFalse
        }

        "throw an Exception when marking an uncovered field" in {
            grid.uncoverField( ( 0, 0 ) )
            grid.markField( ( 0, 0 ) ) must throwA[ MineGridException ]
        }
        
        "throw an Exception when invalid positions get passed" in {
            grid.uncoverField((-1, 2)) must throwA[MineGridException]
            grid.markField((3, 4)) must throwA[MineGridException]
            grid.unmarkField(1,-4) must throwA[MineGridException]
            
        }

    }

    "A MineFieldGrid of size 1 with 1 mine" should {
        "not be able to be constructed" in {
            val grid = MineFieldGrid( 1, 1, 1, ( 0, 0 ) ) must throwA[ IllegalArgumentException ]
        }
    }

    "A MineFieldGrid of size 1 * 2 with 1 mine" should {

        val grid = MineFieldGrid( 1, 2, 1, ( 0, 0 ) )

        "not have a mine on the initial field" in {
            val newgrid = grid.uncoverField( ( 0, 0 ) )
            newgrid._1( 0 )( 0 ).triggered must beFalse
            newgrid._2 must beFalse
            newgrid._3 must beTrue
        }

        "have a mine on the 2nd field" in {
            val newgrid = grid.uncoverField( ( 0, 1 ) )
            newgrid._1( 0 )( 1 ).triggered must beTrue
            newgrid._2 must beTrue
            newgrid._3 must beFalse
        }

        "have a field with 1 adjacent mine in first spot" in {
            grid.getGridState()( 0 )( 0 ).adjacent must be_==( 1 )
        }

    }

    "A MineFieldGrid of size 3 * 3 with 2 mines" should {

        val grid = MineFieldGrid( 3, 3, 2, ( 1, 2 ) )

        "Not have an armed mine in inital field" in {
            val newgrid = grid.uncoverField( ( 1, 2 ) )
            newgrid._1( 1 )( 2 ).triggered must beFalse
            newgrid._2 must beFalse
        }

        "Have exactly 2 mines" in {
            for (i <- 0 until 3; j <- 0 until 3) grid.uncoverField((i,j))
            grid.getGridState().flatten count ( x => x.triggered ) must be_==( 2 )
        }
    }
    
    
}