package minesweeper.model

import org.specs.SpecificationWithJUnit

class MineFieldGridTest extends SpecificationWithJUnit {

    "A MineField Grid of size 1 without mines" should {

        "not be able to be constructed" in {
            MineFieldGrid( 1, 1 ) must throwA[IllegalArgumentException]
        }
    }

    "A MineFieldGrid of size 1 with 1 mine" should {
        "not be able to be constructed" in {
            val grid = MineFieldGrid( 1, 1, ( 0, 0 ) :: Nil ) must throwA[IllegalArgumentException]
        }
    }

    "A MineFieldGrid of size 1 * 2 with 1 mine" should {

        val grid = MineFieldGrid( 1, 2, ( 0, 1 ) :: Nil )

        "not have a mine on the initial field" in {
            grid.uncoverField( ( 0, 0 ) )
            grid( 0, 0 ).triggered must beFalse
            grid.lost must beFalse
        }

        "indicate i won the game" in {
            grid.uncoverField( ( 0, 0 ) )._3 must beTrue
        }

        "have a mine on the 2nd field" in {
            grid.uncoverField( ( 0, 1 ) )
            grid( 0, 1 ).triggered must beTrue
            grid.lost must beTrue
            grid.won must beFalse
        }

        "have a field with 1 adjacent mine in first spot" in {
            grid( 0, 0 ).adjacent must be_==( 1 )
        }

        "throw an Exception when marking an uncovered field" in {
            grid.uncoverField( ( 0, 0 ) )
            grid.markField( ( 0, 0 ) ) must throwA[MineGridException]
        }

        "throw an Exception when invalid positions get passed" in {
            grid.uncoverField( ( -1, 2 ) ) must throwA[MineGridException]
            grid.markField( ( 3, 4 ) ) must throwA[MineGridException]
            grid.unmarkField( 1, -4 ) must throwA[MineGridException]

        }

        "Not uncover a marked field" in {
            grid.markField( ( 0, 0 ) )
            grid.uncoverField( ( 0, 0 ) )
            grid( 0, 0 ).marked must beTrue
            grid( 0, 0 ).uncovered must beFalse
        }

        "return a grid of size 1 * 2 with 1 uncovered untriggered Field with one adjacent mine" in {
            grid.uncoverField( ( 0, 0 ) )
            grid( 0 , 0 ).triggered must beFalse
            grid( 0 , 0 ).adjacent must be_==( 1 )
            grid( 0 , 0 ).uncovered must beTrue
            grid( 0 , 1 ).covered must beTrue
        }

        "return a grid with 1 marked field" in {
            grid.markField( ( 0, 0 ) )
            grid( 0 ,0 ).marked must beTrue
        }

        "return a grid with 1 unmarked field" in {
            grid.markField( ( 0, 0 ) )
            grid.unmarkField( ( 0, 0 ) )
            grid( 0 ,0 ).marked must beFalse
        }

    }

    "A MineFieldGrid of size 3 * 3 with 2 mines" should {

        val grid = MineFieldGrid( 3, 3, ( 1, 2 ) :: ( 2, 1 ) :: Nil )

        "Not have an armed mine in the field" in {
            grid.uncoverField( ( 2, 2 ) )
            grid( 2 , 2 ).triggered must beFalse
            grid.lost must beFalse
        }

        "Have mines in the uncovered fields" in {
            grid.uncoverField( 1, 2 )
            grid.uncoverField( 2, 1 )
            for ( i <- 0 until 3; j <- 0 until 3 ) grid.uncoverField( ( i, j ) )
            val newgrid = grid.getGridState
            newgrid( 1 )( 2 ).triggered must beTrue
            newgrid( 2 )( 1 ).triggered must beTrue
            newgrid.flatten count ( _.triggered ) must be_==( 2 )
            newgrid.flatten count ( _.uncovered ) must be_==( 7 )
            grid.lost must beTrue
            grid.won must beFalse
        }

        "Return a new Grid with 3 mines" in {
            val newgrid = grid.addMine( ( 1, 1 ) )
            newgrid eq grid must beFalse
            newgrid.uncoverField( 1, 1 )
            newgrid.uncoverField( 1, 2 )
            val ( result, loss, won ) = newgrid.uncoverField( 2, 1 )
            result( 1 )( 1 ).triggered must beTrue
            result( 1 )( 2 ).triggered must beTrue
            result( 2 )( 1 ).triggered must beTrue
            result.flatten.count( _.triggered ) must be_==( 3 )
            result.flatten.count( _.covered ) must be_==( 6 )
            loss must beTrue
            won must beFalse
        }

        "Return a new Grid with 1 mine" in {
            val newgrid = grid.removeMine( ( 1, 2 ) )
            newgrid eq grid must beFalse
            newgrid.uncoverField( 2, 1 )
            val ( result, loss, won ) = newgrid.uncoverField( 1, 2 )
            result( 1 )( 2 ).uncovered must beTrue
            result( 2 )( 1 ).triggered must beTrue
            result.flatten.count( _.triggered ) must be_==( 1 )
            loss must beTrue
            won must beFalse
        }
    }

    "A MineFieldGrid of size 100 * 100 without mines" should {
        val grid = MineFieldGrid( 100, 100 )

        "be won immediately and completely uncovered" in {
            val ( state, loss, win ) = grid.uncoverField( 25, 25 )
            state.flatten.forall( _.uncovered ) must beTrue
            loss must beFalse
            win must beTrue
        }
    }

    "A MineFieldGrid of size 2 * 2 with 2 randomly placed mines" should {

        val grid = MineFieldGrid( 2, 2, 2, ( 0, 0 ) :: ( 1, 1 ) :: Nil )

        "not have mines on the excluded positions" in {
            grid.uncoverField( 0, 0 )
            val ( newgrid, loss, win ) = grid.uncoverField( 1, 1 )
            loss must beFalse
            win must beTrue
            newgrid( 0 )( 0 ).uncovered must beTrue
            newgrid( 0 )( 1 ).covered must beTrue
            newgrid( 1 )( 0 ).covered must beTrue
            newgrid( 1 )( 1 ).uncovered must beTrue
        }

        "have mines on the non excluded positions" in {
            grid.uncoverField( 0, 1 )
            val ( newgrid, loss, win ) = grid.uncoverField( 1, 0 )
            loss must beTrue
            win must beFalse
            newgrid( 0 )( 0 ).covered must beTrue
            newgrid( 0 )( 1 ).triggered must beTrue
            newgrid( 1 )( 0 ).triggered must beTrue
            newgrid( 1 )( 1 ).covered must beTrue
        }
    }

    "A MineFieldGrid of size 5 * 5 with 16 randomly placed mines" should {
        val grid = MineFieldGrid( 5, 5, 16, ( 2, 2 ) :: MineFieldGrid.adjacentPos( 2, 2 ) )

        "uncover 8 fields around the center field" in {
            val ( newgrid, loss, win ) = grid.uncoverField( 2, 2 )
            newgrid.flatten.count( _.uncovered ) must be_==( 9 )
            loss must beFalse
            win must beTrue
        }
    }
}