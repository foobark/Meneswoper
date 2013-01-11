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
            val newgrid = grid.uncoverField( ( 0, 0 ) )
            newgrid._1( 0 )( 0 ).triggered must beFalse
            newgrid._2 must beFalse
        }

        "indicate i won the game" in {
            grid.uncoverField( ( 0, 0 ) )._3 must beTrue
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
            val newgrid = grid.uncoverField( ( 0, 0 ) )
            newgrid._1( 0 )( 0 ).marked must beTrue
            newgrid._1( 0 )( 0 ).uncovered must beFalse
        }

        "return a grid of size 1 * 2 with 1 uncovered untriggered Field with one adjacent mine" in {
            val newgrid = grid.uncoverField( ( 0, 0 ) )._1
            newgrid.size must be_==( 1 )
            newgrid( 0 ).size must be_==( 2 )
            newgrid( 0 )( 0 ).triggered must beFalse
            newgrid( 0 )( 0 ).adjacent must be_==( 1 )
            newgrid( 0 )( 0 ).uncovered must beTrue
            newgrid( 0 )( 1 ).covered must beTrue
        }

        "return a grid of size 1 * 2 with 1 marked field" in {
            val newgrid = grid.markField( ( 0, 0 ) )
            newgrid.size must be_==( 1 )
            newgrid( 0 ).size must be_==( 2 )
            newgrid( 0 )( 0 ).marked must beTrue
        }

        "return a grid of size 1 * 2 with 1 unmarked field" in {
            grid.markField( ( 0, 0 ) )
            val newgrid = grid.unmarkField( ( 0, 0 ) )
            newgrid.size must be_==( 1 )
            newgrid( 0 ).size must be_==( 2 )
            newgrid( 0 )( 0 ).marked must beFalse
        }

    }

    "A MineFieldGrid of size 3 * 3 with 2 mines" should {

        val grid = MineFieldGrid( 3, 3, ( 1, 2 ) :: ( 2, 1 ) :: Nil )

        "Not have an armed mine in the field" in {
            val newgrid = grid.uncoverField( ( 2, 2 ) )
            newgrid._1( 2 )( 2 ).triggered must beFalse
            newgrid._2 must beFalse
        }

        "Have mines in the uncovered fields" in {
            grid.uncoverField(1, 2)
            grid.uncoverField(2, 1)
            for ( i <- 0 until 3; j <- 0 until 3 ) grid.uncoverField( ( i, j ) )
            val newgrid = grid.getGridState
            newgrid(1)(2).triggered must beTrue
            newgrid(2)(1).triggered must beTrue
            newgrid.flatten count ( _.triggered ) must be_==( 2 )
            newgrid.flatten count (_.uncovered ) must be_== (7)
            grid.lost must beTrue
            grid.won must beFalse
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
}