package minesweeper.model

import org.specs2.mutable.SpecificationWithJUnit

class MineFieldTest extends SpecificationWithJUnit {

    "An unmarked covered MineField" should {

        val field = MineField()

        "have no adjacent Mines" in {
            field.adjacent must be_==( 0 )
        }

        "be armed" in {
            field.armed must beTrue
        }

        "be not marked" in {
            field.marked must beFalse
        }

        "be covered" in {
            field.covered must beTrue
        }

        "return a marked MineField" in {
            val newfield = field.mark
            newfield.marked must beTrue
        }

        "return a triggered Field" in {
            val newfield = field.uncover()
            newfield.triggered must beTrue
        }
    }

    "An marked covered MineField" should {
        
        val field = MineField.marked

        "be marked" in {
            field.marked must beTrue
        }

        "return an unmarked Field" in {
            val newfield = field.unmark
            newfield.marked must beFalse
        }

        "throw an Exception when " in {
            field.uncover() must throwA[ IllegalArgumentException ]
        }

    }

    "A triggered MineField" should {

        val field = MineField.triggered
        
        "be triggered" in {
            field.triggered must beTrue
        }

        "throw an Exception when getting marked" in {
            field.mark must throwA[ IllegalArgumentException ]
        }
    }
}