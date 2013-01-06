package de.fhkoeln.minesweeper.model

case class NumberField(override val state: MineFieldState ) extends Field( false, state ) {

    def mark(): Field = {
        checkMarkedConstraint
        NumberField( MineFieldState.marked( adjacent ) )
    }

    def unmark(): Field = {
        checkUnmarkConstraint
        NumberField( MineFieldState.covered( adjacent ) )
    }

    def uncover(): Field = {
        checkUncoveredConstraint
        NumberField( MineFieldState.uncovered( adjacent ) )
    }

}

