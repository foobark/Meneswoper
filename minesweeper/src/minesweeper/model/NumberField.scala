package minesweeper.model

class NumberField(override val state: MineFieldState ) extends Field( false, state ) {

    def mark(): Field = {
        checkMarkedConstraint
        new NumberField( MineFieldState.marked( adjacent ) )
    }

    def unmark(): Field = {
        checkUnmarkConstraint
        new NumberField( MineFieldState.covered( adjacent ) )
    }

    def uncover(): Field = {
        checkUncoveredConstraint
        new NumberField( MineFieldState.uncovered( adjacent ) )
    }

    override def toString() = "NumberField - state: " + state.toString
}

