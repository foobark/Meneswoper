package minesweeper.model

abstract case class MineFieldState( val adjacent: Int ) {
    def uncovered: Boolean = false
    def marked: Boolean = false
    def triggered: Boolean = false
    def covered: Boolean = false
}

object MineFieldState {

    class covered( override val adjacent: Int = 0 ) extends MineFieldState( adjacent ) {
        override def covered: Boolean = true

        override def toString() = "H"
    }

    object covered { def apply( adjacent: Int = 0): covered = new covered( adjacent ) }

    class marked( override val adjacent: Int = 0 ) extends MineFieldState( adjacent ) {
        override def marked: Boolean = true

        override def toString() = "#"
    }

    object marked { def apply( adjacent: Int = 0): marked = new marked( adjacent ) }

    class triggered extends MineFieldState( 0 ) {
        override def triggered: Boolean = true

        override def toString() = "*"
    }

    object triggered { def apply(): triggered = new triggered() }

    class uncovered( override val adjacent: Int = 0 ) extends MineFieldState( adjacent ) {
        override def uncovered: Boolean = true

        override def toString() = adjacent.toString
    }

    object uncovered { def apply( adjacent: Int = 0): uncovered = new uncovered( adjacent ) }
}