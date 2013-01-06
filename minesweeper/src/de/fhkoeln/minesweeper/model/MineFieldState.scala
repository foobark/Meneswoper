package de.fhkoeln.minesweeper.model

abstract case class MineFieldState(val adjacent: Int) {
    def uncovered: Boolean = false
    def marked: Boolean = false
    def triggered: Boolean = false
    def covered: Boolean = false
}

object MineFieldState {
    
    case class covered(override val adjacent: Int = 0) extends MineFieldState(adjacent) {
        override def covered:Boolean = true
    }
    
    case class marked(override val adjacent: Int = 0) extends MineFieldState(adjacent) {
        override def marked:Boolean = true
    }
    
    case class triggered extends MineFieldState(0) {
        override def triggered:Boolean = true
    }
    
    case class uncovered(override val adjacent: Int = 0) extends MineFieldState(adjacent) {
        override def uncovered:Boolean = true
    }   
}