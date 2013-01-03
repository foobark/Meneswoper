package de.fhkoeln.minesweeper.model

import org.specs._

class MineFieldTest extends SpecificationWithJUnit {
	
  "An empty unmarked covered MineField" should {
	  
    val field = MineField()
	
    "have no adjacent Mines" in {
	    field.adjacent must be_==(0)
	  }
	  
	  "be not armed" in {
	    field.armed must beFalse
	  }
	  
	"be not marked" in {
	  field.marked must beFalse
	}
	
	"be covered" in {
	  field.uncovered must beFalse
	}
	
	"be marked" in {
	  field.marked = true
	  field.marked must beTrue
	}
	
	"be uncovered" in {
	  field.uncover()
	  field.uncovered must beTrue
	}
	}
  
  "An empty marked covered MineField" should {
    val field = MineField(_marked = true)
    
    "be marked" in {
      field.marked must beTrue
    }
    
    "be not marked" in {
      field.marked = false
      field.marked must beFalse
    }
    
    "throw an Exception" in {
      field.uncover() must throwA[IllegalArgumentException]
    }
        
  }
  
  "An empty uncovered MineField" should {
    
    val field = MineField(_uncovered = true)
    
    "be uncovered" in {
      field.uncovered must beTrue
    }
    
    "throw an Exception when getting marked" in {
      (field.marked = true) must throwA[IllegalArgumentException]
    }
    
  }
}