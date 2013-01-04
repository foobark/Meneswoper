class Flop {}

object foobar {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(66); 
  var foo = Array.ofDim[Flop](1, 1);System.out.println("""foo  : Array[Array[Flop]] = """ + $show(foo ));$skip(9); val res$0 = 
  foo(0);System.out.println("""res0: Array[Flop] = """ + $show(res$0));$skip(20); val res$1 = 
  foo(0)(0) == null;System.out.println("""res1: Boolean = """ + $show(res$1));$skip(12); val res$2 = 
  
  1 to 5;System.out.println("""res2: scala.collection.immutable.Range.Inclusive = """ + $show(res$2));$skip(57); 
  for( i <- 0 until 1; j <- 0 until 1 ) println( (i,j) )}
}