object foobar {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(56); 
  println("haskell ist trotzdem besser");$skip(13); 
 val i = 1+2;System.out.println("""i  : Int = """ + $show(i ));$skip(15); 
 val j = i + 7;System.out.println("""j  : Int = """ + $show(j ))}
}