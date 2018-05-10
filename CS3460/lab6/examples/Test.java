import java.util.HashSet;
import java.util.Iterator;


public class Test
{
     public static void main(String [] args)
     {
       HashSet<String> fred = new HashSet<String>();
       Iterator<String> itr;

       fred.add("Horse");
       fred.add("Cat");
       fred.add("Pig");
       fred.add("Goat");
       fred.add("Camel");
       fred.add("Aardvark");
       itr = fred.iterator(); 
       while(itr.hasNext())
       {
           System.out.println (itr.next());
       }
    }
}
