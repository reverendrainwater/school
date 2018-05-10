import java.util.ArrayList;
import java.util.Random;

public class Testit2
{

  public static void main(String [] args)
  {
     ArrayList<Integer> list1 = new ArrayList<Integer>();
     Random rand = new Random();

     for (int i = 0; i < 50; i++)
     {
         list1.add(i+10);
     }
    list1.remove(43);

    for (Integer i : list1)
       System.out.println(i);
  }
}
