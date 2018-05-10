import java.util.ArrayQueue;

public class TryThis

{

   public static void main(String[] args)

   {

 
      ArrayQueue<String>[] queues =  (ArrayQueue<String>[]) new Object[3];

      String[] words = {"zero", "one", "two", "three"};

      for (int i = 0; i < 3; i++)
 
      {

          for (int j = 0; j < 4; j++)

          {

              queues[i].add(words[j]);
 
         }

      }

      System.out.println(queues[2].remove());
 
   }

}
