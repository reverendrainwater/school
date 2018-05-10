import java.util.Scanner;
public class Think
{

    public static void main (String [] args)
    {
       Scanner scan = new Scanner(System.in);
       int x;
       System.out.println ("Enter a value: ");
       x = scan.nextInt();
       String animal = "cat";
       if (x <= 4 || x >= 50) {
           if (x < 0) {
                 animal = "dog";
           }
           else
                animal = "horse";
          
      }
      else if (x < 20 && x > 10) {
          animal = "pig";
      }
      else if (x != 22 && x > 20) {
           animal = "chicken";
      }
     System.out.println (animal);
    }
}

