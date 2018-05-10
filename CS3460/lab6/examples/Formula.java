import java.util.Scanner;

public class Formula
{
  public static void main ( String [] args )
  {
    double x, y, answer1, answer2;
    Scanner scan = new Scanner ( System.in );

    System.out.println ("Enter number 1: ");
    x = scan.nextDouble (); 
    answer1 = formula ( x );
    System.out.println ("Enter number 2: ");
    y = scan.nextDouble (); 
    answer2 = formula ( y );

    System.out.println ("f(y) / f(x) = " + answer2 + "/" + answer1  +
                            " = " + ( answer2 / answer1 ));
  }

public static double formula(double x)
{
    return 7 * x * x * x - 40 * x * x + 10 * x - 3;
}

}     
