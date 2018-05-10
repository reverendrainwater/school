import java.util.Random;

public class Testit2
{

  public static void main(String [] args)
  {
   System.out.println( xyzThere("abc.xyzxyz"));  
  }

 public static boolean xyzThere(String str) {
  String str2=str.replaceAll("[.]","Q");;
  System.out.println (str2);
  return str2.contains("xyz");
}
}
