public class Test4 
{ 
   public static void main(String [] args) 
   {
        System.out.println (mystery(173,16));
   }

public static String mystery(int num, int b) 
{
       String digitChar = "0123456789ABCDEF";
       Stack<Character> stack = new Stack<Character>();
       String answer = "";
       do {
            stack.push(digitChar.charAt(num%b));
            num /= b;
       } while (num != 0);

       while (!stack.isEmpty()) 
              answer += stack.pop();
       return answer;
}
}

