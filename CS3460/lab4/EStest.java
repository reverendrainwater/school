 /////////////////////////////////////////////////////////////////////
 //
 //    IDENTIFICATION DIVISION.
 //    PROGRAM-ID. EStest.java.
 //    AUTHOR. Rev Taylor R Rainwater.
 //    INSTALLATION. prophet.
 //    DATE-WRITTEN. 20.2.2016.
 //    DESCRIPTION. Test class for the EnhancedStack class.
 //
 /////////////////////////////////////////////////////////////////////
import java.util.Scanner;

public class EStest
{
    /**
     * printIntro - prints intro
     */
    public void printIntro()
    {
        System.out.println("=================== DARPA ==================");
        System.out.println("---- WELCOME TO THE WOPR ENHANCED STACK ----");
        System.out.println("         CLEARANCE LEVEL 2A REQUIRED");
        System.out.println("       CONSULT MANUAL 42-2A FOR USAGE");
        System.out.println("            REV TAYLOR RAINWATER");
        System.out.println("============================================\n");
    }
    /**
     * printMenu - prints main menu
     */
    public void printMenu()
    {
        System.out.println("\n------------------- MAIN MENU -------------------");
        System.out.println("1. CHANGE STACK TYPE TO STRING");
        System.out.println("2. PRINT STACK CONTENTS TO CONSOLE");
        System.out.println("3. RETURN TOP OF STACK");
        System.out.println("4. RETURN NODE WITH SMALLEST VALUE");
        System.out.println("5. PUSH INTEGER ONTO STACK");
        System.out.println("6. POP ONCE FROM STACK");
        System.out.println("7. PRINT SIZE OF STACK TO CONSOLE");
        System.out.println("8. PRINT TOP NODE AND SMALL NODES TO CONSOLE");
        System.out.println("9. TERMINATE");
    }
    /**
     * clearConsole - clears the console for a clean look
     */
    public void clearConsole()
    {
        try {
            final String os = System.getProperty("os.name");

            if (os.contains("Windows")) {

            } else {
                System.out.print("\033[H\033[2J");  
                System.out.flush(); 
            }
        } catch (final Exception e) {
            System.err.println(e);
        }
    }
    /**
     * pushOnStack - push to stack 
     * @param stack stack input
     * @param num   value to push
     */
    public void pushOnStack(EnhancedStack<Integer> stack, int num)
    {
        stack.push(num);
    }
    /**
     * printStack - print contents of stack
     * @param stack stack to print
     */
    public void printStack(EnhancedStack<Integer> stack)
    {   int nl = 0;

        for(Integer item : stack){
            if (nl == 0)
                System.out.println("TOP: " + item);
            else
                System.out.println("     " + item);
            nl++;
        }
    }
    /**
     * topOfStack - prints top of stack
     * @param stackObj stack to print from
     */
    public void topOfStack(EnhancedStack<Integer> stackObj)
    {
        System.out.print("\nTOP OF STACK: ");
        System.out.println(stackObj.top());
    }
    /**
     * popNumber - pops a number from stack
     * @param stackObj stack to pop from
     */
    public void popNumber(EnhancedStack<Integer> stackObj)
    {
        System.out.print("\nREMOVED: ");
        System.out.println(stackObj.pop());
    }
    /**
     * sizeOfStack - prints size of stack
     * @param stackObj stack to print from
     */
    public void sizeOfStack(EnhancedStack<Integer> stackObj)
    {
        System.out.print("\nSIZE: ");
        System.out.println(stackObj.size());
    }
    /**
     * stackWithStrings - method for handling stacks with stings
     *                    as opposed to default integer values
     * @param stack stack input
     */
    public void stackWithStrings(EnhancedStack<String> stack)
    {   Scanner scan = new Scanner(System.in);
        int nl = 2;
        
        System.out.print("\nENTER STRINGS SEPARATED BY SPACES: ");
        String s = scan.nextLine();
        String[] strings = s.split(" ");
        scan.close();
        for (String n : strings){
          stack.push(n);
        }
        System.out.println("\nCONTENTS OF STACK: ");
        for(String item : stack){
            System.out.print(item + " ");
            if (nl % 5 == 1)
                System.out.print("\n");
            nl++;
        }
        System.out.println();
        System.out.print("\nMINIMUM ON STACK: ");
        System.out.println(stack.minimum());
        System.out.print("\nTOP OF STACK: ");
        System.out.println(stack.top());
        System.out.print("\nSIZE: ");
        System.out.println(stack.size());
        stack.pop();
        System.out.println("\nAFTER ONE POP:");
        for(String item2 : stack){
            System.out.println(item2 + " ");
        }
        System.out.println();
    }
    /**
     * printSmallLinks - prints smallest 
     * @param stack stack to print from
     */
    public void printSmallLinks(EnhancedStack<Integer> stack)
    {
        stack.printSmallest();
    }
    /**
     * returnMin - returns minimum 
     * @param stack stack to print from
     */
    public void returnMin(EnhancedStack<Integer> stack)
    {
        System.out.println(stack.minimum());
    }
    /**
     * stackWithNumbers - method for handling stacks with numbers,
     *                    this is the default
     * @param stack stack to handle from
     */
    public void stackWithNumbers(EnhancedStack<Integer> stack)
    {   Scanner scan = new Scanner(System.in);
        String[] strings;
        
        System.out.print("\nENTER INTEGERS SEPARATED BY SPACES: ");
        String s = scan.nextLine();
        strings = s.split(" ");
        for (String n : strings){
            stack.push(Integer.parseInt(n));
        }
        scan.close();
    } 
    /**
     * main 
     * @param args cl arguments
     */
    public static void main(String[] args)
    {   int choice;
        Scanner scan = new Scanner(System.in);
        EStest stackObject = new EStest();
        EnhancedStack<Integer> stack = new EnhancedStack<Integer>();
        boolean moreNumbers = false;
        String more;

        // Initial prompts
        stackObject.clearConsole();
        stackObject.printIntro();
        while (moreNumbers == false){    
            stackObject.stackWithNumbers(stack);
            System.out.print("\nMAIN [M]ENU OR ADD MORE [I]NTEGERS: ");
            more = scan.next();
            if(more.equals("M") || more.equals("m"))
                moreNumbers = true;
        }

        boolean foo = true;
        String moreMethods;
        // Main Menu
        while(foo) {
            stackObject.clearConsole();
            stackObject.printMenu();
            System.out.print("\nSELECT METHOD: ");
            choice = scan.nextInt();
            
            switch(choice){
                case 1:
                    stackObject.clearConsole();
                    EnhancedStack<String> stackString = new EnhancedStack<String>();
                    stackObject.stackWithStrings(stackString);
                    break;
                case 2:
                    stackObject.clearConsole();
                    stackObject.printStack(stack);
                    break;
                case 3: 
                    stackObject.clearConsole();
                    System.out.print("\n");
                    stackObject.topOfStack(stack);
                    break;
                case 4:
                    stackObject.clearConsole();
                    stackObject.returnMin(stack);
                    break;
                case 5:
                    System.out.print("\nVALUE TO ADD: ");
                    int number = scan.nextInt();
                    scan.nextLine();
                    stackObject.pushOnStack(stack, number);
                    break;
                case 6:
                    stackObject.popNumber(stack);
                    break;
                case 7:
                    stackObject.sizeOfStack(stack);
                    break;
                case 8:
                    stackObject.printSmallLinks(stack);
                    break;
                case 9:
                    scan.close();
                    System.exit(0);
                    break;
            }
            System.out.print("\n\nRETURN TO MAIN MENU(Y/N): ");
            moreMethods = scan.next();
            if(moreMethods.equals("N") || moreMethods.equalsIgnoreCase("n"))
                foo = false;
        }
        scan.close();
    }
}


