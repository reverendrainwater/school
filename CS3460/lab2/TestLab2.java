/**
 * TestLab2.java
 * 
 */
import java.util.Random;
import java.util.Scanner;
 
/**
 * Counting out-of-order pairs in an array.
 * 
 * This program compares the times for two different algorithms for
 * computing the number of out-of-order pairs in an array.
 *
 * @author Alice McRae
 * @version 7/31/2015 
 */
public class TestLab2
{
    public static final int DOUBLE_NESTED_LOOP = 1;
    public static final int COUNT_AS_YOU_SORT = 2;
    public static final int CHANGE_VALUES = 3;
    public static final int CHANGE_SIZE_AND_VALUES = 4;
    public static final int CHOOSE_YOUR_OWN_VALUES = 5;
    public static final int CHANGE_MAX_VALUE = 6;
    public static final int PRINT_ARRAY = 7;
    public static final int QUIT = 8;
    public static final int NUM_PER_LINE = 5;

    private static Random ran = new Random();
    private int [] testArray;
    private Scanner scan;
    private int cap;
    private boolean maxCapDesired;

    
    public TestLab2()
    {
        testArray = null;
        scan = new Scanner(System.in);
        cap = 500000;
        maxCapDesired = false;
    }

    /**
     * Fills testArray with random integers.
     * 
     */
    public void fillArray()
    {
        if (maxCapDesired)
        {
           for (int i = 0; i < testArray.length; i++)
           {
              testArray[i] = ran.nextInt(cap);
           }
        }
        else
        {
           for (int i = 0; i < testArray.length; i++)
           {
              testArray[i] = ran.nextInt();
           }
        }
    }

    /**
     * Fills testArray with user-supplied integers.
     * 
     */
    public void scanArray()
    {
        for (int i = 0; i < testArray.length; i++)
        {
            System.out.print ("Enter number " + i + ": ");
            testArray[i] = scan.nextInt();
        }
        System.out.println();
    }

    /**
     * Allows user to change the largest maximum value.
     * 
     */
    public void askForMax()
    {
        int choice;

        maxCapDesired = true;
        System.out.println ("1. Please cap the maximum.");
        System.out.println ("2. Allow any integers.");
        System.out.print ("Your choice: ");
        choice = scan.nextInt();
        if (choice == 1)
        {
           System.out.print ("Enter the maximum: ");
           cap = scan.nextInt();
        }
        else if (choice == 2)
        {
           maxCapDesired = false;
        }
        else
        {
           // impatient user has entered cap?
           cap = choice; 
        }
    }
    
    
    /**
     * Ask user for array size and allocates.
     * 
     * @param message   Prompt for the user 
     */
    public void fixArraySize(String message)
    {
        int numElements;
        System.out.print(message);
        numElements = scan.nextInt();
        if (testArray == null || numElements != testArray.length)
        {
            testArray = null;   // helps garbage-collection?
            testArray = new int[numElements];
        }
    }
    
    
    /**
     * Prints the contents of the testArray.
     * 
     */
    public void printArray()
    {
        System.out.println();
        for (int i = 0; i < testArray.length; i++)
        {
            System.out.printf("%,12d\t", testArray[i]);
            if (i % NUM_PER_LINE == NUM_PER_LINE - 1)
            {
                System.out.println();
            }
        }
        if (testArray.length % NUM_PER_LINE != 0)
        {
            System.out.println();
        }
    }
    
    
    /**
     * Asks the user what to do next.
     * 
     * @return      the menu item chosen by the user
     */
    
    public int menu()
    {
        int choice;

        System.out.println();
        System.out.println("***********************************************");
        System.out.println("           TESTING OPTIONS  ");
        System.out.println("***********************************************");
        System.out.println("      1.  Algorithm #1 ");
        System.out.println("      2.  Algorithm #2 ");
        System.out.println("      3.  Try different random values");
        System.out.println("      4.  Change array size and random values");
        System.out.println("      5.  Choose your own values");
        System.out.println("      6.  Cap the max possible value and refill");
        System.out.println("      7.  Print the array");
        System.out.println("      8.  Quit  ");
        System.out.println("***********************************************");
        System.out.print("\n\nEnter your choice: ");
        choice = scan.nextInt();
        return choice;
    }
   
    /**
     * Driver for the test program 
     * 
     */
    public void test()
    { 
        int choice;
        long starttime = 0;
        long finishtime;
        int answer = 0;

        fixArraySize("Enter the original array size: ");
        fillArray();

        choice = menu();
        while (choice != QUIT)
        {
            if (choice ==  DOUBLE_NESTED_LOOP 
                || choice ==  COUNT_AS_YOU_SORT)
            {
                starttime = System.currentTimeMillis();
            }
            switch (choice)
            {
                case DOUBLE_NESTED_LOOP: 
                    answer = Lab2.countOutOfOrder1(testArray);
                    break;
                case COUNT_AS_YOU_SORT:  
                    answer = Lab2.countOutOfOrder2(testArray);
                    break;
                case CHANGE_VALUES:
                    fillArray();
                    break;
                case CHANGE_SIZE_AND_VALUES:
                    fixArraySize("How many random numbers: ");
                    fillArray();
                    break;
                case CHOOSE_YOUR_OWN_VALUES:
                    fixArraySize("How many values are you entering: ");
                    scanArray();
                    break;
                case CHANGE_MAX_VALUE:
                    askForMax();
                    fillArray();
                    break;
                case PRINT_ARRAY:
                    printArray();
                    break;
                default:
                    System.out.println("Invalid choice: Select again\n");
            }
            if (choice ==  DOUBLE_NESTED_LOOP 
                || choice ==  COUNT_AS_YOU_SORT)
            {
                finishtime = System.currentTimeMillis();
                System.out.println("There were " + answer + " out-of-order.");
                System.out.println("The time to complete is " 
                     + (finishtime - starttime) + " time units.\n\n\n\n");
            }
            choice = menu();
        }
    }

    /**
     * main method for the test program 
     * 
     * @param args   unused
     */
    public  static void main(String args[])
    {
        TestLab2 testing = new TestLab2();
        testing.test();
    }
}
