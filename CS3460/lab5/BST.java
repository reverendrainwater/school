/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	  PROGRAM-ID. BST.java.
//	      AUTHOR. Rev Taylor R Rainwater.
//	INSTALLATION. prophet.
//	DATE-WRITTEN. 26.2.2016.
//	 DESCRIPTION. Lab 5: BST (Binary Search Tree).
//
/////////////////////////////////////////////////////////////////////

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Random;
import java.util.Scanner;

public class BST
{
    //            BINARY SEARCH TREE FIELDS
    private BinaryNode root;     // reference to the root of the tree
    private int treeSize;           // number of elements in the tree

    public BST()
    {
        root = null;
        treeSize = 0;
    }

    //             BINARY SEARCH TREE METHODS
    public int size()
    {
        return treeSize;
    }

    public boolean isEmpty()
    {
        return root == null;
    }

    // MENU ITEM #1 and #2
    public void insert(int item)
    {
        root = insert(item, root);
    }

    // MENU ITEM #3
    public void printInOrder()
    {
        printInOrder(root);
    }

    // MENU ITEM #4
    public int height()
    {
        return height(root);
    }

    // MENU ITEM #5
    public boolean contains(int item)
    {
        return find(item, root);
    }

    // MENU ITEM #6
    public int sumLeaves()
    {
        return sumLeaves(root);
    }

    // MENU ITEM #7
    public int countLeaves()
    {
        return countLeaves(root);
    }

    // MENU ITEM #8
    public int sumNodes()
    {
        return sumNodes(root);
    }

    // MENU ITEM #9
    public int countOdds()
    {
        return countOdds(root);
    }

    // MENU ITEM #10
    public void printPreOrder()
    {
        printPreOrder(root);
    }

    // MENU ITEM #11
    public void printPostOrder()
    {
        printPostOrder(root);
    }

    // MENU ITEM #12
    public void printInReverse()
    {
        printInReverse(root);
    }

    public void printTree()
    {	Writer out = new BufferedWriter(new OutputStreamWriter(System.out));
        try {
			BTreePrint.printTree(out, root);
		} catch (IOException e) {
			e.printStackTrace();
		}
    }

    // MENU ITEM #13
    public int countTwoChildren()
    {
        return countTwoChildren(root);
    }

    // MENU ITEM #14
    public int findMin()
    {
        return findMin(root);
    }

    /**
     * findMax
     * @return value of max node
     */
    public int findMax()
    {   BinaryNode curr = root;

        while (curr.right != null)
            curr = curr.right;
        return curr.key;
    }

    /**
     * secondLargest
     * @return second largest value
     */
    public int secondLargest()
    {   BinaryNode curr = root;

        while (curr.right.right != null){
                curr = curr.right;
        }
        return curr.key;
    }

    // MENU ITEM #17
    public boolean remove(int num)
    {   BinaryNode curr = root;

        curr = remover(num, curr);
        return (curr != null);
    }

    // MENU ITEM #18
    public int findZigZagLength()
    {
        // You may make this iterative or recursive
        // If you make it recursive, then you need to add the method
        return 0;
    }

    // MENU ITEM #19
    public void clear()
    {
        clear(root);
        root = null;
    }


    // PRIVATE METHOD: Menu #1 and 2:
    private BinaryNode insert(int item, BinaryNode t)
    {
        if (t == null) {
            treeSize++;
            return new BinaryNode(item);
        }
        if (t.key < item)
            t.right = insert(item, t.right);
        else if (t.key > item)
            t.left = insert(item, t.left);
        // does not insert duplicates
        return t;
    }

    // PRIVATE METHOD: Menu #3:
    private void printInOrder(BinaryNode t)
    {
        if (t != null) {
            printInOrder(t.left);
            System.out.printf("%,12d\n", t.key);
            printInOrder(t.right);
        }
    }

    // private void printTreeDiagram(BinaryNode t)
    // {   int height = height(t);

    //     if (t.left != null && t.right != null)

    // }

    // Menu #4: height of singleton  is 0,
    //          height of empty tree is -1.
    private int height(BinaryNode t)
    {
        if (t == null)
            return -1;
        int leftHeight = height(t.left);
        int rightHeight = height(t.right);
        if (leftHeight >= rightHeight)
            return 1 + leftHeight;
        return 1 + rightHeight;
    }

    // Menu #5
    private boolean find (int item, BinaryNode t)
    {
        if (t == null)
            return false;
        if (t.key == item)
            return true;
        if (t.key < item)
            return find (item, t.right);
        return find (item, t.left);
    }

    /**
     * sumLeaves
     * @param  t BinaryNode input
     * @return   sum of leaves
     */
    private int sumLeaves(BinaryNode t)
    {
        if (t == null)
            return 0;
        if (t.left != null || t.right != null)
            return (sumLeaves(t.left) + sumLeaves(t.right));
        else
            return t.key;           
    }

    /**
     * countLeaves 
     * @param  t BinaryNode input
     * @return   count of leaves 
     */
    private int countLeaves(BinaryNode t)
    {
        if (t == null)
            return 0;
        if (t.left != null || t.right != null)
            return (countLeaves(t.left) + countLeaves(t.right));
        else
            return 1;
    }

    /**
     * sumNodes
     * @param  t BinaryNode input
     * @return   sum of nodes
     */
    private int sumNodes(BinaryNode t)
    {
        if (t == null)
            return 0;
        else
            return sumNodes(t.left) + sumNodes(t.right) + t.key;
    }

    /**
     * countOdds 
     * @param  t BinaryNode input
     * @return   count of odd nodes
     */
    private int countOdds(BinaryNode t)
    {
        if (t == null)
            return 0;
        else 
            return countOdds(t.left) + countOdds(t.right) 
                   + ((t.key % 2 != 0) ? 1 : 0);
    }

    /**
     * printPreOrder    
     * @param t BinaryNode input
     */
    private void printPreOrder(BinaryNode t)
    {
        if(t != null)
            System.out.println(t.key);
        if (t.left != null)
            printPreOrder(t.left);
        if (t.right != null)
            printPreOrder(t.right);
    }

    /**
     * printPostOrder
     * @param t BinaryNode input
     */
    private void printPostOrder(BinaryNode t)
    {
        if (t.left != null)
            printPreOrder(t.left);
        if (t.right != null)
            printPreOrder(t.right);
        if(t != null)
            System.out.println(t.key);
    }

    /**
     * printReverse
     * @param t BinaryNode input
     */
    private void printInReverse(BinaryNode t)
    {
        if (t.right != null)
            printPreOrder(t.right);
        if(t != null)
            System.out.println(t.key);
        if (t.left != null)
            printPreOrder(t.left);
    }

    /**
     * countTwoChildren
     * @param  t BinaryNode input
     * @return   count of nodes with two children
     */
    private int countTwoChildren(BinaryNode t)
    {
        if (t == null)
            return 0;
        else if (t.left != null && t.right != null)
            return countTwoChildren(t.left) + countTwoChildren(t.right)
                   + 1;
        else
            return countTwoChildren(t.left) + countTwoChildren(t.right);
    }

    /**
     * findMin
     * @param  t BinaryNode input
     * @return   minimum value in tree
     */
    private int findMin(BinaryNode t)
    {
        if (t == null)
            return 0;
        if (findMin(t.left) < findMin(t.right)) 
            return findMin(t.left);
        else if (findMin(t.right) < t.key)
            return findMin(t.right);
        else
            return t.key;
    }

    /**
     * remover
     * @param  num value to remove
     * @param  t   root node
     * @return     BinaryNode output
     */
    private BinaryNode remover(int num, BinaryNode t)
    {
        if (t == null)
            return t;
        if (num < t.key)
            t.left = remover(num, t.left);
        else if (num > t.key)
            t.right = remover(num, t.right);
        else {
            if (t.left == null)
                return t.right;
            else if (t.right == null)
                return t.left;
            t.key = findMin(t.right);
            t.right = remover(t.key, t.right);
        }
        return t;
    }

    // MENU ITEM #18
    private void clear(BinaryNode t)
    {
        if (t == null) return;
        if (t.left != null) {
            clear(t.left);
            t.left = null;
        }
        if (t.right != null) {
            clear(t.right);
            t.right = null;
        }
    }


    public static void main(String [] args)
    {
        BST tree = new BST();
        int howMany, num;
        Random rand = new Random();
        Scanner scan = new Scanner(System.in);

        clearConsole();
        intro();
        int choice = menu(scan);
        while (choice < 20) {
            clearConsole();
            switch (choice) {
            case 1: System.out.print ("How many random integers: ");
                howMany = scan.nextInt() + tree.size();
                while (tree.size() < howMany)
                    tree.insert(rand.nextInt((50) + 1));
                break;
            case 2: System.out.print ("How many integers: ");
                howMany = scan.nextInt();
                for (int i = 1; i <= howMany; i++) {
                    System.out.print ("Enter number " + i + ".  :");
                    num = scan.nextInt();
                    tree.insert(num);
                }
                break;
            case 3: tree.printInOrder();
                break;
            case 4: System.out.println ("Height " + tree.height());
                break;
            case 5: System.out.print ("Enter number to look for: " );
                num = scan.nextInt();
                if (tree.contains(num))
                    System.out.println (num + " is there.");
                else
                    System.out.println (num + " isn't there.");
                break;
            case 6: System.out.println ("The sum of the leaves is " +
                                            tree.sumLeaves());
                break;
            case 7: System.out.println ("There are " +
                                            tree.countLeaves() + " leaves.");
                break;
            case 8: System.out.println ("The sum of the nodes is " +
                                            tree.sumNodes());
                break;
            case 9: System.out.println ("There are " +
                                            tree.countOdds() + " odd integers.");
                break;
            case 10: tree.printPreOrder();
                break;
            case 11: tree.printPostOrder();
                break;
            case 12: tree.printInReverse();
                break;
            case 13: System.out.println ("There are " +
                                             tree.countTwoChildren() + " nodes with 2 children.");
                break;
            case 14: System.out.println ("The minimum found recursively " +
                                             " is " + tree.findMin());
                break;
            case 15: System.out.println ("The maximum found iteratively " +
                                             " is " + tree.findMax());
                break;
            case 16: System.out.println ("The second largest is " +
                                             + tree.secondLargest());
                break;
            case 17: System.out.print ("Enter number to remove: ");
                num = scan.nextInt();
                boolean removed = tree.remove(num);
                if (removed)
                    System.out.println ("It is gone.");
                else
                    System.out.println ("That value is not there.");
                break;
            case 18: tree.clear();
                break;
            case 19: 
                if (tree.height() <= 10)
                    tree.printTree();
                else
                    System.out.println("Tree height too large.");
            }
            System.out.print("Enter zero to continue: ");
            num = scan.nextInt();
            choice = menu(scan);
        }
    }

    /**
     * clearConsole - clears the console for a clean look
     */
    public static void clearConsole()
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
     * printIntro - prints intro
     */
    public static void intro()
    {
        System.out.println("===================== DARPA =====================");
        System.out.println("  ---- WELCOME TO WOPR BINARY SEARCH TREE ----");
        System.out.println("          CLEARANCE LEVEL 3A REQUIRED");
        System.out.println("        CONSULT MANUAL 35-3A FOR USAGE");
        System.out.println("             REV TAYLOR RAINWATER");
        System.out.println("=================================================");
    }

    public static int menu(Scanner cin)
    {
        int choice = 0;

        System.out.println("\n**************************************************");
        System.out.println("           BST TEST METHODS  ");
        System.out.println("**************************************************");
        System.out.println("       1.  INSERT SOME RANDOM NUMBERS ");
        System.out.println("       2.  INSERT SOME SPECIFIC NUMBERS");
        System.out.println("       3.  PRINT THE TREE IN ORDER");
        System.out.println("       4.  DETERMINE THE TREE HEIGHT ");
        System.out.println("       5.  FIND A SPECIFIC NUMBER");
        System.out.println("       6.  SUM THE LEAVES                   #TRR");
        System.out.println("       7.  COUNT THE LEAVES                 #TRR");
        System.out.println("       8.  SUM THE NODES IN THE TREE        #TRR");
        System.out.println("       9.  COUNT THE NUMBER OF ODD NODES    #TRR");
        System.out.println("      10.  PRINT THE TREE IN PRE-ORDER      #TRR");
        System.out.println("      11.  PRINT THE TREE IN POST-ORDER     #TRR");
        System.out.println("      12.  PRINT THE NODES IN REVERSE ORDER #TRR");
        System.out.println("      13.  COUNT THE NODES WITH 2 CHILDREN  #TRR");
        System.out.println("      14.  RETURN THE MINIMUM ITEM          #TRR");
        System.out.println("      15.  RETURN THE MAXIMUM ITEM          #TRR");
        System.out.println("      16.  RETURN THE 2ND LARGEST           #TRR");
        System.out.println("      17.  REMOVE A SPECIFIC NUMBER         #TRR");
        System.out.println("      18.  CLEAR TREE");
        System.out.println("      19.  PRINT DIAGRAM                    #TRR");
        System.out.println("      20.  TERMINATE");
        System.out.println("**************************************************");

        System.out.print("\n\nENTER YOUR CHOICE: ");
        choice = cin.nextInt();

        if (choice < 1 || choice > 20)
            choice = 20;

        return choice;
    }

}

