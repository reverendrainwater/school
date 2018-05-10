/////////////////////////////////////////////////////////////////////
//
//    IDENTIFICATION DIVISION.
//    PROGRAM-ID. Lab2.java.
//    AUTHOR. Rev Taylor Rainwater.
//    INSTALLATION. prophet.
//    DATE-WRITTEN. 29.1.2016.
//    DESCRIPTION. Lab 2: Count Out Of Order and MergeCount.
//
/////////////////////////////////////////////////////////////////////

import java.util.Random;
import java.util.Scanner;

public class Lab2
{
    /**
     * Computes number out-of-order by checking every pair.
     * 
     * @param a    A list of integers
     * @return     The number of pairs out of order
     *             in the list 
     */
    public static int countOutOfOrder1(int[] n)
    {   int swp = 0;
        int len = n.length;

        for (int i = 0; i < len; i++){
            for (int j = i + 1; j < len; j++){
                if (n[i] > n[j])
                    swp++;
            }
        }
        return swp;
    }

    /**
     * Computes number out-of-order while doing a mergesort.
     * 
     * @param a    A list of integers
     * @return     The number of pairs out of order
     *             in the list 
     */
    public static int countOutOfOrder2(int[] n)
    {
        return mergeCount(n);
    }

    public static int mergeCount(int[] a)
    {   int[] tmp = new int[a.length];

        return mergeCount(a, tmp,  0,  a.length - 1);
    }


    private static int mergeCount(int[] a, int[] tmp, int left, int right)
    {   int swp = 0; 

        if( left < right )
        {   int center = (left + right) / 2;

            swp = mergeCount(a, tmp, left, center);
            swp += mergeCount(a, tmp, center + 1, right);
            swp += merge(a, tmp, left, center + 1, right);
        }
        return swp;
    }


    private static int merge(int[] a, int[] tmp, int left, int right, int rightEnd )
    {   int leftEnd = right - 1;
        int k = left;
        int num = rightEnd - left + 1;
        int mid = right;
        int swp = 0;

        while(left <= leftEnd && right <= rightEnd){
            if(a[left] <= a[right]) {
                tmp[k++] = a[left++];
            } else {
                tmp[k++] = a[right++];
                swp += (mid - left);
            }
        }
        while(left <= leftEnd)                      // Copy rest of first half
            tmp[k++] = a[left++];
        while(right <= rightEnd)                    // Copy rest of right half
            tmp[k++] = a[right++];
        for(int i = 0; i < num; i++, rightEnd--)    // Copy tmp back
            a[rightEnd] = tmp[rightEnd];
        return swp;
    }

}
