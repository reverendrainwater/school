/////////////////////////////////////////////////////////////////////
//
//	IDENTIFICATION DIVISION.
//	PROGRAM-ID. sort.java.
//	AUTHOR. Rev Taylor Rainwater.
//	INSTALLATION. student.
//	DATE-WRITTEN. 13.1.2015.
//	DESCRIPTION. pivot sort.
//
/////////////////////////////////////////////////////////////////////
import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;

public class sort 
{
	public static void main(String[] args) throws IOException 
	{	Scanner kb = new Scanner(System.in);
		String filen;

		System.out.print("Enter FULL (with extension) Filename: ");
		filen = kb.nextLine();
		System.out.println("");
		Scanner inFile = new Scanner(new File(filen));
    	List<Integer> temps = new ArrayList<Integer>();
    	while (inFile.hasNext()) {
      		int token1 = inFile.nextInt();
      		temps.add(token1);
    	}
    	kb.close();
    	inFile.close();

    	for (int s : temps) {
      		System.out.println(s);
    	}
  	}
}