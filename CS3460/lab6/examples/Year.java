
public class Year
{
    public enum Month {
        JANUARY, FEBRUARY, MARCH, APRIL, MAY, JUNE, JULY,
        AUGUST, SEPTEMBER,OCTOBER, NOVEMBER, DECEMBER
    };

    private static Month[] monthArray = Month.values();

    public static void main(String[] args)
    {
        if (monthArray[3].equals(APRIL))
            System.out.println("yes");
        else
            System.out.println("no");
        System.exit(0);
    }
}

