public class Two 
{
    private String hi;

    public Two()
    {
       hi = "Hi";
    }

    public String getHi()
    {
        return hi;
    }

    public static void main(String [] args)
    {
        Two two = new Two();
        One one = new One();
        if (one.getHi() == two.getHi())
           System.out.println ("After compile... hmm");
        else
           System.out.println ("I hope it is this.");

    }
}
