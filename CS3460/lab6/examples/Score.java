public class Score
{
	private double rating;
    private String judgeName;

	public Score ( double initRating , String name )
	{
		rating = initRating;
		judgeName = name;
	}

	public double getRating( )
	{
	    return rating;    
	}
	
	public String getJudgeName ( )
	{
		return judgeName;
	}
	
	public void setJudgeName ( String name )
	{
	    judgeName = name;
	}
	
	public void setRating ( double newRating )
	{
	    rating = newRating;
	}
}
