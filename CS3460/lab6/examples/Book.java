public class Book
{
    
    private String author;  
    private String title;   
    private int pages;

    public Book ( String initTitle, String initAuthor, int initPages )
    {
       author = initAuthor;
       title = initTitle;
       pages = initPages;
    }

    public String getAuthor ()
    {
       return author;
    }

    public String getTitle ()
    {
       return title;
    }
 
    public int getNumPages ()
    {
       return pages;
    }
} 
