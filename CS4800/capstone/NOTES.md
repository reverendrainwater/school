# Programmer Notes for YESSViz

## Lanks 2 stuff
- [ncurses howto](http://tldp.org/HOWTO/NCURSES-Programming-HOWTO/index.html) - super simple and easy to follow
- [ncurses tutorial](http://edlinuxeditor.blogspot.com/p/ncurses-library-tutorial.html) - a little more advanced than the howto
- [ncurses tutorial video series](https://youtu.be/2tWN6ntNo4w) - a series by youtuber [Giga raptor](https://www.youtube.com/channel/UCWM9F4d53xWCNcpixy9WIBA)

---

##  Personal Notes

20170912 - Ncurses is a lot easier to use than anticipated.  Currently not much progress beyond learning some ncurses has been made since the past week has been quite hectic with various tests and large amount of work. 

20170920a - Why is javascript such garbage, I mean seriously. Who in their right mind thinks making changes so drastic as to break someone's whole project in _one version_? Only hipsters and people who want fancy things use this horrid language to do anything.

20170920b - AngularJS, or rather JavaScript in general, is absolutely annoying to use since the version changes so often and the changes break any legacy code. There is no long term support and it forces unnecessary work on developers.  

20170921a - Should consider using org-mode for notes, I wonder if Github supports it? We shall see after this note! Maybe then I can fully enjoy the **beauty** that is emacs and embrace the operating system entirely. Praise the Stallman.

20170921b - I have decided to put the standalone web frontend on the back burner and get the ncurses based version off the ground. I have a little over two months left to get this finished and think it would be best to start the ncurses project now.

20171001 - Work on the project has been stalled due to testing but I have made progress on the data structure for storing the dump files. I have implemented two structs: one struct contains the text from one cycle of the dump file and the other contains an array of the structs along with the file type.

20171008 - I encountered a memory leak using the data structure I devised but I managed to resolve it with a bit of proper memory management and organizing where and when I free up the data allocations. I hope to get to processing the information sooner than later.

20171016 - I have reached a certain point to where I may be able to start with actually printing the information to the interface (of which I need to finish). It seems once I get there then I will be able to present this to Dr. Norris. I do regret taking so long to get something to her but my time has been taken up by so many other things.

20171017 - I got a call from my father today, he told me my coworker put in his resignation and his last day is next Friday. I cannot not graduate now, I must be there for dad.

20171023 - Officially began serious work on the frontend. Need to allocate more time now that HR has said I need to focus on school and not handling tickets. From what I can tell, I have been set back about a week from the loss of time with work so I will need to use every minute I can to catch up with my progress. Overall this is going to be a “It’s not about the laps in between, it’s about the last lap” kind of moment. Just need to keep going. Until it is done.

20171027 - I began the work on the ncurses interface again after two days lossed. Having to go back to square one really with learning about how to work with the libraries.

20171031 - So far I have got the basic interface but need to figure out how to create two windows that are side by side. Encountered an issue with trying to get all the information on the screen. Luckily I have the reference material from earlier I can use. 

## Todo

- handle only file types of idump and sdump
- create ncurses interface