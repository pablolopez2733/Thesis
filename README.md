# Thesis   

R  codes for undergrad thesis @ ITAM.       
Student: Pablo L. Landeros   
Advisor: Abdolnasser Sadeghkani   


***
### Scripts:   
* stat_login.py -> A python  web scraper that uses the scrapy package to retrieve data from stathead.com. It gets all the 5 man units and their stats for every team throughout the 2014-2015 season. Exports data to **items.csv**
* data_prep_1415 -> An R script for wrangling the 2014-2015 NBA play-by-play data to output 4 datasets:   

    1. individual_stats.csv -> Plus minus and seconds played for each player in the 2014-2015 season.
    2. grouped_lineup_stats.csv -> Gets the plus minus and time played for every 5 man unit in the season. Outputs same as stat_login.py.
    3. stats_by_lineup.csv -> Stats for every 5 man unit (without grouping).
    4. subs_1415.csv -> A list of all the substitutions made in the 2014-2015 season with 
     **lineupBefore** and **lineupAfter** columns.


