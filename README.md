# Thesis   

R  codes for undergrad thesis @ ITAM.       
Student: Pablo L. Landeros   
Advisor: Abdolnasser Sadeghkani   


***
### Scripts:   
* stat_login.py -> A python  web scraper that uses the scrapy package to retrieve data from stathead.com. It gets all the 5 man units and their stats for every team throughout the 2014-2015 season. Exports data to **items.csv**
* data_prep_1415.R -> An R script for wrangling the 2014-2015 NBA play-by-play data to output 4 datasets:   

    *  individual_stats.csv -> Plus minus and seconds played for each player in the 2014-2015 season.
    *  grouped_lineup_stats.csv -> Gets the plus minus and time played for every 5 man unit in the season. Outputs same as stat_login.py.
    *  lineup_stats.csv -> Stats for every 5 man unit (without grouping). if filtered by team, it is a time series. 
    *  lineup_subs.csv -> A list of all the substitutions made in the 2014-2015 season with 
     **lineupBefore** and **lineupAfter** columns.

* substitution_model.R -> Contains a function that takes a team as an argument and calculates its transitions as a matrix.

* linear_data_prep -> Prepare data for linear regression models.  
