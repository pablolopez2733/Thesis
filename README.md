# Thesis   

R  codes for undergrad thesis @ ITAM.       
Student: Pablo L. Landeros   
Advisor: Abdolnasser Sadeghkani   


***
### Scripts:   
* stat_login.py -> A python  web scraper that uses the scrapy package to retrieve data from stathead.com. It gets all the 5 man units and their stats for every team throughout the 2014-2015 season. Exports data to **items.csv**

* 00_DataDownload.R: Downloads NBA pbp data from the 2014-2015 NBA season and playoffs and manipulates it to get plus minus for every single lineup used by each NBA team. Outputs csvs for reading data without downloading each time. Outputs:
    *  individual_stats.csv -> Plus minus and seconds played for each player in the 2014-2015 season.
    *  grouped_lineup_stats.csv -> Gets the plus minus and time played for every 5 man unit in the season. Outputs same as stat_login.py.
    *  lineup_stats.csv -> Stats for every 5 man unit (without grouping). if filtered by team, it is a time series. 
    *  lineup_subs.csv -> A list of all the substitutions made in the 2014-2015 season with 
     **lineupBefore** and **lineupAfter** columns.


* 01_ReadData: Reads the data downloaded and wrangled from the 00 script.
* 02_TransitionMatrices.R: Declares the necessary functions for calculating each team´s lineup usage-PM matrix.
* 03_Regressions.R: Runs a ridge, lasso and elastic net regression for every NBA team. 




 
