#Workout 1: Data Dictionary 
Below are details on the variables found in the data from: **andre-igoudala.csv, draymond-green.csv, kevin-durant.csv, klay-thompson.csv, and steph-curry.csv**

##Structure of files:

1. andre-igoudala.csv 
* 371 observations
* 15 variables
2. draymond-green.csv
* 578 observations
* 15 variables
3. kevin-durant.csv
* 915 observations
* 15 variables
4. klay-thompson.csv 
* 1220 observations
* 15 variables
5. steph-curry.csv
* 1250 observations
* 15 variables

##variables: 
1. team_name
* type: character
* represents the name of the team a player plays for, in this case all the data will be from "GSW" or the Golden State Warriors
2. game_date
* type: character
* represents the calender date of the shot taken by a given player
3. season
* type: integer
* will be "2016" for all observations, data is from the 2016 season only
4. period
* type: integer
* period of the game that a shot was taken in, there are 4 periods per game, so values range from 1 to 4
5. minutes_remaining
* type: integer
* the minutes component of the time remaining in a quarter (period), measured in minutes, when a shot is taken 
6. seconds_remaining
* type: integer
* the seconds component of the time remaining in a quarter (period), measured in seconds, when a shot is taken 
7. shot_made_flag
* type: character
* levels: shot_yes, shot_no
* indicates if the shot was successful or not
8. action type
* type: character
* description of the action or type of shot that was performed, can be "cutting dunk shot" "layup" or other types of baskets
9. shot_type
* type: character
* indicates whether a shot is either a "2PT Field Goal" or "3PT Field Goal"
10. shot_distance
* type: integer
* distance from the basket a player is when they make a shot, measured in feet (assumedly)
11. opponent
* type: character
* string indicating the name of the opponent team 
12. x
* type: integer
* the x coordinate of the location where the shot was taken on the court, used for mapping
13. y
* type: integer
* the y coordinate of the location where the shot was taken on the court, used for mapping
14. name
* type: character
* the name of the player shooting the basket, one of the GSW starters
15. minute
* type: integer
* this variable was created using period and minute
* indicates the time remaining in a game when a player takes his shot, measured in minutes


