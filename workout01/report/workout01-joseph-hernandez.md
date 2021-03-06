Workout 1
================
Joseph Valentine Hernandez
March 13, 2019

The Greatest Team of All Time? The Impressive *"Two Long Three Short"* Strategy of the Dominant Golden State Warriors
---------------------------------------------------------------------------------------------------------------------

*Background & Introduction*
---------------------------

The Golden State Warriors have been the dominant National Basketball Association team in the Western Conference since 2015, when the Bay Area franchise became champions after adding head coach Steve Kerr. Since 2015, the Warriors have ascended to dynastic status, winning three of the last four NBA championships and positioning themselves to be a major threat in this year's playoff series.

To comprehend the warrior's dominance and get a sense of thier general strategy, one can look back on the Warrior's 2016 season data and apply basic APBRmetrics techniques (the basketball cousin of Sabermetrics) to quantify the impressive performances of Golden State's five key starters: Steph Curry, Andre Iguodala, Draymond Green, Kevin Durant, and Klay Thompson. Among them are several Kia MVPs and top draft picks with loyalties to California, to say nothing of San Diego native Coach Kerr. (See "Warriors Team History" in references)

Among the data used for this article on the 2016 Warriors starters' performances are the distances from the basket, locations on the court, minutes remaining in the quarter, minutes remaining in the game, player shooting, field goal type (two points or three), shot type (layup, three point, etc), opponent team, and date of the game for every attempted or successful shot. Using this vast and expanisve set of data, I have constructed shot charts for each player (Curry, Iguodala, Green, Durant, and Thompson) and tables representing the effective shooting percentages by player.

Using this data and my visualizations, I have drawn several major conclusions about the Golden State Warriors. First, the three point field goal dominance of Steph Curry was evident in the shooting percentages of the Warriors, but Klay Thompson actually leads Curry at 42.41% effective shooting to Curry's 40.75% effective shooting from beyond the three-point line. However, Curry had made more thirty-four more three point baskets overall than Thompson. These results concerning effective three point shooting reverberate in the shot charts for both Curry and Thompson, Golden State's strongest mid-range and long-range shooters. They each have been key to the strategy that has made GSW champions for four seasons in a row, by keeping strong shooters in the mid-court and powerful and effective two-point shooters up front in Green, Durant, and Iguodala. For two point field goals, Curry and Thompson actually lag behind Iguodala and Durant, with Iguodala and Durant both successfully making more than sixty percent of their baskets at 63.80% and 60.65% respectively. The shot charts confirm this pattern, and they show a strong mid-field game for Curry and Thompson and a strong game close to the basket for the taller Iguodala and Durant.

Overall, the Warriors deploy a *"two long three short"* strategy dependent on Curry and Thompson to dominate in three point attempts and the other players to block, layup, and make short range passes.

*Motivation & Purpose*
----------------------

This project was motivated by an especially inspirational statistics professor I had at UC Berkeley, and my topic of choice, the contrasts between the Warriors' short range and long range powerhouses, has been inspired by living in the Bay Area for four years and watching the NBA finals. The purpose of this report is to analyze the Warriors players' performances using data and basic techniques of data analysis, while presenting it in a web-article format.

*What Data Do You Have?*
------------------------

My data is detailed in the data dictionary found in my `data/` folder. It covers the units and details of each variable. As previosuly stated, the data is drawmn from five individual datasets covering the shooting characteristics of Andre Iguodala, Steph Curry, Kevin Durant, Draymond Green, and Klay Thompson from the 2016 NBA season. Notable characteristics that were used to create the shots charts and tables in this article include the spot on the basket where the player took a shot, the time remianing in the quarter when the shot was taken, the shot's type (layup, three-pointer, etc), and the points earned from the field goal (shot). Combining the data from each player, I made a larger dataset called "shots" that allowed me to compare player performances and effective shooting in the aggregate.

*Analysis of the 2016 GSW*
--------------------------

### Tables

The following tables cover the effective shooting performance of each of the five GSW starters, arranged in descending order. My results come from an analysis of shot charts and effective shooting percentages, disaggregated across field goal types.

#### Table: Overall Effective Shooting Percent

| Name           | Total Baskets | Baskets Made | Effective Shooting Percent |
|----------------|---------------|--------------|----------------------------|
| Kevin Durant   | 915           | 495          | 54.09                      |
| Andre Iguodala | 371           | 192          | 51.75                      |
| Klay Thompson  | 1220          | 575          | 47.13                      |
| Stephen Curry  | 1250          | 584          | 46.72                      |
| Draymond Green | 578           | 245          | 42.38                      |

The overall rankings place Durant on top as the most effective shooter on the Golden State Warriors team, which comes as unsurprising news to followers of basketball and devoted fans. He is followed by Iguodala, another tall player who does best sneaking in layups and close shots near the basket. Curry comes in as a surprising fourth, but his success shooting three-pointers may be associated with his lower ranking overall. Draymond Green finishes last in the overall ranking, but also seems to finsih last in both the two and three point categories, which suggests his skills are defensive rather than offensive.

#### Table: Two-Point FG Effective Shooting Percent

| Name           | Total Baskets | Baskets Made | Effective Shooting Percent |
|----------------|---------------|--------------|----------------------------|
| Andre Iguodala | 210           | 134          | 63.80                      |
| Kevin Durant   | 643           | 390          | 60.65                      |
| Stephen Curry  | 563           | 304          | 53.99                      |
| Klay Thompson  | 640           | 329          | 51.40                      |
| Draymond Green | 346           | 171          | 49.42                      |

#### Table: Three-Point FG Effective Shooting Percent

| Name           | Total Baskets | Baskets Made | Effective Shooting Percent |
|----------------|---------------|--------------|----------------------------|
| Klay Thompson  | 580           | 246          | 42.41                      |
| Stephen Curry  | 687           | 280          | 40.75                      |
| Kevin Durant   | 272           | 105          | 38.60                      |
| Andre Iguodala | 161           | 58           | 36.02                      |
| Draymond Green | 232           | 74           | 31.89                      |

As mentioned previously in the **Introduction**, disaggregating the results of the first table into two tables showing the effective shooting percents based on field goal type seems to show the hidden three point strengths of Steph Curry and Klay Thompson, while Durant and Iguodala run away with impressive effective shooting in the sixtieth percentile. Overall, the raw numbers show the impressive number of total baskets by Curry and Thompson, who often have the chance to shoot from near the three-point line while being protected by blockers and heavyweights in Durant, Green, and Iguodala.

*Results & Shot Charts*
-----------------------

As a result of these statistics, I have compiled shot charts that show the *"two long three short"* strategy of the Golden State Warriors in greater detail. The chart includes information on all five players, and I urge readers to focus on the differences between the short range players Iguodala, Durant, and Green and the long range players Curry and Thompson.

#### Shot Charts

![Shot charts overlayed onto a basketball halfcourt image for the Golden State Warriors, using data from the 2016 NBA season.](../images/gsw-shot-charts2.png)

Three main takeaways about the GSW starters are obvious from this image. First, Steph Curry and Klay Thomspon have especially saturated shot charts, especially beyond the three point line. Second, Draymond Green and Andre Iguodala have especially sparse charts, and those two, including Durant, as well seem to rarely shoot from beyond the three point line and play strong defensive games and have respectable numbers near the baskets. Third, as expected, most "shots made" come from close to the basket or from positions on a vertical "line"" stretching out from the basket down the center of the court. Notably, Klay Thompson, the player with the highest three point field goal effective shooting percentage has a seemingly strong preference for the left side of the basket when lobbing three point attempts. He seems to hang left, running towards the basket, before stopping to shoot or pass, and this may be in coordination with his teammates and represent a chance for him to pass the ball to Kevin Durant or Andre Iguodala down the center.

*Conclusion*
------------

The Golden State Warriors have a clear strategy that relies on their two dominant three point shooters and other players playing extensive, active roles in the inner court. Players like Green, Durant, and Iguodala post impressive figures for two-point attempts, but they are even more vital to the GSW strategy as blockers and passers who facilitate high scoring by Curry and Thompson. The shot charts show a well coordinated team with clear sub-objectives and planned positioning, which has made them sharply effective over the last four years in the NBA. Only the test of time will stop the Golden State strategy, and it would not be unprecedented for record numbers such as these to translate into additional successes in the years to come, as the team reigns supreme in the ever-difficult Western Conference. This fits with more recent analysis from Klopfer, Schuhmann, Buckley and others who follow the team and have commented on the teams impressive effective field goal percentage numbers (see references and tables for data).

#### *References*

Buckley, Zach. "5 Wild Stats Defining Golden State Warriors' Season," Bleach Report, 2017. Klopfer, Bradley. "Steph Curry is as efficient from 30 feet as he is from 3 feet" Golden State of Mind, 2018. Schuhmann, John. "One Team, One Stat -- Golden State Warriors' shooting the best in NBA history ... again" NBA.com, 2017.

"Warriors Team History." NBA Media Ventures, 2019.
