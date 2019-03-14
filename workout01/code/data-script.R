setwd("C:/Users/Joseph/Desktop/University_18_19/Classes/STAT133/workout01")

# Data Preparation
iguodala <- read.csv("C:/Users/Joseph/Desktop/University_18_19/Classes/STAT133/workout01/data/andre-iguodala.csv", 
                  stringsAsFactors = FALSE,header = T)
green <- read.csv("C:/Users/Joseph/Desktop/University_18_19/Classes/STAT133/workout01/data/draymond-green.csv", 
                  stringsAsFactors = FALSE,header = T)
durant <- read.csv("C:/Users/Joseph/Desktop/University_18_19/Classes/STAT133/workout01/data/kevin-durant.csv", 
                  stringsAsFactors = FALSE,header = T)
thompson <- read.csv("C:/Users/Joseph/Desktop/University_18_19/Classes/STAT133/workout01/data/klay-thompson.csv", 
                  stringsAsFactors = FALSE,header = T)
curry <- read.csv("C:/Users/Joseph/Desktop/University_18_19/Classes/STAT133/workout01/data/stephen-curry.csv", 
                  stringsAsFactors = FALSE,header = T)

# Adding variable: name
iguodala$name <- "Andre Iguodala"
green$name <- "Draymond Green"
durant$name <- "Kevin Durant"
thompson$name <- "Klay Thompson"
curry$name <- "Stephen Curry"

# Descriptive Shot change 
iguodala$shot_made_flag <- ifelse(iguodala$shot_made_flag=="n","shot_no","shot_yes")
durant$shot_made_flag <- ifelse(durant$shot_made_flag=="n","shot_no","shot_yes")
green$shot_made_flag <- ifelse(green$shot_made_flag=="n","shot_no","shot_yes")
curry$shot_made_flag <- ifelse(curry$shot_made_flag=="n","shot_no","shot_yes")
thompson$shot_made_flag <- ifelse(thompson$shot_made_flag=="n","shot_no","shot_yes")

# Minute 
#  Period = 12 minutes, 4 total, total minutes = 48, 
#   if period = 1, then minutes = 12
#   (period)*12 - minutes remaining = 
#     3(12) - 1 = 35 
curry$minute <- (curry$period*12)-curry$minutes_remaining
iguodala$minute <- (iguodala$period*12)-iguodala$minutes_remaining
green$minute <- (green$period*12)-green$minutes_remaining
durant$minute <- (durant$period*12)-durant$minutes_remaining
thompson$minute <- (thompson$period*12)-thompson$minutes_remaining

# Saving summaries to output as summary.txt files
  # Curry
sink(file = "./output/stephen-curry-summary.txt")
summary(curry)
sink()
  # Durant
sink(file = "./output/kevin-durant-summary.txt")
summary(durant)
sink()
  # Green
sink(file = "./output/draymond-green-summary.txt")
summary(green)
sink()
  # Iguodala
sink(file = "./output/andre-iguodala-summary.txt")
summary(iguodala)
sink()  
  # Thompson
sink(file = "./output/klay-thompson-summary.txt")
summary(thompson)
sink()


# rBinding the data.frame(s) veritcally:
  # Using rbind to make data frame "shots"
shots <- rbind.data.frame(curry, durant, green, iguodala, thompson)
  # Making it into a .csv 
write.csv(shots, file = "./data/shots-data.csv")
  # Saving the summary as a .txt file 
sink(file="./output/shots-data-summary.txt")
summary(shots)
sink()

#----------------------------------
# Effective Shooting Percentage

shots$made <- ifelse(shots$shot_made_flag=="shot_yes",1,0)
shots$shot <- ifelse(shots$shot_made_flag=="shot_yes"|shots$shot_made_flag=="shot_no",1,0)
shots$shot2 <- ifelse(shots$shot_type=="2PT Field Goal",1,0)
shots$made2 <- ifelse((shots$shot2==1)&shots$made==1,1,0)
shots$shot3 <- ifelse(shots$shot_type=="3PT Field Goal",1,0)
shots$made3 <- ifelse(shots$shot3==1&shots$shot_made_flag=="shot_yes",1,0)

# Tables 
ef_shot <- shots %>% 
  group_by(name) %>% 
  summarise(Total=sum(shot),
            Made=sum(made),
            Percent= 100*(sum(made) / sum(shot))
  ) %>% 
  arrange(desc(Percent))

ef_shot2 <- shots %>% 
  group_by(name) %>% 
  summarise(Total=sum(shot2),
            Made=sum(made2),
            Percent= 100*(sum(made2) / sum(shot2))
  ) %>% 
  arrange(desc(Percent))

ef_shot3 <- shots %>% 
  group_by(name) %>% 
  summarise(Total=sum(shot3),
            Made=sum(made3),
            Percent= 100*(sum(made3) / sum(shot3))
  ) %>% 
  arrange(desc(Percent))

