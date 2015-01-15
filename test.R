stepsData <- read.csv("activity/activity.csv")
stepsData$date <- as.Date(stepsData$date, format = "%Y-%m-%d")
stepsDataNoNA <- filter(stepsData, !is.na(stepsData[,1]))
stepsByDay <- stepsDataNoNA %>% group_by(date) %>% summarise(sum(steps))
names(stepsByDay)[2] <- "dailySteps"
#stepsByDay2 <- stepsDataNoNA %>% group_by(date) %>% (sum(steps))
stepsByInterval <- stepsDataNoNA %>% group_by(interval) %>% summarise(mean(steps))
names(stepsByInterval)[2] <- "MeanSteps"


for (i in 1:nrow(stepsData)) {
      row <- stepsData[i,]
      if (is.na(row[1,1])) {
            stepsData[i, 1] <- (stepsByInterval[stepsByInterval$interval == row[1, 3],2])            
      }
}

