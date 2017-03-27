if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}

data <- read.csv("activity.csv")

require(dplyr)
require(ggplot2)
require(lubridate)

data$date <- ymd(data$date)


stepsByDay <- aggregate(steps ~ date, data, sum)
hist(stepsByDay$steps, main = "Total Steps Each Day", col="blue", xlab="Number of Steps")
step_mean <- mean(stepsByDay$steps)
step_median <- median(stepsByDay$steps)



stepsByInterval <- aggregate(steps ~ interval, data, mean)
plot(stepsByInterval$interval,stepsByInterval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
maxInterval <- stepsByInterval[which.max(stepsByInterval$steps),1]


sum(is.na(data$steps))

data.filled <- data
dataIndex <- is.na(data.filled$steps)
int_avg <- tapply(data.filled$steps, data.filled$interval, mean, na.rm=TRUE)


data.filled$steps[dataIndex] <- int_avg[as.character(data.filled$interval[dataIndex])]

data.temp <- group_by(data.filled, date)
data.summarize <- summarize(data.temp
                            , sumSteps = sum(steps, na.rm = TRUE)
                            , meanSteps = mean(steps, na.rm =TRUE))


g <- ggplot(data.summarize, aes(x = sumSteps)) +
        geom_histogram(aes(fill = ..count..), colour = "grey4", binwidth = 1000) + 
        labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")

print(g)


step_mean_filled <- mean(data.summarize$sumSteps)
step_median_filled <- median(data.summarize$sumSteps)


mean_diff <- step_mean_filled - step_mean
med_diff <- step_median_filled - step_median


total_diff <- sum(data.summarize$sumSteps) - sum(stepsByDay$sumSteps)


data.filled <- mutate(data.filled, daytype = ifelse(weekdays(data.filled$date, abbreviate = FALSE) == "Saturday" | weekdays(data.filled$date, abbreviate = FALSE) == "Sunday", "weekend", "weekday"))
data.filled$daytype <- as.factor(data.filled$daytype)
head(data.filled)

data.temp2 <- group_by(data.filled, interval, daytype)
data.summarize2 <- summarize(data.temp2
                             , sumSteps = sum(steps, na.rm = TRUE)
                             , meanSteps = mean(steps, na.rm =TRUE))
head(data.summarize2)

g <- ggplot(data.summarize2, aes(x=interval, y=meanSteps, color = daytype)) +
        geom_line() +
        facet_grid(. ~ daytype)

print(g)

