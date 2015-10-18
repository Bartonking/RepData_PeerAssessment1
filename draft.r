setwd("C:/Projects/ReproducableResults/Project1/Datascience/RepData_PeerAssessment1")\

##load data
unzip("activity.zip")
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
library(plyr) 
library(dplyr)

## **Mean ** Total number of steps taken per day

 
grp <- group_by(data,date) %>%
  summarize( total = sum(steps, na.rm=TRUE), nosteps = mean(steps,na.rm=TRUE) )

grp

## Make a histogram of the number of steps taken each day

library(ggplot2)
qplot(grp$total, geom="histogram",main="Total Number of Steps Per Day")


## Calculate the mean and the median 
This is a calcuation on the number of steps taken per day

mean_num_of_steps <- mean(x = grp$total, na.rm = TRUE)
median_num_of_steps <- median(x= grp$total, na.rm = TRUE)

We find that the mean is 'r mean_num_of_steps'     while the the median is found to be 'r median_num_of_steps'

head(data, 300)






#Question 2
## section 1
tm <- group_by(data,interval) %>%
  summarize(average = mean(steps,na.rm = TRUE), countsteps = sum(steps,na.rm=TRUE))
tm

plot.ts(x = tm$interval, y=tm$average,type = "l", xlab = "5 min intervals", ylab = "Average across all days", main="Average daily activity pattern")
## section 2
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
tm[which.max(tm$average),][[1]]


## Total nas
countNA <- sum(is.na(data$steps))


data$interval <- as.factor(data$interval)


fn_mean <- function(x){
  replace(x, is.na(x), mean(x,na.rm=TRUE))
}
newdata <- ddply(data, .(interval), transform,steps = fn_mean(steps))
 
newdata$interval <- as.character(newdata$interval)
newhistogram <- group_by(newdata,date) %>%
             summarise(total = sum(steps) )

#Historgram
qplot(newhistogram$total, geom="histogram",main="Total Number of Steps Per Day")

newmean <- mean(newhistogram$total)
newmedian <- median(newhistogram$total)





## Are there differences in activity patterns between weekdays and weekends?
install.packages("chron")
library(chron)
newhistogram$date <- as.character.Date(newhistogram$date)
#chron::is.weekend(newhistogram$date)

 
 
newdata$interval <- as.integer(newdata$interval);
wkdays <-  newdata %>% transform(daytype =
            factor(chron::is.weekend(date),levels=c(TRUE,FALSE), 
                   labels=c("Weekend","weekday")))  %>%
            arrange(interval,date)  %>%
            ddply( c("date","interval","daytype"), summarise, arvsteps = mean(steps))
  
library(lattice)
wkdays$arvsteps <- as.integer(wkdays$arvsteps);


xyplot(arvsteps ~ interval | daytype, data=wkdays, grid = TRUE,  layout = c(1,2),  xlab = "Intervals", ylab = "No of steps"  )


