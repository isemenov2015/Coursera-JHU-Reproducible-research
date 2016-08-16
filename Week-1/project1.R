#Script for Week 1 project on JHU/Coursera Reproducible research
#Needs 'downloader' and 'dplyr' packages from CRAN to be installed

getsets <- function(furl = '') {
    # downloads .zip file from location specified in furl,
    # unzips data and removes downloaded archive from disk
    library(downloader)
    zipfile <- 'dfile.zip'
    download(fileurl, dest = zipfile, mode = 'wb')
    unzip(zipfile)
    unlink(zipfile)
}

fileurl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
#debug Uncomment function call before submitting
#getsets(fileurl)
#end debug

activity <- read.csv('activity.csv', stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date)

library(dplyr)
activity.d<- activity %>%
    group_by(date) %>%
    summarize(totsteps=sum(steps, na.rm=TRUE))
hist(activity.d$totsteps, main = 'No of steps per day histogram', 
     xlab = 'Steps per day', breaks = 20)
mean(activity.d$totsteps, na.rm = TRUE)
median(activity.d$totsteps, na.rm = TRUE)

stepints <- tapply(activity$steps, activity$interval, FUN = mean, na.rm = TRUE)
intervals <- unique(activity$interval)
plot(intervals, stepints, type = 'l', 
     main = 'Averaged No of steps acrosss day intervals', 
     xlab  = '5-minute intervals', ylab = 'Averaged No of steps')
intervals[which.max(stepints)]

sum(is.na(activity))
sum(is.na(activity$steps))

activity.noNA<- activity %>%
    group_by(interval)  %>%
    mutate(steps= ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
activity.noNA<- activity.noNA %>%
    group_by(date) %>%
    summarize(totsteps=sum(steps, na.rm=TRUE))
hist(activity.noNA$totsteps, main = 'No of steps per day histogram with NA removed', 
     xlab = 'Steps per day', breaks = 20, col = 'gray')
mean(activity.noNA$totsteps)
median(activity.noNA$totsteps)

activity$day <- weekdays(activity$date)
wdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity$wday <- factor((weekdays(activity$date) %in% wdays), 
                        levels = c(FALSE, TRUE), labels = c('weekend', 'weekday'))
activity.dtype <- activity %>% 
                    group_by(wday, interval) %>% 
                        summarize(totsteps = sum(steps, na.rm = TRUE), 
                                  avsteps = mean(steps, na.rm = TRUE))
library(lattice)
xyplot(avsteps~interval|wday, 
       data=activity.dtype, type='l', layout=(c(1,2)), 
       main="Average Daily Activity by weekdays/weekends", 
       ylab="Average number of steps per interval", 
       xlab="5-minute time interval No")