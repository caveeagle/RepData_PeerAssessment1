
basedir <- file.path(getwd(),"Data","RepDataAssessment")

D <- read.csv( file = file.path(basedir,"activity.csv") )

########################################################################

steps_per_day <- tapply(D$steps, D$date, FUN=sum, na.rm=TRUE)

# средн€€ - это просто арифметическое среднее по всем элементам выборки
# медиана - это процентиль 50%, делит всю выборку на 2 части с равным числом элементов ("пополам")

mean(steps_per_day, na.rm=TRUE)

median(steps_per_day, na.rm=TRUE)

library(ggplot2)

qplot(steps_per_day, xlab='Number of steps per day' ,binwidth = 1000)

########################################################################

mean_per_interval <- aggregate(data = D, steps ~ interval, FUN=mean, na.rm=TRUE )

colnames(mean_per_interval)[2] <- "mean.steps"


ggplot(data=mean_per_interval, aes(x=interval, y=mean.steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("mean of steps")

max_interval <- mean_per_interval[which.max(mean_per_interval$mean.steps),1]

max_time <- format(max_interval/100, 2, format='f')

max_time <- gsub("\\.", ":", max_time)

print(max_time)

########################################################################

NAnum <- sum(is.na(D$steps))

print(NAnum)


# merge
D2 <- merge(D, mean_per_interval, by = "interval", sort= FALSE)

#replace NA values
D2$steps[is.na(D2$steps)] <- D2$mean.steps[is.na(D2$steps)]

########################################################################

steps_per_day_full <- tapply(D2$steps, D$date, FUN=sum, na.rm=TRUE)

mean(steps_per_day_full, na.rm=TRUE)

median(steps_per_day_full, na.rm=TRUE)

qplot(steps_per_day_full, xlab='Number of steps per day (full set)' ,binwidth = 1000)

########################################################################

# CHANGE LOCALE INTO ENGLISH
Sys.setlocale("LC_TIME","English United States")

# Calculate week days
dates <- strptime(D2$date, "%Y-%m-%d")
D2$weekday <- dates$wday

# ! SUNDAY is 0 !

# FOR CHECK: weekdays( as.POSIXct("2015-05-07"), abbreviate = FALSE )

D2$is.weekend <- "weekday" # by default
D2$is.weekend[D2$weekday==0|D2$weekday==6]  <- "weekend" 

########################################################################

mean_per_weekdays <- aggregate(steps ~ interval + is.weekend, data=D2, mean)

ggplot(mean_per_weekdays, aes(interval, steps, colour=is.weekend)) +
  geom_line(color="red")  + 
  facet_wrap(~is.weekend, ncol = 1) +
  theme(legend.position = 'none') + 
  theme(strip.background = element_rect(colour = "blue" ) )+ 
  theme(strip.text = element_text(size = rel(1.5))   )+ 
  xlab("Interval") +
  ylab("Number of steps") 

########################################################################
































