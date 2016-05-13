#load and pre process data
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date,"%Y-%m-%d")
activity <- activity %>%
  mutate(interval= (interval%%100+60*(interval%/%100)))



#calculate total numberof steps per day
library(dplyr)
daily_stats <- activity %>%
                group_by(date) %>%
                summarise(total = sum(steps, na.rm=TRUE))
mean_daily_total = mean(daily_stats$total, na.rm=T)
median_daily_total = median(daily_stats$total, na.rm=T)

with(daily_stats, hist(total, 
                       main="Total Steps per day", 
                       xlab="Steps per day", 
                       col="orange"))

library(ggplot2)
ggplot(data=daily_stats, aes(daily_stats$total)) +
  labs(x="Steps", y="Frequency") +
  ggtitle("Histogram of total steps per day") +      
  geom_histogram(bins=10, col="black", 
                 fill="black", 
                 alpha = .6) +
  geom_vline(xintercept=median_daily_total, col="white", linetype="dotted") + 
  annotate("text", label = paste("median =",round(median_daily_total,0)), 
           x = median_daily_total, y = 5, vjust=-0.5, size = 4, angle=90, 
           colour = "white") +
  geom_vline(xintercept=mean_daily_total, col="white", linetype="dotted") + 
  annotate("text", label = paste("mean =",round(mean_daily_total,0)), 
           x = mean_daily_total, y = 5, vjust=-0.5, size = 4, angle=90, 
           colour = "white")

# average 5 mininterval steps
daily_5min_stats <- activity %>%
  group_by(interval) %>%
  summarise(average = mean(steps, na.rm=TRUE))

plot(daily_5min_stats$interval, daily_5min_stats$average, type="l", xlab="Minutes since midnight", ylab="Average Steps in 5 min interval", main="Average Daily Activity Pattern")


#no. of rows with na
