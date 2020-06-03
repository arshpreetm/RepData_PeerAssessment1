## Peer Review Assignment - Week 2 Arshpreet Makker

### Reading and processing the dataset.

```{r}
if (!file.exists('activity.csv')) {
  unzip(zipfile = "activity.zip")
}

activityData <- read.csv(file="activity.csv", header=TRUE)
```

### Calculate the total steps taken per day.

```{r}
totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)
```

### Make a histogram of the total number of steps taken per day.

```{r}
hist(totalSteps$steps,
     main = "Total Steps per Day",
     xlab = "Number of Steps")
```
### Mean and Median number of steps taken each day.

```{r}
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
medSteps <- median(totalSteps$steps, na.rm = TRUE)
```

Result:
  Mean = 10766.1886792453
  Median = 10765
  
### Time series plot of the average number of steps taken, and the 5-minute interval.

```{r}
library(ggplot2)
library(dplyr)
meanStepsByInt <- aggregate(steps ~ interval, activityData, mean)
ggplot(data = meanStepsByInt, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```
 
### Which 5-minute interval across all days contain the maximum number of steps?

```{r}
maxInt <- meanStepsByInt[which.max(meanStepsByInt$steps),]
```
  
Results:
Interval = 835 and No. of steps = 206.1698

### Imputing missing values.

```{r}
missingVals <- is.na(activityData$steps)
```

### Devise a strategy for filling in all of the missing values

There are 17568 missing values. I will replace these missing values with the 5-day average of that respective interval.


### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
imp_activityData <- transform(activityData,
                              steps = ifelse(is.na(activityData$steps),
                                             meanStepsByInt$steps[match(activityData$interval, 
                                                                        meanStepsByInt$interval)],
                                             activityData$steps))
```

### Make a histogram of the total number of steps taken each day with imputed values.

```{r}
impStepsByInt <- aggregate(steps ~ date, imp_activityData, FUN=sum)
hist(impStepsByInt$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")
```
### Create a new factor variable in the dataset with two levels - "weekend" and "weekday".

```{r}
DayType <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
      return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
      return ("weekend")
  else
      stop ("Invalid Date Format.")
}
imp_activityData$date <- as.Date(imp_activityData$date)
imp_activityData$day <- sapply(imp_activityData$date, FUN = DayType)
```

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

```{r}
meanStepsByDay <- aggregate(steps ~ interval + day, imp_activityData, mean)
ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```