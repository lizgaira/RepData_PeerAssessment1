# Reproducible Research: Peer Assessment 1


First of all, we make all our code chunks visible with **opts_chunk$set(echo=TRUE)**



##1. Loading and preprocessing the data

1.1. Read the .csv file.Transform the variable *date* into Date R object.

```r
data <- read.delim("activity.csv", sep=",", dec=",", header=TRUE)
```

```
## Warning: no fue posible abrir el archivo 'activity.csv': No such file or
## directory
```

```
## Error: no se puede abrir la conexión
```

```r
data$date<-as.Date(data$date, "%Y-%M-%d")
```

```
## Error: do not know how to convert 'data$date' to class "Date"
```

1.2. Summarise data by day: total number of steps a  day, average number of steps taken per day.

```r
library(plyr)
data2<-ddply(data, .(date), summarise, 
             steps.day=sum(steps, na.rm=TRUE),
             mean.steps.day=mean(steps, na.rm=TRUE))
```

```
## Error: unique() puede ser aplicada solamente a vectores
```

1.3. Summarise data by interval: average number of steps taken per interval.

```r
library(plyr)
data3<-ddply(data, .(interval), summarise, 
             mean.steps.interval=mean(steps, na.rm=TRUE))
```

```
## Error: objeto 'interval' no encontrado
```



##2. What is mean total number of steps taken per day?

2.1. Histogram of the steps per day.

```r
hist(data2$steps.day, main="Number of steps taken each day", xlab="Steps per day",col="steelblue")
```

```
## Error: 'x' must be numeric
```

2.2. Mean and median of the data.

```r
matrix(c(mean(data2$steps.day),median(data2$steps.day)), nrow=1, ncol=2,
                dimnames=list(NULL,c("Mean", "Median")))
```

```
## Warning: argument is not numeric or logical: returning NA
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
```

```
##      Mean Median
## [1,]   NA     NA
```
As we can see, the average number of total steps per day in the two-month period is 18407, and the  median number of steps is 20525.



##3. What is the average daily activity pattern?
3.1. Series plot of the 5-minute interval and the average number of steps taken, averaged across all days.

```r
plot(data3$interval,data3$mean.steps.interval, type="l", main="Average number of steps by interval", xlab="Interval", ylab="Steps (mean)", col="steelblue", lwd=2)
```

```
## Error: objeto 'data3' no encontrado
```

```r
abline(v=data3[data3[2]==max(data3$mean.steps.interval)][1], col="black", lty=2, lwd=1.5)
```

```
## Error: objeto 'data3' no encontrado
```

3.2. Across all the days in the data set, the 5-minute interval that contains the maximum number of steps is the 835 interval.

```r
data3[data3[2]==max(data3$mean.steps.interval),]
```

```
## Error: objeto 'data3' no encontrado
```


##4. Imputing missing values

4.1. Total number of rows with NAs.

```r
dim(data[is.na(data$steps),])
```

```
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo
## 'NULL
```

```
## [1]  0 10
```
There are 2304 rows with missing values in the original data set.
To fill the missing data, we will replace NA values with the average number of steps of that interval in the two-month period.

```r
data$steps.filled<-data$steps
for (i in 1:nrow(data)){
    ifelse(is.na(data$steps.filled[i])==TRUE, 
           data$steps.filled[i]<-data3[data3$interval==data$interval[i],"mean.steps.interval"],
           data$steps.filled[i])
}
```

```
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
## Warning: is.na() aplicado a un objeto que no es (lista o vector) de tipo 'NULL
```

```r
newdata <- data[,2:4]
```

4.2. Histograms of the number of steps by day (with original and the new data set). 

```r
par(mfrow=c(1,2))
hist(data2$steps.day, main="Number of steps taken each day", xlab="Steps per day",col="steelblue")
```

```
## Error: 'x' must be numeric
```

```r
library(plyr)
newdata2<-ddply(newdata, .(date), summarize,
                new.steps.day=sum(steps.filled))
```

```
## Error: unique() puede ser aplicada solamente a vectores
```

```r
hist(newdata2$new.steps.day, main="Number of steps taken each day (new data set)", xlab="Steps per day",col="steelblue")
```

```
## Error: objeto 'newdata2' no encontrado
```

4.3. Mean and median of the data (with the new data set).

```r
matrix(c(mean(data2$steps.day), mean(newdata2$new.steps.day), 
         median(data2$steps.day), median(newdata2$new.steps.day)), nrow=2, ncol=2, 
       dimnames=list(c("original data", "filled data"),c("Mean", "Median")))
```

```
## Warning: argument is not numeric or logical: returning NA
```

```
## Error: objeto 'newdata2' no encontrado
```
The values have changed. The average number of steps a day in the two-month period with filled data set is now 21185.08, and the  median number of steps is 21641. The average and median values are now between 10180 and 33887 and all values are above 10000 steps a day, while  with the original data, many of the daily values were zero or below that value. Maybe now we have "inflated" the average daily number of steps... we should consider another ways of imputation of missing data.



##5. Are there differences in activity patterns between weekdays and weekends?


5.1. Create new factor variable in the new data set for weekday or weekend.

```r
library(plyr)
newdata$day<-as.factor(weekdays(newdata$date))
```

```
## Error: no applicable method for 'weekdays' applied to an object of class
## "NULL"
```

```r
newdata$type.of.day<-newdata$day
newdata$type.of.day<- as.factor(ifelse(newdata$day == "Saturday" | newdata$day == "Sunday", 
                             "weekend", "weekday"))
```

```
## Error: replacement has 0 rows, data has 465
```

```r
newdata3<-aggregate(steps.filled ~ interval + type.of.day , data = newdata, mean)
```

```
## Error: objeto 'steps.filled' no encontrado
```

5.2. Panel plot comparing average number of steps on weekend days vs weekdays.

```r
library(lattice)
xyplot(steps.filled~interval|type.of.day, data=newdata3, type="l", layout=c(1,2),
       xlab="Interval", ylab="Steps per day", main="Number of steps on weekend days vs weekdays")
```

```
## Error: objeto 'newdata3' no encontrado
```

Seems that patterns between weekend days and weekdays are not so different, with lower values till the interval 500 and above 2000, and higher number of steps between 500 and 2000. In fact, summary values are quite similar.

```r
matrix(rbind(summary(newdata$steps.filled[newdata$type.of.day=="weekend"]),
        summary(newdata$steps.filled[newdata$type.of.day=="weekday"])), nrow=2, ncol=6, 
       dimnames=list(c("original data", "filled data"),
                     c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")))
```

```
##               Min. 1st Qu. Median Mean 3rd Qu. Max   
## original data "0"  "NULL"  "NULL" "0"  "NULL"  "NULL"
## filled data   "0"  "NULL"  "NULL" "0"  "NULL"  "NULL"
```
