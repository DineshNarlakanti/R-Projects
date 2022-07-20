#importing data 
athlete_data <- read.csv("C:/Users/ndine/Downloads/athelete_events.csv")

#omitting NAs
athlete_data <- athlete_data[!is.na(athlete_data$Medal),]

#Assigning points based on the medals earned
athlete_data$Points <- ifelse(athlete_data$Medal == 'Gold', 3, ifelse( athlete_data$Medal == 'Silver', 2, ifelse( athlete_data$Medal == 'Bronze',1,0)))

#Filtering USA team and Swimming sport
USA_Swimming_Data <- subset(athlete_data, athlete_data$NOC =="USA" & athlete_data$Sport =="Swimming")

#Filtering France team and Fencing sport
France_Fencing_data <- subset(athlete_data, athlete_data$NOC =="FRA" & athlete_data$Sport =="Fencing")

# Calculating sum of points for USA Swimmers
points_sum_usa <- aggregate(USA_Swimming_Data$Points, by=list(USA_Swimming_Data$Name), FUN=sum)

#Calculating sum of points for France Fencers
points_sum_france <- aggregate(France_Fencing_data$Points, by=list(France_Fencing_data$Name), FUN=sum)

#Plotting PDFs and CDFs
plot(density(points_sum_usa$x), main= "Probability Density Function - USA Swimmers")

plot(ecdf(points_sum_usa$x), main= "Cumulative Distributive Function - USA Swimmers")

plot(density(points_sum_france$x), main= "Probability Ddensity Function - France Fencers")

plot(ecdf(points_sum_france$x), main= "Cumulative Distributive Function - France Fencers")

#Summary
summary(points_sum_usa)
summary(points_sum_france)

#Calculating modes for verifying the observation
v <- points_sum_usa$x
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(v)

v <- points_sum_france$x
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(v)

#Verifying unique score for the observation
unique(points_sum_usa$x)
sort(unique(points_sum_usa$x))

#Determining the distribution for the data
install.packages("fitdistrplus")
library(fitdistrplus)
descdist(points_sum_usa$x, discrete = FALSE)
descdist(points_sum_france$x, discrete = FALSE)