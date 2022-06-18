#reading data
athlete_data <- read.csv("C:/Users/ndine/Downloads/athelete_events.csv")

#removing na
athlete_data <- athlete_data[!is.na(athlete_data$Medal),]

#assigning points
athlete_data$Points <- ifelse(athlete_data$Medal == 'Gold', 3, ifelse( athlete_data$Medal == 'Silver', 2, ifelse( athlete_data$Medal == 'Bronze',1,0)))

#filtering US Swimmers
USA_Swimming_Data <- subset(athlete_data, athlete_data$NOC =="USA" & athlete_data$Sport =="Swimming")
USA_Swimming_Data 


#Pre ww2 data
preWW <- subset(USA_Swimming_Data, USA_Swimming_Data$Year<1939)
preWW

#Post ww2 data
postWW <- subset(USA_Swimming_Data, USA_Swimming_Data$Year>1939)
postWW


pre_sum <- aggregate(preWW$Points, by=list(preWW$Name), FUN=sum)
length(pre_sum$x)

post_sum <- aggregate(postWW$Points, by=list(postWW$Name), FUN=sum)
length(post_sum$x)
install.packages("pwr")

#Calculating n1 and n2 values
power.t.test(d=0.5,power=0.8,sig.level=0.05,alternative = "two.sided")
#n1 = 63.76576  

power.t.test(d=0.8,power=0.8,sig.level=0.05,alternative = "two.sided")
#n2 = 25.52463  


#creating empty vectors
sample_mean_pre_m = rep(NA,30)
sample_mean_post_l = numeric()
sample_mean_pre_l = NULL
sample_mean_post_m = vector()


#vectors of sample means
for (i in 1:30){
  sample_mean_pre_m[i] = mean(rnorm(64,mean = mean(pre_sum$x), sd=sd(pre_sum$x)))
  #sample_mean_pre_m[i] = mean(rnorm(64,mean = mean(preWW$Points), sd=sd(preWW$Points)))
}
for (i in 1:30){
  #sample_mean_post_m[i] = mean(rnorm(64,mean = mean(postWW$Points), sd=sd(postWW$Points)))
  sample_mean_post_m[i] = mean(rnorm(64,mean = mean(post_sum$x), sd=sd(post_sum$x)))
}


for (i in 1:30){
  #sample_mean_pre_l[i] = mean(rnorm(26,mean = mean(preWW$Points), sd=sd(preWW$Points)))
  sample_mean_pre_l[i] = mean(rnorm(26,mean = mean(pre_sum$x), sd=sd(pre_sum$x)))

  }
for (i in 1:30){
  #sample_mean_post_l[i] = mean(rnorm(26,mean = mean(postWW$Points), sd=sd(postWW$Points)))
  sample_mean_post_l[i] = mean(rnorm(26,mean = mean(post_sum$x), sd=sd(post_sum$x)))
}

#plotting sample means
par(mfrow=c(2,2))
dens <- density(sample_mean_pre_m)
hist(sample_mean_pre_m, main = "Sample means for Pre WW2 with medium effect size",
     xlab = "Sample Means", col = "steelblue")
abline(v=mean(sample_mean_pre_m))
lines(dens)
hist(sample_mean_post_m, main = "Sample means for Post WW2 with medium effect size", 
     xlab = "Sample Means", col = "steelblue")
abline(v=mean(sample_mean_post_m))
hist(sample_mean_pre_l, main = "Sample means for Pre WW2 with large effect size", 
     xlab = "Sample Means", col = "steelblue")
abline(v=mean(sample_mean_pre_l))
hist(sample_mean_post_l, main = "Sample means for Post WW2 with large effect size",
     xlab = "Sample Means", col = "steelblue")
abline(v=mean(sample_mean_post_l))

#means of points and sample means
mean(preWW$Points)
mean(sample_mean_pre_m)
mean(sample_mean_pre_l)

mean(postWW$Points)
mean(sample_mean_post_m)
mean(sample_mean_post_l)

mean(pre_sum$x)
mean(post_sum$x)
length(pre_sum$Group.1))
length(post_sum$Group.1)
