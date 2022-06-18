
install.packages('olsrr')
library(olsrr)
install.packages('GGally')
library(GGally)


s1 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S01.csv")
s2 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S02.csv")
s3 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S03.csv")
s4 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S04.csv")
s5 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S05.csv")
s6 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S06.csv")
s7 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S07.csv")
s8 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S08.csv")
s10 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S10.csv")
s11 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S11.csv")
s12 <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S12.csv")

s1_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S01-Baseline.csv")
s2_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S02-Baseline.csv")
s3_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S03-Baseline.csv")
s4_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S04-Baseline.csv")
s5_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S05-Baseline.csv")
s6_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S06-Baseline.csv")
s7_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S07-Baseline.csv")
s8_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S08-Baseline.csv")
s10_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S10-Baseline.csv")
s11_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S11-Baseline.csv")
s12_b <- read.csv("C:/Users/ndine/OneDrive/Desktop/Data-HW7/S12-Baseline.csv")

library(tidyverse)
s1 <- s1 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s2 <- s2 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s3 <- s3 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s4 <- s4 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s5 <- s5 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s6 <- s6 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s7 <- s7 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s8 <- s8 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s10 <- s10 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s11 <- s11 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)
s12 <- s12 %>% drop_na(pp_nr5, Accelerator, Brake, Steering, Speed)

s1$SubjectID <- 1
s2$SubjectID <- 2
s3$SubjectID <- 3
s4$SubjectID <- 4
s5$SubjectID <- 5
s6$SubjectID <- 6
s7$SubjectID <- 7
s8$SubjectID <- 8
s10$SubjectID <- 10
s11$SubjectID <- 11
s12$SubjectID <- 12

s1$log_delta_pp <- 0
s2$log_delta_pp <- 0
s3$log_delta_pp <- 0
s4$log_delta_pp <- 0
s5$log_delta_pp <- 0
s6$log_delta_pp <- 0
s7$log_delta_pp <- 0
s8$log_delta_pp <- 0
s10$log_delta_pp <- 0
s11$log_delta_pp <- 0
s12$log_delta_pp <- 0

names(s1)
mean_pp2 <- mean(s1_b$pp_nr2)
s1$new_pp <- s1$pp_nr5 - mean_pp2

mean_pp2 <- mean(s2_b$pp_nr2)
s2$new_pp <- s2$pp_nr5 - mean_pp2

mean_pp2 <- mean(s3_b$pp_nr2)
s3$new_pp <- s3$pp_nr5 - mean_pp2

mean_pp2 <- mean(s4_b$pp_nr2)
s4$new_pp <- s4$pp_nr5 - mean_pp2

mean_pp2 <- mean(s5_b$pp_nr2)
s5$new_pp <- s5$pp_nr5 - mean_pp2

mean_pp2 <- mean(s6_b$pp_nr2)
s6$new_pp <- s6$pp_nr5 - mean_pp2

mean_pp2 <- mean(s7_b$pp_nr2)
s7$new_pp <- s7$pp_nr5 - mean_pp2

mean_pp2 <- mean(s8_b$pp_nr2)
s8$new_pp <- s8$pp_nr5 - mean_pp2

mean_pp2 <- mean(s10_b$pp_nr2)
s10$new_pp <- s10$pp_nr5 - mean_pp2

mean_pp2 <- mean(s11_b$pp_nr2)
s11$new_pp <- s11$pp_nr5 - mean_pp2

mean_pp2 <- mean(s12_b$pp_nr2)
s12$new_pp <- s12$pp_nr5 - mean_pp2


log_mean_pp2 <-mean(log(s1_b$pp_nr2)) 
s1$log_new_pp <- log(s1$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s2_b$pp_nr2)) 
s2$log_new_pp <- log(s2$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s3_b$pp_nr2)) 
s3$log_new_pp <- log(s3$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s4_b$pp_nr2)) 
s4$log_new_pp <- log(s4$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s5_b$pp_nr2)) 
s5$log_new_pp <- log(s5$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s6_b$pp_nr2)) 
s6$log_new_pp <- log(s6$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s1_b$pp_nr2)) 
s1$log_new_pp <- log(s1$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s7_b$pp_nr2)) 
s7$log_new_pp <- log(s7$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s1_b$pp_nr2)) 
s1$log_new_pp <- log(s1$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s8_b$pp_nr2)) 
s8$log_new_pp <- log(s8$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s10_b$pp_nr2)) 
s10$log_new_pp <- log(s10$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s11_b$pp_nr2)) 
s11$log_new_pp <- log(s11$pp_nr5) - log_mean_pp2

log_mean_pp2 <-mean(log(s12_b$pp_nr2)) 
s12$log_new_pp <- log(s12$pp_nr5) - log_mean_pp2


#mean and sd for speed
s1$mean_speed <- 0
a <- 1
for(x in 30:nrow(s1)) {
  s1$mean_speed[x] <- mean(s1$Speed[a:x])
  a <- a +1
}
s1$sd_speed <- 0
a <- 1
for(x in 30:nrow(s1)) {
  s1$sd_speed[x] <- sd(s1$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s1$mean_acc <- 0
a <- 1
for(x in 30:nrow(s1)) {
  s1$mean_acc[x] <- mean(s1$Accelerator[a:x])
  a <- a +1
}
s1$sd_acc <- 0
a <- 1
for(x in 30:nrow(s1)) {
  s1$sd_acc[x] <- sd(s1$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s1$mean_brake <- 0
a <- 1
for(x in 30:nrow(s1)) {
  s1$mean_brake[x] <- mean(s1$Brake[a:x])
  a <- a +1
}
s1$sd_brake <- 0
a <- 1
for(x in 30:nrow(s1)) {
  s1$sd_brake[x] <- sd(s1$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s1$mean_steering <- 0
a <- 1
for(x in 30:nrow(s1)) {
  s1$mean_steering[x] <- mean(s1$Steering[a:x])
  a <- a +1
}
s1$sd_steering <- 0
a <- 1
for(x in 30:nrow(s1)) {
  s1$sd_steering[x] <- sd(s1$Steering[a:x])
  a <- a +1
}


s1$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s1)-5)
for (x in 1:c) {
  
  s1$delta_pp[x] <- mean(s1$new_pp[a:b])
  s1$log_delta_pp[x] <- mean(s1$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}

#___________________

#mean and sd for speed
s2$mean_speed <- 0
a <- 1
for(x in 30:nrow(s2)) {
  s2$mean_speed[x] <- mean(s2$Speed[a:x])
  a <- a +1
}
s2$sd_speed <- 0
a <- 1
for(x in 30:nrow(s2)) {
  s2$sd_speed[x] <- sd(s2$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s2$mean_acc <- 0
a <- 1
for(x in 30:nrow(s2)) {
  s2$mean_acc[x] <- mean(s2$Accelerator[a:x])
  a <- a +1
}
s2$sd_acc <- 0
a <- 1
for(x in 30:nrow(s2)) {
  s2$sd_acc[x] <- sd(s2$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s2$mean_brake <- 0
a <- 1
for(x in 30:nrow(s2)) {
  s2$mean_brake[x] <- mean(s2$Brake[a:x])
  a <- a +1
}
s2$sd_brake <- 0
a <- 1
for(x in 30:nrow(s2)) {
  s2$sd_brake[x] <- sd(s2$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s2$mean_steering <- 0
a <- 1
for(x in 30:nrow(s2)) {
  s2$mean_steering[x] <- mean(s2$Steering[a:x])
  a <- a +1
}
s2$sd_steering <- 0
a <- 1
for(x in 30:nrow(s2)) {
  s2$sd_steering[x] <- sd(s2$Steering[a:x])
  a <- a +1
}


s2$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s2)-5)
for (x in 1:c) {
  
  s2$delta_pp[x] <- mean(s2$new_pp[a:b])
  
  s2$log_delta_pp[x] <- mean(s2$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}
#________________________

#mean and sd for speed
s3$mean_speed <- 0
a <- 1
for(x in 30:nrow(s3)) {
  s3$mean_speed[x] <- mean(s3$Speed[a:x])
  a <- a +1
}
s3$sd_speed <- 0
a <- 1
for(x in 30:nrow(s3)) {
  s3$sd_speed[x] <- sd(s3$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s3$mean_acc <- 0
a <- 1
for(x in 30:nrow(s3)) {
  s3$mean_acc[x] <- mean(s3$Accelerator[a:x])
  a <- a +1
}
s3$sd_acc <- 0
a <- 1
for(x in 30:nrow(s3)) {
  s3$sd_acc[x] <- sd(s3$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s3$mean_brake <- 0
a <- 1
for(x in 30:nrow(s3)) {
  s3$mean_brake[x] <- mean(s3$Brake[a:x])
  a <- a +1
}
s3$sd_brake <- 0
a <- 1
for(x in 30:nrow(s3)) {
  s3$sd_brake[x] <- sd(s3$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s3$mean_steering <- 0
a <- 1
for(x in 30:nrow(s3)) {
  s3$mean_steering[x] <- mean(s3$Steering[a:x])
  a <- a +1
}
s3$sd_steering <- 0
a <- 1
for(x in 30:nrow(s3)) {
  s3$sd_steering[x] <- sd(s3$Steering[a:x])
  a <- a +1
}


s3$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s3)-5)
for (x in 1:c) {
  
  s3$delta_pp[x] <- mean(s3$new_pp[a:b])
  s3$log_delta_pp[x] <- mean(s3$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}

#_______________________________________________________

#mean and sd for speed
s4$mean_speed <- 0
a <- 1
for(x in 30:nrow(s4)) {
  s4$mean_speed[x] <- mean(s4$Speed[a:x])
  a <- a +1
}
s4$sd_speed <- 0
a <- 1
for(x in 30:nrow(s4)) {
  s4$sd_speed[x] <- sd(s4$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s4$mean_acc <- 0
a <- 1
for(x in 30:nrow(s4)) {
  s4$mean_acc[x] <- mean(s4$Accelerator[a:x])
  a <- a +1
}
s4$sd_acc <- 0
a <- 1
for(x in 30:nrow(s4)) {
  s4$sd_acc[x] <- sd(s4$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s4$mean_brake <- 0
a <- 1
for(x in 30:nrow(s4)) {
  s4$mean_brake[x] <- mean(s4$Brake[a:x])
  a <- a +1
}
s4$sd_brake <- 0
a <- 1
for(x in 30:nrow(s4)) {
  s4$sd_brake[x] <- sd(s4$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s4$mean_steering <- 0
a <- 1
for(x in 30:nrow(s4)) {
  s4$mean_steering[x] <- mean(s4$Steering[a:x])
  a <- a +1
}
s4$sd_steering <- 0
a <- 1
for(x in 30:nrow(s4)) {
  s4$sd_steering[x] <- sd(s4$Steering[a:x])
  a <- a +1
}


s4$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s4)-5)
for (x in 1:c) {
  
  s4$delta_pp[x] <- mean(s4$new_pp[a:b])
  s4$log_delta_pp[x] <- mean(s4$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}

#-------------------------------------------------------

#mean and sd for speed
s5$mean_speed <- 0
a <- 1
for(x in 30:nrow(s5)) {
  s5$mean_speed[x] <- mean(s5$Speed[a:x])
  a <- a +1
}
s5$sd_speed <- 0
a <- 1
for(x in 30:nrow(s5)) {
  s5$sd_speed[x] <- sd(s5$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s5$mean_acc <- 0
a <- 1
for(x in 30:nrow(s5)) {
  s5$mean_acc[x] <- mean(s5$Accelerator[a:x])
  a <- a +1
}
s5$sd_acc <- 0
a <- 1
for(x in 30:nrow(s5)) {
  s5$sd_acc[x] <- sd(s5$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s5$mean_brake <- 0
a <- 1
for(x in 30:nrow(s5)) {
  s5$mean_brake[x] <- mean(s5$Brake[a:x])
  a <- a +1
}
s5$sd_brake <- 0
a <- 1
for(x in 30:nrow(s5)) {
  s5$sd_brake[x] <- sd(s5$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s5$mean_steering <- 0
a <- 1
for(x in 30:nrow(s5)) {
  s5$mean_steering[x] <- mean(s5$Steering[a:x])
  a <- a +1
}
s5$sd_steering <- 0
a <- 1
for(x in 30:nrow(s5)) {
  s5$sd_steering[x] <- sd(s5$Steering[a:x])
  a <- a +1
}


s5$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s5)-5)
for (x in 1:c) {
  
  s5$delta_pp[x] <- mean(s5$new_pp[a:b])
  s5$log_delta_pp[x] <- mean(s5$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}

#-----------------------------------------

#mean and sd for speed
s6$mean_speed <- 0
a <- 1
for(x in 30:nrow(s6)) {
  s6$mean_speed[x] <- mean(s6$Speed[a:x])
  a <- a +1
}
s6$sd_speed <- 0
a <- 1
for(x in 30:nrow(s6)) {
  s6$sd_speed[x] <- sd(s6$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s6$mean_acc <- 0
a <- 1
for(x in 30:nrow(s6)) {
  s6$mean_acc[x] <- mean(s6$Accelerator[a:x])
  a <- a +1
}
s6$sd_acc <- 0
a <- 1
for(x in 30:nrow(s6)) {
  s6$sd_acc[x] <- sd(s6$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s6$mean_brake <- 0
a <- 1
for(x in 30:nrow(s6)) {
  s6$mean_brake[x] <- mean(s6$Brake[a:x])
  a <- a +1
}
s6$sd_brake <- 0
a <- 1
for(x in 30:nrow(s6)) {
  s6$sd_brake[x] <- sd(s6$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s6$mean_steering <- 0
a <- 1
for(x in 30:nrow(s6)) {
  s6$mean_steering[x] <- mean(s6$Steering[a:x])
  a <- a +1
}
s6$sd_steering <- 0
a <- 1
for(x in 30:nrow(s6)) {
  s6$sd_steering[x] <- sd(s6$Steering[a:x])
  a <- a +1
}


s6$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s6)-5)
for (x in 1:c) {
  
  s6$delta_pp[x] <- mean(s6$new_pp[a:b])
  s6$log_delta_pp[x] <- mean(s6$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}

#--------------------------------------------------

#mean and sd for speed
s7$mean_speed <- 0
a <- 1
for(x in 30:nrow(s7)) {
  s7$mean_speed[x] <- mean(s7$Speed[a:x])
  a <- a +1
}
s7$sd_speed <- 0
a <- 1
for(x in 30:nrow(s7)) {
  s7$sd_speed[x] <- sd(s7$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s7$mean_acc <- 0
a <- 1
for(x in 30:nrow(s7)) {
  s7$mean_acc[x] <- mean(s7$Accelerator[a:x])
  a <- a +1
}
s7$sd_acc <- 0
a <- 1
for(x in 30:nrow(s7)) {
  s7$sd_acc[x] <- sd(s7$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s7$mean_brake <- 0
a <- 1
for(x in 30:nrow(s7)) {
  s7$mean_brake[x] <- mean(s7$Brake[a:x])
  a <- a +1
}
s7$sd_brake <- 0
a <- 1
for(x in 30:nrow(s7)) {
  s7$sd_brake[x] <- sd(s7$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s7$mean_steering <- 0
a <- 1
for(x in 30:nrow(s7)) {
  s7$mean_steering[x] <- mean(s7$Steering[a:x])
  a <- a +1
}
s7$sd_steering <- 0
a <- 1
for(x in 30:nrow(s7)) {
  s7$sd_steering[x] <- sd(s7$Steering[a:x])
  a <- a +1
}


s7$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s7)-5)
for (x in 1:c) {
  
  s7$delta_pp[x] <- mean(s7$new_pp[a:b])
  s7$log_delta_pp[x] <- mean(s7$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}


#----------------------------------------------

#mean and sd for speed
s8$mean_speed <- 0
a <- 1
for(x in 30:nrow(s8)) {
  s8$mean_speed[x] <- mean(s8$Speed[a:x])
  a <- a +1
}
s8$sd_speed <- 0
a <- 1
for(x in 30:nrow(s8)) {
  s8$sd_speed[x] <- sd(s8$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s8$mean_acc <- 0
a <- 1
for(x in 30:nrow(s8)) {
  s8$mean_acc[x] <- mean(s8$Accelerator[a:x])
  a <- a +1
}
s8$sd_acc <- 0
a <- 1
for(x in 30:nrow(s8)) {
  s8$sd_acc[x] <- sd(s8$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s8$mean_brake <- 0
a <- 1
for(x in 30:nrow(s8)) {
  s8$mean_brake[x] <- mean(s8$Brake[a:x])
  a <- a +1
}
s8$sd_brake <- 0
a <- 1
for(x in 30:nrow(s8)) {
  s8$sd_brake[x] <- sd(s8$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s8$mean_steering <- 0
a <- 1
for(x in 30:nrow(s8)) {
  s8$mean_steering[x] <- mean(s8$Steering[a:x])
  a <- a +1
}
s8$sd_steering <- 0
a <- 1
for(x in 30:nrow(s8)) {
  s8$sd_steering[x] <- sd(s8$Steering[a:x])
  a <- a +1
}


s8$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s8)-5)
for (x in 1:c) {
  
  s8$delta_pp[x] <- mean(s8$new_pp[a:b])
  s8$log_delta_pp[x] <- mean(s8$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}

#----------------------------------

#mean and sd for speed
s10$mean_speed <- 0
a <- 1
for(x in 30:nrow(s10)) {
  s10$mean_speed[x] <- mean(s10$Speed[a:x])
  a <- a +1
}
s10$sd_speed <- 0
a <- 1
for(x in 30:nrow(s10)) {
  s10$sd_speed[x] <- sd(s10$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s10$mean_acc <- 0
a <- 1
for(x in 30:nrow(s10)) {
  s10$mean_acc[x] <- mean(s10$Accelerator[a:x])
  a <- a +1
}
s10$sd_acc <- 0
a <- 1
for(x in 30:nrow(s10)) {
  s10$sd_acc[x] <- sd(s10$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s10$mean_brake <- 0
a <- 1
for(x in 30:nrow(s10)) {
  s10$mean_brake[x] <- mean(s10$Brake[a:x])
  a <- a +1
}
s10$sd_brake <- 0
a <- 1
for(x in 30:nrow(s10)) {
  s10$sd_brake[x] <- sd(s10$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s10$mean_steering <- 0
a <- 1
for(x in 30:nrow(s10)) {
  s10$mean_steering[x] <- mean(s10$Steering[a:x])
  a <- a +1
}
s10$sd_steering <- 0
a <- 1
for(x in 30:nrow(s10)) {
  s10$sd_steering[x] <- sd(s10$Steering[a:x])
  a <- a +1
}


s10$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s10)-5)
for (x in 1:c) {
  
  s10$delta_pp[x] <- mean(s10$new_pp[a:b])
  s10$log_delta_pp[x] <- mean(s10$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}


#-----------------------------

#mean and sd for speed
s11$mean_speed <- 0
a <- 1
for(x in 30:nrow(s11)) {
  s11$mean_speed[x] <- mean(s11$Speed[a:x])
  a <- a +1
}
s11$sd_speed <- 0
a <- 1
for(x in 30:nrow(s11)) {
  s11$sd_speed[x] <- sd(s11$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s11$mean_acc <- 0
a <- 1
for(x in 30:nrow(s11)) {
  s11$mean_acc[x] <- mean(s11$Accelerator[a:x])
  a <- a +1
}
s11$sd_acc <- 0
a <- 1
for(x in 30:nrow(s11)) {
  s11$sd_acc[x] <- sd(s11$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s11$mean_brake <- 0
a <- 1
for(x in 30:nrow(s11)) {
  s11$mean_brake[x] <- mean(s11$Brake[a:x])
  a <- a +1
}
s11$sd_brake <- 0
a <- 1
for(x in 30:nrow(s11)) {
  s11$sd_brake[x] <- sd(s11$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s11$mean_steering <- 0
a <- 1
for(x in 30:nrow(s11)) {
  s11$mean_steering[x] <- mean(s11$Steering[a:x])
  a <- a +1
}
s11$sd_steering <- 0
a <- 1
for(x in 30:nrow(s11)) {
  s11$sd_steering[x] <- sd(s11$Steering[a:x])
  a <- a +1
}


s11$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s11)-5)
for (x in 1:c) {
  
  s11$delta_pp[x] <- mean(s11$new_pp[a:b])
  s11$log_delta_pp[x] <- mean(s11$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}

#-----------------------------------------

#mean and sd for speed
s12$mean_speed <- 0
a <- 1
for(x in 30:nrow(s12)) {
  s12$mean_speed[x] <- mean(s12$Speed[a:x])
  a <- a +1
}
s12$sd_speed <- 0
a <- 1
for(x in 30:nrow(s12)) {
  s12$sd_speed[x] <- sd(s12$Speed[a:x])
  a <- a +1
}


#mean and sd for accelerator
s12$mean_acc <- 0
a <- 1
for(x in 30:nrow(s12)) {
  s12$mean_acc[x] <- mean(s12$Accelerator[a:x])
  a <- a +1
}
s12$sd_acc <- 0
a <- 1
for(x in 30:nrow(s12)) {
  s12$sd_acc[x] <- sd(s12$Accelerator[a:x])
  a <- a +1
}


#mean and sd for brake
s12$mean_brake <- 0
a <- 1
for(x in 30:nrow(s12)) {
  s12$mean_brake[x] <- mean(s12$Brake[a:x])
  a <- a +1
}
s12$sd_brake <- 0
a <- 1
for(x in 30:nrow(s12)) {
  s12$sd_brake[x] <- sd(s12$Brake[a:x])
  a <- a +1
}


#mean and sd for steering
s12$mean_steering <- 0
a <- 1
for(x in 30:nrow(s12)) {
  s12$mean_steering[x] <- mean(s12$Steering[a:x])
  a <- a +1
}
s12$sd_steering <- 0
a <- 1
for(x in 30:nrow(s12)) {
  s12$sd_steering[x] <- sd(s12$Steering[a:x])
  a <- a +1
}


s12$delta_pp <- 0
a <- 2
b <- 6
c <- (nrow(s12)-5)
for (x in 1:c) {
  
  s12$delta_pp[x] <- mean(s12$new_pp[a:b])
  s12$log_delta_pp[x] <- mean(s12$log_new_pp[a:b])
  a <- a+1
  b <- b+1
}

s1 <- s1[-c(1:29),]
s1 <- head(s1, -5)

s2 <- s2[-c(1:29),]
s2 <- head(s2, -5)

s3 <- s3[-c(1:29),]
s3 <- head(s3, -5)

s4 <- s4[-c(1:29),]
s4 <- head(s4, -5)

s5 <- s5[-c(1:29),]
s5 <- head(s5, -5)

s6 <- s6[-c(1:29),]
s6 <- head(s6, -5)

s7 <- s7[-c(1:29),]
s7 <- head(s7, -5)

s8 <- s8[-c(1:29),]
s8 <- head(s8, -5)

s10 <- s10[-c(1:29),]
s10 <- head(s10, -5)

s11 <- s11[-c(1:29),]
s11 <- head(s11, -5)

s12 <- s12[-c(1:29),]
s12 <- head(s12, -5)

final <- rbind(s1,s2,s3,s4,s5,s6,s7,s8,s10,s11,s12)



model <- lm(log_delta_pp ~ mean_speed + sd_speed + mean_acc + sd_acc + mean_brake + sd_brake + mean_steering + sd_steering + (1|SubjectID) , data = final)
vif<- ols_vif_tol(model)
max(vif$VIF)
summary(model)
head(s1)
ggpairs(input)

input <- final[,c("log_delta_pp","mean_speed","sd_speed","mean_acc", "sd_acc", "mean_brake", "sd_brake", "mean_steering", "sd_steering")]
plot(input, pch=16, col="blue",
     main="Matrix Scatterplot")

library(olsrr)
ols_step_forward_aic(model, details = TRUE)

ols_step_backward_aic(model, details = TRUE)

ols_step_both_aic(model, details = TRUE)

model2 <- lm(log_delta_pp ~ mean_speed + sd_speed + mean_acc + sd_acc + sd_brake + mean_steering + sd_steering + (1|SubjectID) , data = final)
ols_vif_tol(model2)
ols_step_forward_aic(model2, details = TRUE)
ols_step_backward_aic(model2, details = TRUE)
ols_step_both_aic(model2, details = TRUE)
