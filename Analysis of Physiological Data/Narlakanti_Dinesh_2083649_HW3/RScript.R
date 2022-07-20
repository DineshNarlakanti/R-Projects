psy_data <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Narlakanti_Dinesh_2083649_HW3/Physiological Data.csv")

nrow(psy_data)

data_rb <- subset(psy_data, psy_data$Treatment=="RB")


chest_HR <- aggregate(data_rb$Chest_HR_QC, by = list(data_rb$Participant_ID), FUN = mean)

wrist_HR <- aggregate(data_rb$Wrist_HR_QC, by = list(data_rb$Participant_ID), FUN = mean)

br_rb <- aggregate(data_rb$BR_QC, by = list(data_rb$Participant_ID), FUN = mean)

ch <- chest_HR[!is.na(chest_HR$x),]
wh <- wrist_HR[!is.na(wrist_HR$x),]
br <- br_rb[!is.na(br_rb$x),]

par(mfrow=c(2,3))

boxplot(chest_HR$x, main = "RB session Chest HR", xlab = 'Heart Rate', col = "steelblue", horizontal = TRUE)
abline(v= mean(ch$x), col = 'red')
abline(v=75, col = 'green')

boxplot(wrist_HR$x, main = "RB session Wrist HR", xlab = 'Heart Rate', col = "steelblue", horizontal = TRUE)
abline(v= mean(wh$x), col = 'red')
abline(v=75, col = 'green')

boxplot(br_rb$x, main = "RB session Breath Rate", xlab = 'Breathe Rate', col = "steelblue", horizontal = TRUE)
abline(v= mean(br$x), col = 'red')
abline(v=14, col = 'green')

#chest_HR
t.test(chest_HR$x, alternative = "two.sided", mu= 75)


#WristHR
t.test(wrist_HR$x, alternative = "two.sided", mu= 75)

#Breath rate
t.test(br_rb$x, alternative = "two.sided", mu = 14)

#For PR

data_pr <- subset(psy_data, psy_data$Treatment=="PR")

chest_pr <- aggregate(data_pr$Chest_HR_QC, by = list(data_pr$Participant_ID), FUN = mean)

wrist_pr <- aggregate(data_pr$Wrist_HR_QC, by = list(data_pr$Participant_ID), FUN = mean)

br_pr <- aggregate(data_pr$BR_QC, by = list(data_pr$Participant_ID), FUN = mean)


ch_pr <- chest_pr[!is.na(chest_pr$x),]
wh_pr <- wrist_pr[!is.na(wrist_pr$x),]
brpr <- br_pr[!is.na(br_pr$x),]


boxplot(chest_pr$x, main = "PR session Chest HR", xlab = 'Heart Rate', col = "steelblue", horizontal = TRUE)
abline(v= mean(ch_pr$x), col = 'red')
abline(v=85)

boxplot(wrist_pr$x, main = "PR session Wrist HR", xlab = 'Heart Rate', col = "steelblue", horizontal = TRUE)
abline(v= mean(wh_pr$x), col = 'red')
abline(v=85)

boxplot(br_pr$x, main = "PR session Breath Rate", xlab = 'Breathe Rate', col = "steelblue", horizontal = TRUE)
abline(v= mean(brpr$x), col = 'red')
abline(v=16)

#presentation Chest HR
t.test(chest_pr$x, alternative = "greater", mu= 85)

#presentation Wrist HR
t.test(wrist_pr$x, alternative = "greater", mu= 85)

#Breath rate
t.test(br_pr$x, alternative = "greater", mu= 16)

#verifying breath rate
t.test(br_pr$x, alternative = "less", mu= 16)
nrow(br_rb)

