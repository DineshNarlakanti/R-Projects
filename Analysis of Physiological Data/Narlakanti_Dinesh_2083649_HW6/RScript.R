psy_data <- read.csv("C:/Users/ndine/Downloads/Physiological Data - QC1.csv")
psy_data <- psy_data[!is.na(psy_data$Chest_HR_QC),]
psy_data <- psy_data[!is.na(psy_data$Wrist_HR_QC),]


chest <- aggregate(psy_data$Chest_HR_QC, by = list(psy_data$Participant_ID, psy_data$Treatment), FUN = mean)
wrist <- aggregate(psy_data$Wrist_HR_QC, by = list(psy_data$Participant_ID, psy_data$Treatment), FUN = mean)

aggregated <- inner_join(chest, wrist, by = c('Group.1', 'Group.2'))
colnames(aggregated) <- c('Participant_ID', 'Treatment', 'Chest HR', 'Wrist HR')

aggregated$difference <- abs(aggregated$`Chest HR` - aggregated$`Wrist HR`)

nrow(psy_data)


relation <- lm(aggregated$`Wrist HR` ~ aggregated$`Chest HR`)


summary(relation) 


cor.test(aggregated$`Chest HR`, aggregated$`Wrist HR`)


par(mfrow=c(1,2))

plot(aggregated$`Wrist HR`,aggregated$`Chest HR`,main = "Heart Rate",
     abline(lm(aggregated$`Chest HR` ~ aggregated$`Wrist HR`)), cex = 1.3, pch = 16,
     xlab = "Chest",
     ylab = "Wrist")



PDF <- density(aggregated$difference)
plot(PDF, main = "Data with outliers")


Q <- quantile(aggregated$difference, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(aggregated$difference)

eliminated <- subset(aggregated, aggregated$difference > (Q[1] - 1.5*iqr) & aggregated$difference < (Q[2]+1.5*iqr))

relation2 <- lm(eliminated$`Wrist HR` ~ eliminated$`Chest HR`)
summary(relation2)

cor.test(eliminated$`Wrist HR`, eliminated$`Chest HR`)

nrow(eliminated)
par(mfrow=c(1,2))

plot(eliminated$`Wrist HR`,eliminated$`Chest HR`,main = "Heart Rate",
     abline(lm(eliminated$`Chest HR` ~ eliminated$`Wrist HR`)), cex = 1.3, pch = 16,
     xlab = "Chest",
     ylab = "Wrist")


PDF2 <- density(eliminated$difference)
plot(PDF2, main = "Data without outliers" )

a<- cor.test(aggregated$`Chest HR`, aggregated$`Wrist HR`)
a$estimate
