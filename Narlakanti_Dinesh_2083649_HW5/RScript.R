install.packages("dplR")
install.packages("car")
library(car)
library(tidyr)
library(dplyr)


#Q1______________________________________________________________
psy_data <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Narlakanti_Dinesh_2083649_HW3/Physiological Data.csv")

par(mfrow=c(3,2))
RB <- subset(psy_data, psy_data$Treatment == "RB")
RB <- RB[!is.na(RB$PP_QC),]
RB$PP_QC <- log(RB$PP_QC)

PR <- subset(psy_data, psy_data$Treatment == "PR")
PR <- PR[!is.na(PR$PP_QC),]
PR_before <- PR$PP_QC
hist(PR_before)
PR$PP_QC <- log(PR$PP_QC)
hist(PR$PP_QC)

ST <- subset(psy_data, psy_data$Treatment == "ST")
ST <- ST[!is.na(ST$PP_QC),]
ST_before <- ST$PP_QC
hist(ST_before)
ST$PP_QC <- log(ST$PP_QC)
hist(ST$PP_QC)

DT <- subset(psy_data, psy_data$Treatment == "DT")
DT <- DT[!is.na(DT$PP_QC),]
DT_before <- DT$PP_QC
hist(DT_before)
DT$PP_QC <- log(DT$PP_QC)
hist(DT$PP_QC)

RB_pp <- aggregate(RB$PP_QC, by = list(RB$Participant_ID), FUN = mean)
PR_pp <- aggregate(PR$PP_QC, by = list(PR$Participant_ID), FUN = mean)
ST_pp <- aggregate(ST$PP_QC, by = list(ST$Participant_ID), FUN = mean)
DT_pp <- aggregate(DT$PP_QC, by = list(DT$Participant_ID), FUN = mean)

shapiro.test(PR_pp$x)
PR_RB <- inner_join(RB_pp, PR_pp, by = 'Group.1')
ST_RB <- inner_join(RB_pp, ST_pp, by = 'Group.1')
DT_RB <- inner_join(RB_pp, DT_pp, by = 'Group.1')


PR_RB$aroused <- PR_RB$x.y - PR_RB$x.x
ST_RB$aroused <- ST_RB$x.y - ST_RB$x.x
DT_RB$aroused <- DT_RB$x.y - DT_RB$x.x

PR_RB$Treatment <- 'PR'
ST_RB$Treatment <- 'ST'
DT_RB$Treatment <- 'DT'

comby_data <- rbind(PR_RB, ST_RB, DT_RB)
treatment_factors <- factor(comby_data$Treatment)

comb_res <- aov(comby_data$aroused ~ treatment_factors, data = comby_data)
summary(comb_res)
boxplot(comby_data$aroused ~ treatment_factors)
means <- tapply(comby_data$aroused, treatment_factors, mean)
points( means, pch=8, col="red")

#leveneTest(comby_data$aroused ~ treatment_factors, center = mean)

posthoc <- TukeyHSD(x = comb_res, "treatment_factors", conf.level=0.95)
posthoc

#Q2____________________________________________________________________


RB_2 <- subset(psy_data, psy_data$Treatment == "RB")
RB_2 <- RB_2[!is.na(RB_2$Chest_HR_QC),]

ST_2 <- subset(psy_data, psy_data$Treatment == "ST")
ST_2 <- ST_2[!is.na(ST_2$Chest_HR_QC),]

DT_2 <- subset(psy_data, psy_data$Treatment == "DT")
DT_2 <- DT_2[!is.na(DT_2$Chest_HR_QC),]

PR_2 <- subset(psy_data, psy_data$Treatment == "PR")
PR_2 <- PR_2[!is.na(PR_2$Chest_HR_QC),]

RB_chest <- aggregate(RB_2$Chest_HR_QC, by = list(RB_2$Participant_ID), FUN = mean)
PR_chest <- aggregate(PR_2$Chest_HR_QC, by = list(PR_2$Participant_ID), FUN = mean)
ST_chest <- aggregate(ST_2$Chest_HR_QC, by = list(ST_2$Participant_ID), FUN = mean)
DT_chest <- aggregate(DT_2$Chest_HR_QC, by = list(DT_2$Participant_ID), FUN = mean)

PR_RB_chest <- inner_join(RB_chest, PR_chest, by = 'Group.1')
ST_RB_chest <- inner_join(RB_chest, ST_chest, by = 'Group.1')
DT_RB_chest <- inner_join(RB_chest, DT_chest, by = 'Group.1')


PR_RB_chest$aroused <- PR_RB_chest$x.y - PR_RB_chest$x.x 
ST_RB_chest$aroused <- ST_RB_chest$x.y - ST_RB_chest$x.x
DT_RB_chest$aroused <- DT_RB_chest$x.y - DT_RB_chest$x.x

PR_RB_chest$Treatment <- 'PR'
ST_RB_chest$Treatment <- 'ST'
DT_RB_chest$Treatment <- 'DT'

comby_data_chest <- rbind(PR_RB_chest, ST_RB_chest, DT_RB_chest)
treatment_factors_chest <- factor(comby_data_chest$Treatment)

comb_res_chest <- aov(comby_data_chest$aroused ~ treatment_factors_chest, data = comby_data_chest)
summary(comb_res_chest)
boxplot(comby_data_chest$aroused ~ treatment_factors_chest)
means_chest <- tapply(comby_data_chest$aroused, treatment_factors_chest, mean)
points( means_chest, pch=8, col="red")

#leveneTest(comby_data_chest$aroused ~ treatment_factors_chest, center = mean)

posthoc_chest <- TukeyHSD(x = comb_res_chest, "treatment_factors_chest", conf.level=0.95)
posthoc_chest

par(mfrow=c(3,2))
qqnorm(PR_before, main = "PR pp_qc before normalization", col = "steelblue")
qqnorm(PR$PP_QC, main = "PR pp_qc after normalization",col = "steelblue")
qqnorm(ST_before, main = "ST pp_qc before normalization",col = "steelblue")
qqnorm(ST$PP_QC, main = "ST pp_qc after normalization",col = "steelblue")
qqnorm(DT_before, main = "DT pp_qc before normalization",col = "steelblue")
qqnorm(DT$PP_QC, main = "DT pp_qc after normalization",col = "steelblue")
