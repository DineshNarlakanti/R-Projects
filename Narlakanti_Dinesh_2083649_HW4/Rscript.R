library(tidyr)
library(dplyr)


psy_data_ori <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Narlakanti_Dinesh_2083649_HW3/Physiological Data.csv")

psy_data <- psy_data_ori[!is.na(psy_data_ori$Chest_HR_QC),]
psy_data <- psy_data[!is.na(psy_data$Wrist_HR_QC),]
#POOLED
chest_HR <- aggregate(psy_data$Chest_HR_QC, by = list(psy_data$Participant_ID, psy_data$Treatment), FUN = mean)

wrist_HR <- aggregate(psy_data$Wrist_HR_QC, by = list(psy_data$Participant_ID, psy_data$Treatment), FUN = mean)
par(mfrow=c(2,2))
boxplot(chest_HR$x, wrist_HR$x)

t.test(chest_HR$x, wrist_HR$x)
#equal and no difference. 

boxplot(psy_data$Chest_HR_QC, psy_data$Wrist_HR_QC)

t.test(psy_data$Chest_HR_QC, psy_data$Wrist_HR_QC)

#equal and no difference
#______________________________________________________________________________________________________________________
#PAIRED

t.test(data_wna$Chest_HR_QC, data_wna$Wrist_HR_QC, paired = TRUE)

t.test(psy_data$Chest_HR_QC, psy_data$Wrist_HR_QC, paired = TRUE)
#________________________________________________________________________________________________________________________


RB <- subset(psy_data_ori, psy_data_ori$Treatment == "RB")
RB <- RB[!is.na(RB$PP_QC),]

PR <- subset(psy_data_ori, psy_data_ori$Treatment == "PR")
PR <- PR[!is.na(PR$PP_QC),]

RB_pp <- aggregate(RB$PP_QC, by = list(RB$Participant_ID), FUN = mean)
PR_pp <- aggregate(PR$PP_QC, by = list(PR$Participant_ID), FUN = mean)

combined_data <- inner_join(RB_pp, PR_pp, by = 'Group.1')

per_second_data <- inner_join(RB, PR, by = 'Participant_ID')

boxplot(combined_data$x.x, combined_data$x.y)

t.test(combined_data$x.x, combined_data$x.y, paired = TRUE)

boxplot(per_second_data$PP_QC.x, per_second_data$PP_QC.y)
t.test(per_second_data$PP_QC.x, per_second_data$PP_QC.y, paired = TRUE )


nrow(subset(psy_data_ori, psy_data_ori$Participant_ID == 'T003' & psy_data_ori$Treatment == 'RB'))
nrow(subset(psy_data_ori, psy_data_ori$Participant_ID == 'T005' & psy_data_ori$Treatment == 'RB'))

t.test(combined_data$x.x, combined_data$x.y, alternative = 'greater', paired = TRUE)
