mtd <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Narlakanti_Dinesh_2083649_HW10/Multitasking-Data.csv")
mtd <- mtd[!is.na(mtd$PP_QC),]

mtd_rb <- subset(mtd, mtd$Treatment == 'RB')
mtd_dt <- subset(mtd, mtd$Treatment == 'DT')

nrow(mtd_dt)
nrow(mtd_rb)
levels(factor(mtd_dt$Participant_ID))
levels(factor(mtd_rb$Participant_ID))

mtd_rba <- aggregate(mtd_rb$PP_QC, by = list( mtd_rb$Participant_ID, mtd_rb$Group), FUN = mean)
mtd_dta <- aggregate(mtd_dt$PP_QC, by = list( mtd_dt$Participant_ID, mtd_dt$Group), FUN = mean)

colnames(mtd_rba) <- c('Participant_ID', 'Treatment', 'PP_QC')
colnames(mtd_dta) <- c('Participant_ID', 'Treatment', 'PP_QC')

mtd_dta$norm_pp <- 0
mtd_dta$norm_pp <- mtd_dta$PP_QC - mtd_rba$PP_QC
mtd_dta$log_norm_pp <- log(mtd_dta$PP_QC) - log(mtd_rba$PP_QC)

par(mfrow=c(1,2))
qqnorm(mtd_dta$norm_pp, main="Before log transformation")
qqnorm(mtd_dta$log_norm_pp,main="After log tranformation")

mtd_dta$Treatment <- factor(mtd_dta$Treatment)
mtd_dta$Email <- substr(mtd_dta$Treatment,1,1)
mtd_dta$Stressor <- substr(mtd_dta$Treatment,2,2)

mtd_dta$Email <- factor(mtd_dta$Email)
mtd_dta$Stressor <- factor(mtd_dta$Stressor)

drop <- c("PP_QC", "norm_pp","Treatment")
mtd_dta = mtd_dta[,!(names(mtd_dta) %in% drop)]



par(mfrow = c(1,3))
boxplot(log_norm_pp ~ Email, data = mtd_dta, main="PP by Email",
        xlab="Email type",ylab="PP")

boxplot(log_norm_pp ~ Stressor, data = mtd_dta, main="PP by Stressor",
        xlab="Stressor type",ylab="PP")

boxplot(log_norm_pp ~ Email*Stressor, data=mtd_dta, ylab="PP", xlab="Email.Stressor",main="PP by Email.Stressor")

par(mfrow = c(1,2))

interaction.plot(mtd_dta$Email, mtd_dta$Stressor,mtd_dta$log_norm_pp)


#2way anova
result <- aov(log_norm_pp ~ Email * Stressor, data = mtd_dta)
summary(result)

#glm
result_2 <- glm(log_norm_pp ~ Email * Stressor, data = mtd_dta)
summary(result_2)

pchisq(26.517,3,lower.tail=FALSE)
install.packages('AICcmodavg')

par(mfrow=c(2,2))
plot(result)
TukeyHSD(result)

anova(result, result_2)
