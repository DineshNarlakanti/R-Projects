install.packages('sjPlot')
install.packages('lme4')
install.packages("dplyr")
install.packages('ggplot2')
install.packages('olsrr')
install.packages('jtools')
library(olsrr)
library(lme4)
library(sjPlot)
library(ggplot2)
library(cowplot)
library(dplyr)
library(ggplot2)
library(jtools)
library(gridExtra)
library(tidyverse)
library(MASS)
library(olsrr)

all_data <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Project/Milestone-3/Data/AllData.csv")
key_data <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Project/Milestone-3/Data/KeyData.csv")

levels(factor(all_data$P_SR))
levels(factor(key_data$FC))
key_data$s30 <- ifelse(all_data$P_SR %in% c("< 10%", "10-20%", "20-30%"),0,1)
key_data$s50 <- ifelse(all_data$P_SR %in% c("50-75%","75-90%", "> 90%"),1,0)
key_data$s75 <- ifelse(key_data$FC %in% c(5,6),1,0)
key_data$sdd <- ifelse(key_data$FC %in% c(6),1,0)
key_data$s50dd <- ifelse((key_data$s50 == 1 & key_data$sdd == 1),1,0)
head(key_data)


levels(factor(all_data$P_AR))

key_data$PR <- ifelse(all_data$P_AR == "< 1 month",1,
                      ifelse(all_data$P_AR == "1-3 months", 2,
                             ifelse(all_data$P_AR == "3-6 months", 3,
                                    ifelse(all_data$P_AR == "6-12 months", 4,
                                           ifelse(all_data$P_AR == "> 12 months",5,0 )))))

head(key_data)
key_data$NP <- factor(key_data$NP)
key_data$FA <- factor(key_data$FA)
key_data$DS <- factor(key_data$DS)
key_data$BF <- factor(key_data$BF)
key_data$PR <- factor(key_data$PR)
key_data$T <- factor(key_data$T)
key_data$RS <- factor(key_data$RS)
key_data$WH <- factor(key_data$WH)
key_data$AP <- factor(key_data$AP)
key_data$DWH <- factor(key_data$DWH)


s30_fullmodel <- glm(s30 ~ NASA + Rank + NP + FA + DS + BF + PR + RS + T + WH + AP + DWH + TA + E + A + C + N + O + AC +EC + TC + H + TWR + DWR , data = key_data, family = "binomial")
s30_first <- summary(s30_fullmodel)
s30_null <- glm(s30 ~ 1, data = key_data, family = 'binomial')
s30_nullmodel <- stepAIC(s30_null, direction = "forward", scope = formula(s30_fullmodel))
summary(s30_nullmodel)
s30_forward_third <- glm(s30 ~ NP + FA  +TA + H + DS, data = key_data, family = 'binomial')
summary(s30_forward_third)
#AIC reducing while adding DS
s30_forward_fourth <- glm(s30 ~ NP + FA  +TA + H+ DS, data = key_data, family = 'binomial')
summary(s30_forward_fourth)
#both
s30_both <- stepAIC(s30_fullmodel, direction = "both")
summary(s30_both)
s30_both_second <- glm(s30 ~NP+FA+TA+H, data = key_data, family= 'binomial')
summary(s30_both_second)
s30_both_third <- glm(s30 ~NP+FA+TA+H+DS, data = key_data, family= 'binomial')
summary(s30_both_third)
#backward
s30_backward <- stepAIC(s30_fullmodel, direction = "backward")
summary(s30_backward)
s30_second <- glm(s30 ~ NP + FA + TA + H + DS, data = key_data, family = "binomial")
s30_second_backward <- stepAIC(s30_second, direction = "backward")
summary(s30_second_backward)
s30_table <- data.frame(coefficients(summary(s30_second_backward)), check.names = FALSE)
s30_table$Prob_wise <- exp(s30_table$Estimate)/(1+exp(s30_table$Estimate))
s30_table$Predictor <- c('Intercept', 'NP2', 'NP3', 'FA-NIH', 'FA-DOE', 'FA-DOD', 'FA-NASA', 'FA-OT', 'TA', 'H', 'DS2')
s30_table <- dplyr::select(s30_table, 6,5,1,2,3,4)
colnames(s30_table) <- c('Predictor', 'Prob-wise','Odds-wise', 'Std. Error', 'z value', 'Pr(>|z|)')
s30_table$`Pr(>|z|)` <- round(s30_table$`Pr(>|z|)`, digits =3)
s30_table$`Pr(>|z|)`[s30_table$`Pr(>|z|)` == 0.000] <- '<0.001'
s30_table$"." <- ifelse(s30_table$`Pr(>|z|)` == '<0.001','***',ifelse(s30_table$`Pr(>|z|)`<0.01,'**',ifelse(s30_table$`Pr(>|z|)`<0.05,'*'," ")))
tab_df(s30_table, digits = 3)


s50_fullmodel <- glm(s50 ~ Rank + NP + FA + DS + BF + PR + RS + T + WH + AP + DWH + TA + E + A + C + N + O + AC +EC + TC + H + TWR + DWR, data = key_data, family = "binomial")
s50_first <- summary(s50_fullmodel)
s50_null <- glm(s50 ~ 1, data = key_data, family = 'binomial')
s50_nullmodel <- stepAIC(s50_null, direction = "forward", scope = formula(s50_fullmodel))
summary(s50_nullmodel)
s50_forward_second <- glm(s50~NP+FA+BF+PR+E+H+AC+DS, data = key_data, family = 'binomial')
summary(s50_forward_second)
#both
s50_both <- stepAIC(s50_fullmodel, direction = "both")
summary(s50_both)
s50_both_second <- glm(s50 ~ NP+FA+DS+BF+PR+E+AC+H+DS, data = key_data, family = 'binomial')
summary(s50_both_second)
#backward
s50_backward <- stepAIC(s50_fullmodel, direction = "backward")
summary(s50_backward)
s50_second <- glm(s50 ~ NP + FA + BF + PR + E + AC + H + DS , data = key_data, family = "binomial")
summary(s50_second)
s50_second_backward <- stepAIC(s50_second, direction = "backward")
s50_table <- data.frame(coefficients(summary(s50_second)), check.names = FALSE)
s50_table$Prob_wise <- exp(s50_table$Estimate)/(1+exp(s50_table$Estimate))
s50_table$Predictor <- c('Intercept', 'NP2', 'NP3', 'FA-NIH', 'FA-DOE', 'FA-DOD', 'FA-NASA', 'FA-OT', 'BF2', 'PR2', 'PR3', 'PR4', 'PR5' , 'E', 'AC', 'H', 'DS2')
s50_table <- dplyr::select(s50_table, 6,5,1,2,3,4)
colnames(s50_table) <- c('Predictor', 'Prob-wise','Odds-wise', 'Std. Error', 'z value', 'Pr(>|z|)')
s50_table$`Pr(>|z|)` <- round(s50_table$`Pr(>|z|)`, digits =3)
s50_table$`Pr(>|z|)`[s50_table$`Pr(>|z|)` == 0.000] <- '<0.001'
s50_table$"." <- ifelse(s50_table$`Pr(>|z|)` == '<0.001','***',ifelse(s50_table$`Pr(>|z|)`<0.01,'**',ifelse(s50_table$`Pr(>|z|)`<0.05,'*'," ")))
tab_df(s50_table, digits = 3)

s75_fullmodel <- glm(s75 ~ Rank + NP + FA + DS + BF + PR + RS + T + WH + AP + DWH + TA + E + A + C + N + O + AC +EC + TC + H + TWR + DWR, data = key_data, family = "binomial")
s75_first <- summary(s75_fullmodel)
s75_null <- glm(s75 ~ 1, data = key_data, family = 'binomial')
s75_nullmodel <- stepAIC(s75_null, direction = "forward", scope = formula(s75_fullmodel))
summary(s75_nullmodel)
#Forward
s75_fourth_forward <- glm(s75 ~ FA + RS +T+O+TWR+H, data = key_data, family = 'binomial')
summary(s75_fourth_forward)
#Both
s75_both <- stepAIC(s75_fullmodel, direction = "both")
summary(s75_both)
s75_both_second <- glm(s75 ~ FA+RS+T+E+O+H+TWR, data= key_data, family='binomial')
summary(s75_both_second)
s75_both_third <- glm(s75 ~ FA+RS+T+O+H+TWR, data= key_data, family='binomial')
summary(s75_both_third)
#Backward
s75_backward <- stepAIC(s75_fullmodel, direction = "backward")
summary(s75_backward)
s75_second <- glm(s75 ~ FA + RS + T + E + O + H + TWR, data = key_data, family = "binomial")
s75_second_backward <- stepAIC(s75_second, direction = "backward")
summary(s75_second_backward)
s75_third <- glm(s75 ~ FA + RS + T + O + H + TWR, data = key_data, family = "binomial")
s75_third_backward <- stepAIC(s75_third, direction = "backward")
summary(s75_third_backward)
s75_table <- data.frame(coefficients(summary(s75_third_backward)), check.names = FALSE)
s75_table$Prob_wise <- exp(s75_table$Estimate)/(1+exp(s75_table$Estimate))
s75_table$Predictor <- c('Intercept', 'FA-NIH', 'FA-DOE', 'FA-DOD', 'FA-NASA', 'FA-OT', 'RS2', 'TS2', 'O', 'H', 'TWR')
s75_table <- dplyr::select(s75_table, 6,5,1,2,3,4)
colnames(s75_table) <- c('Predictor', 'Prob-wise','Odds-wise', 'Std. Error', 'z value', 'Pr(>|z|)')
s75_table$`Pr(>|z|)` <- round(s75_table$`Pr(>|z|)`, digits =3)
s75_table$`Pr(>|z|)`[s75_table$`Pr(>|z|)` == 0.000] <- '<0.001'
s75_table$"." <- ifelse(s75_table$`Pr(>|z|)` == '<0.001','***',ifelse(s75_table$`Pr(>|z|)`<0.01,'**',ifelse(s75_table$`Pr(>|z|)`<0.05,'*'," ")))
tab_df(s75_table, digits = 3)

sdd_fullmodel <- glm(sdd ~ Rank + NP + FA + DS + BF + PR + RS + T + WH + AP + DWH + TA + E + A + C + N + O + AC +EC + TC + H + TWR + DWR, data = key_data, family = "binomial")
sdd_first <- summary(sdd_fullmodel)
sdd_null <- glm(sdd ~ 1, data = key_data, family = 'binomial')
sdd_nullmodel <- stepAIC(sdd_null, direction = "forward", scope = formula(sdd_fullmodel))
summary(s30_nullmodel)
#forward
sdd_forward_third <- glm(sdd ~ FA+O+TWR, data = key_data, family = 'binomial')
summary(sdd_forward_third)
#both
sdd_both <- stepAIC(sdd_fullmodel, direction = "both")
summary(sdd_both)
sdd_both_second <- glm(sdd ~ FA +O+TWR, data = key_data, family = 'binomial')
summary(sdd_both_second)
#backward
sdd_backward <- stepAIC(sdd_fullmodel, direction = "backward")
summary(sdd_backward)
sdd_second <- glm(sdd ~ FA + O + TWR, data = key_data, family = "binomial")
sdd_second_backward <- stepAIC(sdd_second, direction = "backward")
summary(sdd_second_backward)
sdd_table <- data.frame(coefficients(summary(sdd_second_backward)), check.names = FALSE)
sdd_table$Prob_wise <- exp(sdd_table$Estimate)/(1+exp(sdd_table$Estimate))
sdd_table$Predictor <- c('Intercept', 'FA-NIH', 'FA-DOE', 'FA-DOD', 'FA-NASA', 'FA-OT','O', 'TWR')
sdd_table <- dplyr::select(sdd_table, 6,5,1,2,3,4)
colnames(sdd_table) <- c('Predictor', 'Prob-wise','Odds-wise', 'Std. Error', 'z value', 'Pr(>|z|)')
sdd_table$`Pr(>|z|)` <- round(sdd_table$`Pr(>|z|)`, digits =3)
sdd_table$`Pr(>|z|)`[sdd_table$`Pr(>|z|)` == 0.000] <- '<0.001'
sdd_table$"." <- ifelse(sdd_table$`Pr(>|z|)` == '<0.001','***',ifelse(sdd_table$`Pr(>|z|)`<0.01,'**',ifelse(sdd_table$`Pr(>|z|)`<0.05,'*'," ")))
tab_df(sdd_table, digits = 3)

s50dd_fullmodel <- glm(s50dd ~ Rank + NP + FA + DS + BF + PR + RS + T + WH + AP + DWH + TA + E + A + C + N + O + AC +EC + TC + H + TWR + DWR, data = key_data, family = "binomial")
s50dd_first <- summary(s50dd_fullmodel)
s50dd_null <- glm(s50dd ~ 1, data = key_data, family = 'binomial')
s50dd_nullmodel <- stepAIC(s50dd_null, direction = "forward", scope = formula(s50dd_fullmodel))
summary(s50dd_nullmodel)
#forward
s50dd_forward_third <- glm(s50dd ~ NP+E+A+TWR,data=key_data, family = 'binomial')
summary(s50dd_forward_third)
#both
s50dd_both <- stepAIC(s50dd_fullmodel, direction = "both")
summary(s50dd_both)
s50dd_both_second <- glm(s50dd ~ NP+E+A+TWR, data= key_data, family = 'binomial')
summary(s50dd_both_second)
#backward
s50dd_backward <- stepAIC(s50dd_fullmodel, direction = "backward")
summary(s50dd_backward)
s50dd_second <- glm(s50dd ~ NP + E + A + TWR, data = key_data, family = "binomial")
s50dd_second_backward <- stepAIC(s50dd_second, direction = "backward")
summary(s50dd_second_backward)
stepAIC(s50dd_fullmodel, direction = "both")
s50dd_table <- data.frame(coefficients(summary(s50dd_second_backward)), check.names = FALSE)
s50dd_table$Prob_wise <- exp(s50dd_table$Estimate)/(1+exp(s50dd_table$Estimate))
s50dd_table$Predictor <- c('Intercept', 'NP2', 'NP3', 'E', 'A', 'TWR')
s50dd_table <- dplyr::select(s50dd_table, 6,5,1,2,3,4)
colnames(sdd_table) <- c('Predictor', 'Prob-wise','Odds-wise', 'Std. Error', 'z value', 'Pr(>|z|)')
s50dd_table$`Pr(>|z|)` <- round(s50dd_table$`Pr(>|z|)`, digits =3)
s50dd_table$`Pr(>|z|)`[s50dd_table$`Pr(>|z|)` == 0.000] <- '<0.001'
s50dd_table$"." <- ifelse(s50dd_table$`Pr(>|z|)` == '<0.001','***',ifelse(s50dd_table$`Pr(>|z|)`<0.01,'**',ifelse(s50dd_table$`Pr(>|z|)`<0.05,'*'," ")))
tab_df(s50dd_table, digits = 3)



NP_type <- c("NP1", "NP2", "NP3")
FA_type <- c('NSF', 'NIH', 'DOE', 'DOD', 'NASA', 'OT')
BF_type <- c('BF1', 'BF2')
PR_type <- c('PR1','PR2','PR3','PR4','PR5')
DS_type <- c('DS1','DS2')
TS_type <- c('TS1', 'TS2')
RS_type <- c('RS1', 'RS2')
#red = "#E02E1F"
#grey = "#cccccc"

set_theme( base = theme_classic(), axis.linecolor = "white") 


#EFFECT PLOT


NP_30 <- effect_plot(s30_lm, pred = NP, colors=c("grey","red","red"),y.label = 'SG30',x.label = " "  ,main.title = 'Number of Proposals' ,cat.interval.geom = c( "linerange"))+ ylim(0,1)+ theme(panel.grid = element_blank()) + scale_x_discrete(labels= NP_type)+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1)) 
FA_30 <- effect_plot(s30_lm, pred = FA,colors=c("grey","black","black", "darkgoldenrod2", "black", 'darkgoldenrod2'), main.title = 'Funding Agency' ,cat.interval.geom = c( "linerange")) + theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1)) + ylim(0,1) + theme(panel.grid = element_blank()) + scale_x_discrete(labels= FA_type)+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
g30_bf <- data.frame()
BF_30 <- ggplot(g30_bf) + geom_point()+geom_blank()+theme_bw()+ggtitle("Break Frequency")+
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )
g30_pr <- data.frame()
PR_30 <- ggplot(g30_pr) + geom_point()+geom_blank()+theme_bw()+ggtitle("Pilot Research")+
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )

NP_50 <- effect_plot(s50_lm, pred = NP, colors=c("grey","darkgoldenrod2","red"), cat.interval.geom = c( "linerange")) + ylim(0,1)+ theme(panel.grid = element_blank(),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_x_discrete(labels= NP_type)+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))

FA_50 <- effect_plot(s50_lm, pred = FA,colors=c("grey","black","black", "cyan3", "black", 'darkgoldenrod2'), cat.interval.geom = c( "linerange"))+ ylim(" ")  + theme(panel.grid = element_blank(),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_x_discrete(labels= FA_type)
BF_50 <- effect_plot(s50_lm, pred = BF,colors=c("grey","cyan3"), cat.interval.geom = c( "linerange"))+ ylim(0,1)+ theme(panel.grid = element_blank(),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_x_discrete(labels= BF_type)+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
PR_50 <- effect_plot(s50_lm, pred = PR,colors=c("grey","cyan3", "darkgoldenrod2", "cyan3", 'cyan3'), cat.interval.geom = c( "linerange")) + ylim(0,1)+ theme(panel.grid = element_blank(),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_x_discrete(labels= PR_type)+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))

H_30 <- plot_model(s30_lm,type="pred", terms=c('H'), axis.title = c(" ","SG30"), title = "h-index")+ theme(panel.grid = element_blank()) + geom_vline(xintercept = mean(key_data$H), linetype='dashed', color='grey', size=2) + geom_path(color = 'cyan3', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
#effect_plot(s30_lm, pred = H, interval = TRUE,colors=c("cyan3"),cat.interval.geom = c( "linerange"))+ theme(panel.grid = element_blank())
DS_30 <- effect_plot(s30_lm, pred = DS,colors=c("grey", "darkgoldenrod2"), main.title = 'Deadline Stress' ,cat.interval.geom = c( "linerange"))  + ylim(0,1)+ theme(panel.grid = element_blank(),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_x_discrete(labels= DS_type)+ theme(plot.title=element_text(hjust=0.5))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
TA_30 <- plot_model(s30_lm,type="pred", terms=c('TA'),axis.title = c(" "," "), title = "Trait Anxiety") + ylim(" ")+ geom_vline(xintercept = mean(key_data$TA), linetype='dashed', color='grey', size=2) + theme(panel.grid = element_blank())+ geom_path(color = 'cyan3', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))
g30_e <- data.frame()
E_30 <- ggplot(g30_e) + geom_point()+geom_blank()+theme_bw()+ggtitle("Extraversion")+
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )
g30_e <- data.frame()
AC_30 <- ggplot(g30_e) + geom_point()+geom_blank()+theme_bw()+ggtitle("Avoidance Coping")+
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )

H_50 <- plot_model(s50_lm,type="pred", terms=c('H')) + geom_vline(xintercept = mean(key_data$H), linetype='dashed', color='grey', size=2)+ theme(panel.grid = element_blank()) + geom_path(color = 'darkgoldenrod2', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
g30_e <- data.frame()
DS_50 <- ggplot(g30_e) + geom_point()+geom_blank()+theme_bw()+
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )
g30_e <- data.frame()
TA_50 <- ggplot(g30_e) + geom_point()+geom_blank()+theme_bw()+
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )
E_50 <- plot_model(s50_lm,type="pred", terms=c('E')) + geom_vline(xintercept = mean(key_data$E), linetype='dashed', color='grey', size=2)+ theme(panel.grid = element_blank()) + geom_path(color = 'darkgoldenrod3', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
AC_50 <- plot_model(s50_lm,type="pred", terms=c('AC')) + geom_vline(xintercept = mean(key_data$AC), linetype='dashed', color='grey', size=2)+ theme(panel.grid = element_blank()) + geom_path(color = 'cyan3', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))


FA_75 <- effect_plot(s75_lm, pred = FA,colors=c("grey","black","cyan3", "darkgoldenrod2", "black", 'black'), main.title = 'Funding Agency' ,cat.interval.geom = c( "linerange")) + ylim(0,1)+ scale_x_discrete(labels= NP_type)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
TS_75 <- effect_plot(s75_lm, pred = T,colors=c("grey","cyan3"), main.title = 'Time of Submission' ,cat.interval.geom = c( "linerange")) + ylim(0,1)+ theme(panel.grid = element_blank(),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_x_discrete(labels= TS_type)+ theme(plot.title=element_text(hjust=0.5))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
TWR_75 <- plot_model(s75_lm,type="pred", terms=c('TWR')) + geom_vline(xintercept = mean(key_data$TWR), linetype='dashed', color='grey', size=2) + geom_path(color = 'darkgoldenrod3', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))

FA_dd <- effect_plot(sdd_lm, pred = FA,colors=c("grey","black","black", "red", "black", 'black'), cat.interval.geom = c( "linerange")) + ylim(0,1)+ scale_x_discrete(labels= FA_type)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
g30_e <- data.frame()
TS_dd <- ggplot(g30_e) + geom_point()+geom_blank()+theme_bw()+
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )
TWR_dd <- plot_model(sdd_lm,type="pred", terms=c('TWR')) + geom_vline(xintercept = mean(key_data$TWR), linetype='dashed', color='grey', size=2) + geom_path(color = 'red', alpha = 2)+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))


H_75 <- plot_model(s75_lm,type="pred", terms=c('H'), axis.title = c(" ","S$75"), title = "h-index") + geom_vline(xintercept = mean(key_data$H), linetype='dashed', color='grey', size=2) + geom_path(color = 'cyan3', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
RS_75 <- effect_plot(s75_lm, pred = RS,colors=c("grey","cyan3"), main.title = 'Research Style' ,cat.interval.geom = c( "linerange")) + ylim(0,1)+ theme(panel.grid = element_blank())+ scale_x_discrete(labels= RS_type)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
O_75 <- plot_model(s75_lm,type="pred", terms=c('O'), title = "Openness") + geom_vline(xintercept = mean(key_data$O), linetype='dashed', color='grey', size=2) + geom_path(color = 'cyan3', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))

g30_e <- data.frame()
H_dd <- ggplot(g30_e, axis.title = c(" "," S$$ ")) + geom_point()+geom_blank()+theme_bw()+
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )

RS_dd <- ggplot(g30_e) + geom_point()+geom_blank()+theme_bw()+
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )
O_dd <- plot_model(sdd_lm,type="pred", terms=c('O')) + geom_vline(xintercept = mean(key_data$O), linetype='dashed', color='grey', size=2) + geom_path(color = 'cyan3', alpha = 2)+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))


NP_50dd <- effect_plot(s50dd_lm, pred = NP, colors=c("grey","darkgoldenrod2","darkgoldenrod2"), cat.interval.geom = c( "linerange")) + ylim(0,1)+ theme(panel.grid = element_blank(),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_x_discrete(labels= NP_type)+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
TWR_50dd <- plot_model(s50dd_lm,type="pred", terms=c('TWR')) + geom_vline(xintercept = mean(key_data$TWR), linetype='dashed', color='grey', size=2) + geom_path(color = 'darkgoldenrod2', alpha = 2)+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
E_50dd <- plot_model(s50dd_lm,type="pred", terms=c('E')) + geom_vline(xintercept = mean(key_data$E), linetype='dashed', color='grey', size=2)+ theme(panel.grid = element_blank()) + geom_path(color = 'cyan3', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
A_50dd <- plot_model(s50dd_lm,type="pred", terms=c('A')) + geom_vline(xintercept = mean(key_data$E), linetype='dashed', color='grey', size=2)+ theme(panel.grid = element_blank()) + geom_path(color = 'cyan3', alpha = 2)+ theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill = NA, size = 1))+ scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))


plot_colors <- c("cyan3","darkgoldenrod2", "red")
legend_all <- legend(x = "bottom",inset = 0,
                     legend = c("*", "**", "***"), col = plot_colors, 
                     lwd=5, cex=1, horiz = TRUE)

grid.arrange(NP_30, FA_30, NP_50, FA_50, BF_50, PR_50, H_30 ,DS_30, TA_30, H_50, E_50, AC_50, ncol =3)

getwd()
