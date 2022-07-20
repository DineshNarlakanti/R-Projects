library(tidyverse)
library(grid)
library(ggplot2)
library(dplyr)
library(gridExtra)
install.packages('sjPlot')
library(sjPlot)
install.packages('lme4')
library(lme4)
library(cowplot)

mimicry_original <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Narlakanti_Dinesh_2083649_HW9/Presenter-Judges_n=39_FGSPB_PR_1HzMean_COSC6323_2022-04-06.csv")

mimicry <- subset(mimicry_original, select = c('Participant_ID', 'Group', 'Seconds', 'F_SumEmoBinary', 'J_SumEmoBinary', 'PP_QC', 'BL_PP', 'BFI_Agreeableness'))
names(mimicry)

mimicry <- mimicry %>% drop_na(Participant_ID, F_SumEmoBinary, J_SumEmoBinary, PP_QC, BL_PP, BFI_Agreeableness)

mimicry$delta_pp <- log(mimicry$PP_QC) - log(mimicry$BL_PP)

mimicry <- mimicry %>% group_by((Participant_ID)) %>% mutate(delta_pp_mean = mean(delta_pp, na.rm = TRUE))

mimicry$Participant_ID <- substr(mimicry$Participant_ID,3,4) 
mimicry$Participant_ID <- as.numeric(mimicry$Participant_ID)

mimicry$Group <- as.factor(mimicry$Group)
mimicry$F_SumEmoBinary <- as.factor(mimicry$F_SumEmoBinary)
mimicry$J_SumEmoBinary <- as.factor(mimicry$J_SumEmoBinary)

mimicry$Group<- relevel(mimicry$Group,2)

relation <- glmer(F_SumEmoBinary ~ (J_SumEmoBinary*Group) + (J_SumEmoBinary*delta_pp_mean) + (J_SumEmoBinary*BFI_Agreeableness) + (1|(Participant_ID)) , family = "binomial", 
                data = mimicry)
summary(relation)
names(mimicry)

a <- plot_model(relation,type="pred",terms="J_SumEmoBinary", title="")
b <- plot_model(relation,type="pred",terms= "Group", title="")
c <- plot_model(relation, type = "pred", terms = c("J_SumEmoBinary","Group"), title="")
d <- plot_model(relation, type = 'pred', terms = "delta_pp_mean", title="")
e <- plot_model(relation, type = 'pred', terms = "BFI_Agreeableness", title="")



plot_grid(a,b,c,d,e)

