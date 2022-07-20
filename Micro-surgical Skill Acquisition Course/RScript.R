library(dplR)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(cowplot)


performance_data <- read.csv("C:/Users/ndine/OneDrive/Desktop/MicrosurgeryPerformance.csv")
physiology_data <- read.csv("C:/Users/ndine/OneDrive/Desktop/MicrosurgeryPhysiology.csv")

perf_lev <- levels(factor(performance_data$ID))
phy_lev <- levels(factor(physiology_data$Subject))

physiology_data <- subset(physiology_data, physiology_data$Subject %in% perf_lev)
levels(factor(physiology_data$Subject))
nrow(physiology_data)

temp_data <- physiology_data %>%
  group_by(Subject, Day, Session) %>%
  summarise(Perspiration = mean(Perspiration, na.rm = TRUE),
            .groups = 'drop')

nrow(temp_data)
baseline_phy <- subset(temp_data, temp_data$Session == 'B')
cut_sut_phy <- subset(temp_data, temp_data$Session != 'B')

nrow(baseline_phy)
nrow(cut_sut_phy)


cut_sut_phy$normal <- 0
cut_sut_phy$log_normal <- 0

for (x in nrow(cut_sut_phy)){
  if (cut_sut_phy$Subject == baseline_phy$Subject & cut_sut_phy$Day == baseline_phy$Day){
    #cut_sut_phy$normal <- cut_sut_phy$Perspiration - baseline_phy$Perspiration
    cut_sut_phy$log_normal <- log(cut_sut_phy$Perspiration) - log(baseline_phy$Perspiration)
  }
}

#_____
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

day1_performance_data <- performance_data[,c(1,8,9,10,11)]
day1_performance_data <- drop_na(day1_performance_data)
day1_gathered_data <- gather(day1_performance_data, "Task", "Score", -ID)
day1_gathered_data$Scorer <- substrRight(day1_gathered_data$Task, 1)
day1_gathered_data$Session <- substr(day1_gathered_data$Task, 7,7)
day1_gathered_data$Task <- "Session 1"
day1_gathered_data$Day <- 1


day2_performance_data <- performance_data[,c(1,15,16,17,18)]
day2_performance_data <- drop_na(day2_performance_data)
day2_gathered_data <- gather(day2_performance_data, "Task", "Score", -ID)
day2_gathered_data$Scorer <- substr(day2_gathered_data$Task,10,10)
day2_gathered_data$Session <- substr(day2_gathered_data$Task, 7,7)
day2_gathered_data$Task <- "Session 2"
day2_gathered_data$Day <- 2

day3_performance_data <- performance_data[,c(1,22,23,24,25)]
day3_performance_data <- drop_na(day3_performance_data)
day3_gathered_data <- gather(day3_performance_data, "Task", "Score", -ID)
day3_gathered_data$Scorer <- substr(day3_gathered_data$Task,10,10)
day3_gathered_data$Session <- substr(day3_gathered_data$Task, 7,7)
day3_gathered_data$Task <- "Session 3"
day3_gathered_data$Day <- 3

day4_performance_data <- performance_data[,c(1,29,30,31,32)]
day4_performance_data <- drop_na(day4_performance_data)
day4_gathered_data <- gather(day4_performance_data, "Task", "Score", -ID)
day4_gathered_data$Scorer <- substr(day4_gathered_data$Task,10,10)
day4_gathered_data$Session <- substr(day4_gathered_data$Task, 7,7)
day4_gathered_data$Task <- "Session 4"
day4_gathered_data$Day <- 4

day5_performance_data <- performance_data[,c(1,36,37,38,39)]
day5_performance_data <- drop_na(day5_performance_data)
day5_gathered_data <- gather(day5_performance_data, "Task", "Score", -ID)
day5_gathered_data$Scorer <- substr(day5_gathered_data$Task,10,10)
day5_gathered_data$Session <- substr(day5_gathered_data$Task, 7,7)
day5_gathered_data$Task <- "Session 5"
day5_gathered_data$Day <- 5


gathered_day <- rbind(day1_gathered_data, day2_gathered_data, day3_gathered_data, day4_gathered_data, day5_gathered_data)
nrow(gathered_day)

merged_data <- merge(x=gathered_day, y=cut_sut_phy, 
                     by.x = c("ID", "Session", "Day"), 
                     by.y = c("Subject", "Session", "Day"),
                     all.x=TRUE)

merged_data$Score <- as.integer(merged_data$Score)
cut_data <- subset(merged_data, merged_data$Session == 'C')
sut_data <- subset(merged_data, merged_data$Session == 'S')


model1 <- lm(Score ~ Session + Task +log_normal + Scorer + (1|Day) ,data = merged_data)
summary1 <- summary(model1)
summary1


plot1 <- ggplot(cut_data, aes(x=as.factor(Day), y=Score)) + 
  geom_boxplot(fill="white", alpha=0.2) + scale_y_continuous(limits=c(5,30)) +
  stat_summary(fun=mean, geom="point", shape=23, size=2, color="red", fill="red") + xlab("Session") + ylab("Score") + ggtitle("Cutting") + annotate('text', x = c(2, 3, 4, 5), y = 28, label = '0.000', col = 'red')


plot2 <- ggplot(sut_data, aes(x=as.factor(Day), y=Score)) + 
  geom_boxplot(fill="white", alpha=0.2) + scale_y_continuous(limits=c(5,30)) +
  stat_summary(fun=mean, geom="point", shape=23, size=2, color="red", fill="red") + xlab("Session") + ylab("Score") + ggtitle("Suturing")+ annotate('text', x = c(2, 3, 4, 5), y = 28, label = '0.000', col = 'red')

plot_grid(plot1,plot2)

cut_score <- aggregate(cut_data$Score, by = list(cut_data$Task, cut_data$ID), FUN = sum)
sub1 <- subset(cut_score, cut_score$Group.2==1)
sub1

cut_score_whole <- aggregate(cut_data$Score, by = list(cut_data$Task), FUN = sum)
cut_score_whole

sut_score_whole <- aggregate(sut_data$Score, by = list(sut_data$Task), FUN = sum)
sut_score_whole

cut_score_whole_pp <- aggregate(cut_data$log_normal, by = list(cut_data$Task), FUN = mean)
cut_score_whole_pp

sut_score_whole_pp <- aggregate(sut_data$log_normal, by = list(sut_data$Task), FUN = sum)
sut_score_whole_pp
