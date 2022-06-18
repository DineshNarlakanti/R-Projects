#PART-1

install.packages("EMT")
library(EMT)
gender_distribution <- c(Male=272, Female = 131)
US_academia<-c(Male_p=0.648,Female_p=0.352)

#________________________________

#PART-2

library(ggplot2)
library(cowplot)
g50 <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Project/Milestone-2/Data/G50.csv")
dd <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Project/Milestone-2/Data/$$.csv")
d75 <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Project/Milestone-2/Data/$75.csv")
g30 <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Project/Milestone-2/Data/G30.csv")
g50dd <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Project/Milestone-2/Data/G50$$.csv")


#1
g30_np <- g30[c(1:3),]
g30_np$type <- c("NP1","NP2","NP3")
g30_np$NP <- factor(g30_np$NP, levels = g30_np$NP)
plot30_np <- ggplot(g30_np, aes(x=type, y=Prediction*100)) + geom_pointrange(aes(ymin=Lower_limit*100, ymax=Upper_limit*100), color = ifelse(g30_np$Prediction > 0.5, "grey", "red"))+ ggtitle("Number of Proposals") +xlab(" ") + ylab("SG30")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#2
g30_fa <- g30[c(4:9),]
g30_fa$FA <- factor(g30_fa$FA, levels = g30_fa$FA)
plot30_fa <- ggplot(g30_fa, aes(x=FA, y=Prediction*100)) + geom_pointrange(aes(ymin=Lower_limit*100, ymax=Upper_limit*100), color = ifelse(g30_fa$Prediction==0.5654074,"grey",ifelse(g30_fa$Prediction > 0.8, "darkgoldenrod2", "black")))+ ggtitle("Funding Agency") +xlab(" ") + ylab("SG30")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)


#3
g30_bf <- data.frame()
plot30_bf <- ggplot(g30_bf) + geom_point()+geom_blank()+theme_bw()+ggtitle("Break Frequency")
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )


#4
g30_pr <- data.frame()
plot30_pr <- ggplot(g30_pr) + geom_point()+geom_blank()+theme_bw()+ggtitle("Pilot Research")
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )


#G50
#5
g50_np <- g50[c(1:3),]
g50_np$NP <- factor(g50_np$NP, levels = g50_np$NP)
plot50_np <- ggplot(g50_np, aes(x=NP, y=Pred.*100)) + geom_pointrange(aes(ymin=Lower_L*100, ymax=Upper_L*100), color = ifelse(g50_np$Pred.==0.52496689
,"grey",ifelse(g50_np$Pred. > 0.2, "darkgoldenrod2", "red")))+xlab(" ") + ylab("SG50")+ ylim(0,100) + theme(
plot.title = element_text(hjust = 0.5),
panel.background = element_rect(fill='transparent'),
plot.background = element_rect(fill='transparent', color=NA),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
legend.background = element_rect(fill='transparent'),
legend.box.background = element_rect(fill='transparent')
)


#6
g50_fa <- g50[c(4:9),]
g50_fa$FA <- factor(g50_fa$FA, levels = g50_fa$FA)
plot50_fa <- ggplot(g50_fa, aes(x=FA, y=Pred.*100)) + geom_pointrange(aes(ymin=Lower_L*100, ymax=Upper_L*100), color = ifelse(g50_fa$Lower_L == 0.26502507,"grey",ifelse(g50_fa$Pred. < 0.7774556, "black", ifelse(g50_fa$Pred. > 0.8, "darkgoldenrod2", "cyan3")))) +xlab(" ") + ylab("SG50")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#7
g50_bf <- g50[c(11:12),]
g50_bf$type <- c("BF2","BF1")
plot50_bf <- ggplot(g50_bf, aes(x=type, y=Pred.*100)) + geom_pointrange(aes(ymin=Lower_L*100, ymax=Upper_L*100), color = ifelse(g50_bf$Pred.> 0.7,"cyan3","grey"))+xlab(" ") + ylab("SG50")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#8
g50_pr <- g50[c(14:18),]
g50_pr$type <- c("PR1","PR2","PR3","PR4","PR5")
plot50_pr <- ggplot(g50_pr, aes(x=type, y=Pred.*100)) + geom_pointrange(aes(ymin=Lower_L*100, ymax=Upper_L*100), color = ifelse(g50_pr$Pred.>  0.5,"grey",ifelse(g50_pr$Pred.< 0.1608640,"darkgoldenrod2","cyan3")))+xlab(" ") + ylab("SG50")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)


#______________________________________________________________________
#9    
g30_h <- g30[c(12:14),]
plot30_h<- ggplot(g30_h, aes(x=H,y=Prediction*100)) + ylim(0,100)+ xlim(0,200) +
  geom_point(size = 1,color = 'cyan3') + geom_path(color = 'cyan3', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(g30_h$H),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=g30_h, aes(x=H, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ ggtitle("h-index")+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )
#10
g30_ds <- g30[c(10:11),]
g30_ds$type <- c("DS1", "DS2")
plot30_ds <- ggplot(g30_ds, aes(x=type, y=Prediction*100)) + geom_pointrange(aes(ymin=Lower_limit*100, ymax=Upper_limit*100), color = ifelse(g30_ds$Prediction > 0.5,"grey","darkgoldenrod2"))+ ggtitle("Deadline Stress") +xlab(" ") + ylab("SG30")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#11
g30_ta <- g30[c(15:17),]
plot30_ta<- ggplot(g30_ta, aes(x=TA,y=Prediction*100)) + ylim(0,100)+ xlim(20,70) +
  geom_point(size = 1,color = 'cyan3') + geom_path(color = 'cyan3', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(g30_ta$TA),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=g30_ta, aes(x=TA, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ ggtitle("Trait Anxiety")+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

#12
g30_e <- data.frame()
plot30_e <- ggplot(g30_e) + geom_point()+geom_blank()+theme_bw()+ggtitle("Extraversion")
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )


#13
g30_ac <- data.frame()
plot30_ac <- ggplot(g30_ac) + geom_point()+geom_blank()+theme_bw()+ggtitle("Avoidance coping")
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )


#14
g50_h <- g50[c(19:21),]
plot50_h<- ggplot(g50_h, aes(x=H,y=Pred.*100)) + ylim(0,100)+ xlim(0,200) +
  geom_point(size = 1,color = 'goldenrod2') + geom_path(color = 'goldenrod2', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(g50_h$H),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=g50_h, aes(x=H, ymin=100*(Lower_L), ymax=100*(Upper_L)),alpha=0.2,inherit.aes = FALSE)+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )


#15
g50_ds <- data.frame()
plot50_ds <- ggplot(g50_ds) + geom_point()+geom_blank()+theme_bw()
theme (plot.title = element_text(hjust = 0.5),
       panel.bdskground = element_rect(fill='transparent'),
       plot.bdskground = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.bdskground = element_rect(fill='transparent'),
       legend.box.bdskground = element_rect(fill='transparent') )


#16
g50_ta <- data.frame()
plot50_ta <- ggplot(g50_ta) + geom_point()+geom_blank()+theme_bw()
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )


#17
g50_e <- g50[c(22:24),]
plot50_e<- ggplot(g50_e, aes(x=EX,y=Pred.*100)) + ylim(0,100)+ xlim(2,10) +
  geom_point(size = 1,color = 'goldenrod2') + geom_path(color = 'goldenrod2', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(g50_e$EX),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=g50_e, aes(x=EX, ymin=100*(Lower_L), ymax=100*(Upper_L)),alpha=0.2,inherit.aes = FALSE)+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

#18
g50_ac <- g50[c(25:27),]
plot50_ac<- ggplot(g50_ac, aes(x=AV,y=Pred.*100)) + ylim(0,100)+ xlim(10,30) +
  geom_point(size = 1,color = 'cyan3') + geom_path(color = 'cyan3', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(g50_ac$AV),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=g50_ac, aes(x=AV, ymin=100*(Lower_L), ymax=100*(Upper_L)),alpha=0.2,inherit.aes = FALSE)+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )


#___________________________________________________________________________

#19

d75_fa <- d75[c(1:6),]
d75_fa$FA <- factor(d75_fa$FA, levels = d75_fa$FA)
plot75_fa <- ggplot(d75_fa, aes(x=FA, y=Prediction*100)) + geom_pointrange(aes(ymin=Lower_limit*100, ymax=Upper_limit*100), color = ifelse(d75_fa$Prediction == 0.5738049, "grey", ifelse(d75_fa$Prediction < 0.8543334, "black", ifelse(d75_fa$Prediction > 0.8543334, "cyan3","goldenrod2")) ))+ ggtitle("Funding Agency") +xlab(" ") + ylab("SG50")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)
#20
d75_ts <- d75[c(7:8),]
d75_ts$type <- c("TS1", "TS2")
plot75_ts <- ggplot(d75_ts, aes(x=type, y=Prediction*100)) + geom_pointrange(aes(ymin=Lower_limit*100, ymax=Upper_limit*100), color = ifelse(d75_ts$T == 1, "grey", "cyan3"))+ ggtitle("Time of Submissions") +xlab(" ") + ylab("SG50")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#21
d75_twr <- d75[c(11:13),]
plot75_twr<- ggplot(d75_twr, aes(x=TWR,y=Prediction*100)) + ylim(0,100)+ xlim(0,100) +
  geom_point(size = 1,color = 'goldenrod2') + geom_path(color = 'goldenrod2', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(d75_twr$TWR),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=d75_twr, aes(x=TWR, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ ggtitle("Typical Week Research")+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

#22
dd_fa <- dd[c(1:6),]
dd_fa$FA <- factor(dd_fa$FA, levels = dd_fa$FA)
plotdd_fa <- ggplot(dd_fa, aes(x=FA, y=Prediction*100)) + geom_pointrange(aes(ymin=Lower_limit*100, ymax=Upper_limit*100), color = ifelse(dd_fa$Prediction == 0.5738049, "grey", ifelse(dd_fa$Prediction == 0.1699917, "grey", ifelse(dd_fa$Prediction < 0.5, "black","red")) )) +xlab(" ") + ylab("SG50")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#23
dd_ts <- data.frame()
plotdd_ts <- ggplot(dd_ts) + geom_point()+geom_blank()+theme_bw()
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )


#24
dd_twr <- dd[c(7:9),]
plotdd_twr<- ggplot(dd_twr, aes(x=TWR,y=Prediction*100)) + ylim(0,100)+ xlim(0,100) +
  geom_point(size = 1,color = 'red') + geom_path(color = 'red', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(dd_twr$TWR),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=dd_twr, aes(x=TWR, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

#_________________________________________________________________________________


#25
d75_h <- d75[c(14:16),]
plotd75_h<- ggplot(d75_h, aes(x=H,y=Prediction*100)) + ylim(0,100)+ xlim(0,100) +
  geom_point(size = 1,color = 'cyan3') + geom_path(color = 'cyan3', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(d75_h$H),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=d75_h, aes(x=H, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ ggtitle("h-index")+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

#26
d75_rs <- d75[c(9:10),]
d75_rs$type <- c("RS1","RS2")
plotd75_rs <- ggplot(d75_rs, aes(x=type, y=Prediction*100)) + geom_pointrange(aes(ymin=Lower_limit*100, ymax=Upper_limit*100), color = ifelse(d75_rs$Prediction > 0.5, "grey", "cyan3"))+ ggtitle("Research Style") +xlab(" ") + ylab("SG50")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#27
d75_op <- d75[c(17:19),]
plotd75_op<- ggplot(d75_op, aes(x=OP,y=Prediction*100)) + ylim(0,100)+ xlim(2,10) +
  geom_point(size = 1,color = 'cyan3') + geom_path(color = 'cyan3', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(d75_op$OP),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=d75_op, aes(x=OP, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ ggtitle("Openess")+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

#28
dd_h <- data.frame()
plotdd_h <- ggplot(dd_h) + geom_point()+geom_blank()+theme_bw()
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )

#29
dd_rs <- data.frame()
plotdd_rs <- ggplot(dd_rs) + geom_point()+geom_blank()+theme_bw()
theme (plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.background = element_rect(fill='transparent'),
       legend.box.background = element_rect(fill='transparent') )


#30
dd_op <- dd[c(10:12),]
plotdd_op<- ggplot(dd_op, aes(x=OP,y=Prediction*100)) + ylim(0,100)+ xlim(2,10) +
  geom_point(size = 1,color = 'cyan3') + geom_path(color = 'cyan3', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(dd_op$OP),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=dd_op, aes(x=OP, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

#_____________________________________________________________________

#31
g50dd_np <- g50dd[c(1:3),]
g50dd_np$type <- c("NP1","NP2","NP3")
plotg50dd_np <- ggplot(g50dd_np, aes(x=type, y=Prediction*100)) + geom_pointrange(aes(ymin=Lower_limit*100, ymax=Upper_limit*100), color = ifelse(g50dd_np$Prediction == 0.102912834, "grey", "goldenrod2"))+ ggtitle("Number of Proposals") +xlab(" ") + ylab("SG50$$")+ ylim(0,100) + theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#32
g50dd_twr <- g50dd[c(4:6),]
plotg50dd_twr<- ggplot(g50dd_twr, aes(x=TWR,y=Prediction*100)) + ylim(0,100)+ xlim(0,100) +
  geom_point(size = 1,color = 'goldenrod2') + geom_path(color = 'goldenrod2', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(g50dd_twr$TWR),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=g50dd_twr, aes(x=TWR, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ ggtitle("Typical Week Research")+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

#33
g50dd_e <- g50dd[c(7:9),]
plotg50dd_e<- ggplot(g50dd_e, aes(x=EX,y=Prediction*100)) + ylim(0,100)+ xlim(2,10) +
  geom_point(size = 1,color = 'cyan3') + geom_path(color = 'cyan3', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(g50dd_e$EX),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=g50dd_e, aes(x=EX, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ ggtitle("Extraversion")+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

#34
g50dd_ag <- g50dd[c(10:12),]
plotg50dd_ag<- ggplot(g50dd_ag, aes(x=AGR,y=Prediction*100)) + ylim(0,100)+ xlim(4,10) +
  geom_point(size = 1,color = 'cyan3') + geom_path(color = 'cyan3', alpha = 2, size = 1.5) + geom_vline(xintercept = mean(g50dd_ag$AGR),linetype='dashed', color='grey', size=2)+
  geom_ribbon(data=g50dd_ag, aes(x=AGR, ymin=100*(Lower_limit), ymax=100*(Upper_limit)),alpha=0.2,inherit.aes = FALSE)+ ggtitle("Agreeableness")+ xlab(" ")+ ylab(" ") + theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

plot_grid(plot30_np, plot30_fa, plot30_bf, plot30_pr, plot50_np, plot50_fa, plot50_bf, plot50_pr,ncol=4)
plot_grid(plot30_h, plot30_ds, plot30_ta, plot30_e,plot30_ac, plot50_h, plot50_ds, plot50_ta, plot50_e,plot50_ac,ncol=5)
plot_grid(plot75_fa, plot75_ts, plot75_twr,plotdd_fa, plotdd_ts, plotdd_twr,ncol=3)
plot_grid(plotd75_h, plotd75_rs, plotd75_op,plotdd_h, plotdd_rs, plotdd_op,ncol=3)
plot_grid(plotg50dd_np, plotg50dd_twr, plotg50dd_e,plotg50dd_ag,ncol=4)



