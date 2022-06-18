alldata <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Narlakanti_Dinesh_2083649_HW11/AllData.csv")

rank_gender <- table(alldata$Gender, alldata$Rank)
rank_gender
prop.table(rank_gender)*100

chisqt <- chisq.test(alldata$Rank, alldata$Gender)

chisqt$observed
chisqt$expected
x=seq(0,20,length=200)
y=dchisq(x,2)
plot(x,y,type="l",col="blue")

x2=seq(chisqt$statistic,20,length=100)
y2=dchisq(x2,2)

polygon(c(chisqt$statistic,x2,20),c(0,y2,0),col="red")
arrows(chisqt$statistic,0.13,chisqt$statistic,0.08)
text(chisqt$statistic,0.13,"9.392",pos=3)


barplot(prop.table(rank_gender,2)*100,beside=TRUE,legend.text=TRUE, main = "",
        ylab="% of Proportions", ylim = c(0,100), col=c("pink","blue"))
box()

library(corrplot)
corrplot(chisqt$residuals, is.cor = FALSE)

contrib <- 100*chisqt$residuals^2/chisqt$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)
