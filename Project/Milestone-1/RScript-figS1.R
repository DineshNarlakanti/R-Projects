all_data <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Project/AllData.csv")

filtered <- all_data[44:48]
n <- nrow(filtered)
head(filtered)
levs <- c(1,2,3,4,5)
table <- sapply(filtered, function(all_data) table(factor(all_data, levels = levs, ordered = TRUE)))
class(table)
barplot(table/n*100, 
        ylab = 'Respondents [%]',
        col = c("blue", "cyan",  "white","pink","red"), ylim=c(0,100),  
        ,names.arg = c('Regular\nResearch', 'Sleep', 'Diet', 'Physical\nActivity', 'Interpersonal\nRelationships'),
        border = "black")
legend('top',legend = c("No Disruption","","","", 'Extreme disruption'),
       xpd =  TRUE, fill = c("blue", "cyan",  "white","pink","red"),inset = c(0,-0.15), bty ='n', horiz = TRUE, text.width =  c(0.50,0.01,0.01,0.01,0.10) )


