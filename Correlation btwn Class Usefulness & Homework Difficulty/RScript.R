install.packages('readxl')
library('readxl')

student_data <- read_excel("C:/Users/ndine/Semester 2/Stats cosc 6323/Narlakanti_Dinesh_2083649_HW12/Survey-COSC6323-2022 (1).xlsx")

head(student_data)
colnames(student_data) <- c('Survey Number', 'Q1', 'Q2', "Feedback")

levels(factor(student_data$Q1))

student_data$Q1_scale <- ifelse(student_data$Q1 =="Strongly Disagree", 1,
             ifelse(student_data$Q1=="Disagree",2,
                    ifelse(student_data$Q1=="Neither Agree nor Disagree", 3,
                           ifelse(student_data$Q1=="Agree", 4,
                                  ifelse(student_data$Q1=="Strongly Agree", 5,0)))))


student_data$Q2_scale <- ifelse(student_data$Q2 =="Very Easy", 1,
                                ifelse(student_data$Q2=="Easy",2,
                                       ifelse(student_data$Q2=="Neutral", 3,
                                              ifelse(student_data$Q2=="Difficult", 4,
                                                     ifelse(student_data$Q2=="Very Difficult", 5,0)))))

par(mfrow=c(1,2))
shapiro.test(student_data$Q1_scale)
shapiro.test(student_data$Q2_scale)

cor.test(student_data$Q1_scale, student_data$Q2_scale, method = 'spearman', exact = FALSE)
#coeff negative -> negative corelation -> useful inc - difficulty decreases
# p-val less than zero - significant correlation. 



beginning_middle_end <- subset(student_data, student_data$`Survey Number` %in% c(1,6,11))


shapiro.test(beginning_middle_end$Q1_scale)
shapiro.test(beginning_middle_end$Q2_scale)

kruskal.test(beginning_middle_end$`Survey Number` ~ beginning_middle_end$Q1_scale)
#pval greater than 0.05 - no difference in sentiments

install.packages('ggplot2')
library("ggpubr")
ggboxplot(beginning_middle_end, x = "Survey Number", y = "Q1_scale", 
          color = "Survey Number", 
          palette = c("#00AFBB", "#E7B800", "#FC4E07"))

ggline(beginning_middle_end, x = "Survey Number", y = "Q1_scale", 
          color = "Survey Number", 
          palette = c("#00AFBB", "#E7B800", "#FC4E07"))

install.packages('tm')
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text <- student_data$Feedback
docs <- Corpus(VectorSource(text))
inspect(docs)

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, c("unanswered", "can", "get","null"))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 8)

findAssocs(dtm, terms = "class", corlimit = 0.4)
findAssocs(dtm, terms = "shaila", corlimit = 0.5)
findAssocs(dtm, terms = "vitalii", corlimit = 0.3)
findAssocs(dtm, terms = "doubts", corlimit = 0.5)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
