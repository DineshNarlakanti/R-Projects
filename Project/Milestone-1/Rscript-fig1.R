all_data <- read.csv("C:/Users/ndine/Semester 2/Stats cosc 6323/Project/AllData.csv")

all_data$Sno <- c(1:nrow(all_data))
head(all_data)

#1
table(all_data$Gender)
n <- nrow(all_data)
gender_percentage <- table(all_data$Gender)/n * 100
par(mfrow=c(2,3))
gender_dist <- barplot(table(all_data$Gender), ylim=c(0,300), xlim=c(0,2), ylab="# of Respondents", main="Gender Distribution", col = "steelblue", border="white", width = 0.7)
#text(x = gender_dist, y = gender_percentage, label = round(gender_percentage,1), pos = 3,adj = 0.5, col = "black")
gpf <- paste(round(gender_percentage[1],1),'%')
gpm <- paste(round(gender_percentage[2],1),'%')
text(0.5,150,gpf)
text(1.3,295,gpm)
box("figure")


#2
table(all_data$Department)
table(all_data$Department_Other)
NAT <- c('Applied Math', 'Chemistry and Biochemistry', 'Oceanography', 'Environmental Studies', 'Chemistry', 'Physics', 'Geosciences', 'Mathematics')
CIS <- c( 'Informatics', 'Information', 'Information and Logistics Technology', 'Information Science', 'Computer Science', 'Scientific Computing')
ENG <- c('Engineering / Learning Sciences', 'Management', 'Statistical Sciences and Operations Research', 'Technology', 'Engineering')
BIO <- c('Health', 'HHP','Health and Human Performance', 'Neurobiology', 'Neuroscience', 'Optometry', 'Pharmacological and Pharmaceutical Sciences', 'Pharmacy', 'Vision Science', 'Biology', 'Medicine')
BEHAV <- c('Political Science', 'Sociology', 'Psychology', 'Speech, Language, and Hearing', 'Political Science', 'Communication Studies')
all_data$dept <- ifelse(all_data$Department %in% NAT | all_data$Department_Other %in% NAT, 'NAT', 
                        ifelse(all_data$Department %in% CIS | all_data$Department_Other %in% CIS, 'CIS', 
                               ifelse(all_data$Department %in% ENG | all_data$Department_Other %in% ENG, 'ENG', 
                                      ifelse(all_data$Department %in% BIO | all_data$Department_Other %in% BIO, 'BIO', 'BEHAV'))))
head(all_data)
dept_percentage <- table(all_data$dept)/n * 100
dept_dist <- barplot(table(all_data$dept),ylab="# of Respondents", ylim = c(0,250), space = 1, main="Disciplinary Distribution", col = "steelblue", border="white")
#text(x = dept_dist, y = dept_percentage, label = round(dept_percentage), pos = 3, col = "black")
text(1.5,52,paste(round(dept_percentage[1]),'%'), cex = 0.9)
text(3.5,73,paste(round(dept_percentage[2]),'%'), cex = 0.9)
text(5.5,85,paste(round(dept_percentage[3]),'%'), cex = 0.9)
text(7.5,125,paste(round(dept_percentage[4]),'%'), cex = 0.85)
text(9.5,123,paste(round(dept_percentage[5]),'%'), cex = 0.9)
box("figure")

#3
table(all_data$State)
East <- c('Alabama', 'New Jersey', 'Massachusetts', 'Connecticut', 'New York', 'Pennsylvania', 'North Carolina', 'Virginia',  'Maryland')
West <- c('Colorado', 'Arizona', 'California', 'Nevada', 'New Mexico', 'Oregon', 'Utah')
Midwest <- c('Illinois', 'Michigan', 'Minnesota', 'Ohio', 'Wisconsin' )
South <- c('Florida', 'Georgia', 'Oklahoma', 'Texas')
all_data$geo <- ifelse(all_data$State %in% East, 'East', 
                       ifelse(all_data$State %in% West, 'West', 
                              ifelse(all_data$State %in% Midwest, 'Midwest','South')))
head(all_data) 
geo_percentage <- table(all_data$geo)/n * 100
geo_dist <- barplot(table(all_data$geo),ylab="# of Respondents", space =1, ylim = c(0,200), main="Geographic Distribution", col = "steelblue", border="white")
#text(x = geo_dist, y = geo_percentage, label = round(geo_percentage), pos = 3, col = "black")
text(1.5,85,paste(round(geo_percentage[1]),'%'))
text(3.6,92,paste(round(geo_percentage[2]),'%'))
text(5.6, 185,paste(round(geo_percentage[3]),'%'))
text(7.6,87,paste(round(geo_percentage[4]),'%'))
box("figure")


#4
table(all_data$WH)
wh_percentage <- table(all_data$WH)/n * 100
week_work <- barplot(table(all_data$WH),ylim= c(0,250), ylab="# of Respondents", main="Weekly Workload [hrs]", col = "steelblue")
#text(x = week_work, y = wh_percentage, label = round(wh_percentage), pos = 3, col = "black")
text(0.7,35,paste(round(wh_percentage[1]),'%'))
text(2,245,paste(round(wh_percentage[2]),'%'))
text(3.1,50,paste(round(wh_percentage[3]),'%'))
text(4.3,155,paste(round(wh_percentage[4]),'%'))
box("figure")

#5
table(all_data$TWR)
all_data$rl <- ifelse(all_data$TWR <=20, 20, 
                      ifelse(all_data$TWR == 30, 30, 
                             ifelse(all_data$TWR == 40, 40,
                                    ifelse(all_data$TWR == 50, 50,
                                           ifelse(all_data$TWR == 60, 60, 70)))))
rl_percentage <- table(all_data$rl)/n * 100
research_load <- barplot(table(all_data$rl),ylim= c(0,100), space = 1, main="Research Load [%]", col = "steelblue", border="white", names.arg = c('<20','30','40','50','60','>70'))
#text(x = research_load, y = rl_percentage, label = round(rl_percentage), pos = 3, col = "black")
text(1.5,60, paste(round(rl_percentage)[1],'%'), cex = 0.9)
text(3.5,87, paste(round(rl_percentage)[2],'%'), cex = 0.9)
text(5.5,95, paste(round(rl_percentage)[3],'%'), cex = 0.9)
text(7.5,97, paste(round(rl_percentage)[4],'%'), cex = 0.9)
text(9.5,57, paste(round(rl_percentage)[5],'%'), cex = 0.9)
text(11.5,35, paste(round(rl_percentage)[6],'%'), cex = 0.9)
box("figure")

#6
table(all_data$Research_Funded_By_External_Grants)
fc_percentage <- table(all_data$Research_Funded_By_External_Grants)/n * 100
fund_cov <- barplot(table(all_data$Research_Funded_By_External_Grants), ylim=c(0,110) , space =0.5,  main = "Funding Coverage [%]", border="white", col = "steelblue", names.arg = c('75-10','1-25','25-50','50-75','FF','NF'))
#text(x = fund_cov, y = fc_percentage, label = round(fc_percentage), pos = 3, col = "black")
#text(1,100,paste(round(fc_percentage)[1],'%'))
text(2.5,60,paste(round(fc_percentage)[2],'%'))
text(4,50,paste(round(fc_percentage)[3],'%'))
text(5.5,80,paste(round(fc_percentage)[4],'%'))
text(7,95,paste(round(fc_percentage)[5],'%'))
text(8.5,40,paste(round(fc_percentage)[6],'%'))
box("figure")




