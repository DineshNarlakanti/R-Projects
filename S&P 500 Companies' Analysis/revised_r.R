setwd("~/IA")

install.packages("dbplyr")
install.packages("dplyr")
install.packages("readxl")
install.packages('tidyverse')
install.packages('fs')
install.packages("sqldf")
install.packages("WriteXLS")

library(RPostgres)
library(dplyr)
library(dbplyr)
library(readxl)
library(fs)
library(tidyverse)
library(sqldf)
library(WriteXLS)

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='sesh')

sp500_latest <- read_excel("SP-500-Companies-List-1.xlsx")
sp500_latest$ticker <- substr(sp500_latest$`Copy All Below Symbol and Paste In TradingView`,1,nchar(sp500_latest$`Copy All Below Symbol and Paste In TradingView`)-1)
sp500_latest <- sp500_latest[, c(2,7)]
#write.csv(sp500_latest, file = 'msp500.csv')

folder <- "comp_funda"

if (file.exists(folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder)
  
}

new1.function <- function(start_year, end_year)
{
  for(i in start_year:end_year)
  {
    file_name <- paste('comp_funda_',i,'.csv',sep='')
    all_year <- read.csv(file_name)
    wdf <- sqldf('SELECT sp.ticker, f.tic, f.gvkey FROM sp500_latest as sp JOIN all_year as f ON sp.ticker = f.tic')
    file_name <- paste('comp_funda/a_',i,'.csv',sep='')
    write.csv(wdf, file = file_name, append = TRUE)
  }
}

new1.function(1990,2022)


file_paths <- fs::dir_ls("comp_funda")
file_contents <- list()
for(i in seq_along(file_paths)){
  file_contents[[i]] <- read.csv(file = file_paths[[i]])
}
file_contents <- setNames(file_contents, file_paths)
merged <- Reduce(function(x,y) merge(x, y, by = 'ticker', all.x = TRUE, all.y = TRUE),file_contents)
merged <- merged[, c(seq(1, 100, by = 3))]
colnames(merged) <-  c('ticker',1990:2022)
write.csv(merged, file = 'Final_Data_temp.csv')

