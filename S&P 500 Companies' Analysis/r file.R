
setwd("~/IA")

install.packages("dbplyr")
install.packages("dplyr")
install.packages("readxl")
install.packages('tidyverse')
install.packages('fs')

library(RPostgres)
library(dplyr)
library(dbplyr)
library(readxl)
library(fs)
library(tidyverse)

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='sesh')

res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='comp'
                   order by table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
write.csv(data, file = 'List_of_datasets.csv')

res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='comp'
                   and table_name='funda'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
write.csv(data, file = 'List_of_column_headers.csv')

sp500_latest <- read_excel("SP-500-Companies-List-1.xlsx")
sp500_latest$ticker <- substr(sp500_latest$`Copy All Below Symbol and Paste In TradingView`,1,nchar(sp500_latest$`Copy All Below Symbol and Paste In TradingView`)-1)

new.function <- function(start_year, end_year)
{
  for(i in start_year : end_year)
  {
    command = paste("select DISTINCT tic, conm, gvkey, cik, cusip, fyear from comp.funda where fyear =", i, sep=" ")
    res <- dbSendQuery(wrds, command)
    data <- dbFetch(res, n=-1)
    dbClearResult(res)
    file_name = paste('comp_funda_',i,'.csv',sep="")
    write.csv(data, file = file_name)
    
  }
}

new.function(1990,2022)

new1.function <- function(start_year, end_year)
{
  for(i in start_year:end_year)
  {
    file_name <- paste('comp_funda_',i,'.csv',sep='')
    all_year <- read.csv(file_name)
    all_year$ticker <- all_year$tic
    common <- merge(sp500_latest, all_year, by = 'ticker')
    drop <- c("Symbol","Copy All Below Symbol and Paste In TradingView",",","tic")
    common <- common[,!(names(common) %in% drop)]
    file_name <- paste('common_latest_and_',i,'.csv',sep='')
    write.csv(common, file = file_name)
  }
}
new1.function(1990,2022)

file_paths <- fs::dir_ls("Common Data")
file_contents <- list()
for(i in seq_along(file_paths)){
  file_contents[[i]] <- read.csv(file = file_paths[[i]])
}

file_contents <- setNames(file_contents, file_paths)

merged <- Reduce(function(x,y) merge(x, y, by = c('ticker', 'gvkey', 'cik', 'cusip'), all.x = TRUE, all.y = TRUE),
       file_contents)
merged <- merged[ , colSums(is.na(merged)) < nrow(merged)]  
merged <- na.omit(merged)
merged <- merged[, c(1,2,3,4,7,10)]
colnames(merged)[5] <- "CompanyName1"
colnames(merged)[6] <- "CompanyName2"
write.csv(merged, file = 'Final_Data.csv')
