
#相似简历推荐系统

#1抓取数据
#2调用模型
#3计算相似度
#4推送结果


#1抓取数据
#library(RMySQL)
#con <- dbConnect(MySQL(),user="dcshallot",password="1m1nd1",dbname="dbh57f6095rv6n96")
#jblist <- dbGetQuery(con, "select * from ods_datanalyst_zhaopin where date > CURRENT_DATE-3")
#dccv <- dbGetQuery(con, "select * from ods_mycv where id= 1")
#dict1 <- dbGetQuery(con, "select * from dim_zhaopin_term1")
#dbDisconnect(con); rm(con)
#jblist <- jblist[ which( jblist$content != 'NA' ) , ]
#txt <- c( jblist$content ,dccv[1,2]);  #文本最后一列是我自己的简历
library(RODBC)
con <- odbcConnect( "aliecs" , uid ="dcshallot", pwd = "1m1nd1" )
jblist <- sqlQuery(con, "select * from ods_datanalyst_zhaopin where date > CURRENT_DATE-3")
dccv <- sqlQuery(con, "select * from ods_mycv where id= 1")
dict1 <- sqlQuery(con, "select * from dim_zhaopin_term1")
close(con); rm(con)

#2调用模型
library(Rwordseg)
#insertWords(dict1$wd); rm(dict1)
wsg <- segmentCN( txt, nature=TRUE)
wsg <- as.character( wsg )

library(tm)
corp <- Corpus(  VectorSource(wsg))
corp <- tm_map( corp , tolower) #大小写切换,toupper,tolower
corp <- tm_map( corp , stripWhitespace) #将连续的空格压缩为单个
corp <- tm_map( corp ,removePunctuation) #reduce标点符号
dtm <- DocumentTermMatrix( corp , control = list(
  weighting = weightTfIdf, #weightSMART
  wordLengths = c (2, Inf ), #只出现一次的词干掉
  removePunctuation = TRUE,
  removeNumbers=TRUE
))


cor <- as.matrix( dissimilarity(dtm, method = 'cosine') )
jb.select <-  cbind.data.frame( jblist[,c(2,3,9)], cor[-37,37])

