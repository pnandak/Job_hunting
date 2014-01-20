
#���Ƽ����Ƽ�ϵͳ

#1ץȡ����
#2����ģ��
#3�������ƶ�
#4���ͽ��


#1ץȡ����
#library(RMySQL)
#con <- dbConnect(MySQL(),user="dcshallot",password="1m1nd1",dbname="dbh57f6095rv6n96")
#jblist <- dbGetQuery(con, "select * from ods_datanalyst_zhaopin where date > CURRENT_DATE-3")
#dccv <- dbGetQuery(con, "select * from ods_mycv where id= 1")
#dict1 <- dbGetQuery(con, "select * from dim_zhaopin_term1")
#dbDisconnect(con); rm(con)
#jblist <- jblist[ which( jblist$content != 'NA' ) , ]
#txt <- c( jblist$content ,dccv[1,2]);  #�ı����һ�������Լ��ļ���
library(RODBC)
con <- odbcConnect( "aliecs" , uid ="dcshallot", pwd = "1m1nd1" )
jblist <- sqlQuery(con, "select * from ods_datanalyst_zhaopin where date > CURRENT_DATE-3")
dccv <- sqlQuery(con, "select * from ods_mycv where id= 1")
dict1 <- sqlQuery(con, "select * from dim_zhaopin_term1")
close(con); rm(con)

#2����ģ��
library(Rwordseg)
#insertWords(dict1$wd); rm(dict1)
wsg <- segmentCN( txt, nature=TRUE)
wsg <- as.character( wsg )

library(tm)
corp <- Corpus(  VectorSource(wsg))
corp <- tm_map( corp , tolower) #��Сд�л�,toupper,tolower
corp <- tm_map( corp , stripWhitespace) #�������Ŀո�ѹ��Ϊ����
corp <- tm_map( corp ,removePunctuation) #reduce������
dtm <- DocumentTermMatrix( corp , control = list(
  weighting = weightTfIdf, #weightSMART
  wordLengths = c (2, Inf ), #ֻ����һ�εĴʸɵ�
  removePunctuation = TRUE,
  removeNumbers=TRUE
))


cor <- as.matrix( dissimilarity(dtm, method = 'cosine') )
jb.select <-  cbind.data.frame( jblist[,c(2,3,9)], cor[-37,37])
