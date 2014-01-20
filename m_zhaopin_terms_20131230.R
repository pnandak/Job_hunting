
#短语发现模型第一版 20131229
#数据分析师工作中的专业术语

#从职位内容中按照临近关系把词组选出
#根据2种语法结构筛选术语
#根据频数筛选术语
#人工挑选筛出的术语，导入数据库

# 获取原始数据
library(RODBC)
con <- odbcConnect( "aliecs" , uid ="dcshallot", pwd = "1m1nd1" )
txt.org <- sqlQuery(con, "select content from ods_datanalyst_zhaopin limit 1000") 
close(con); rm(con)
txt <- as.character( txt.org[1:500,])  #一次引入样本不应超过1000个否则1核处理困难

#分词过滤清洗
txt <- tolower(txt)
txt <- gsub( "[[:digit:]]", " ", txt)
txtplain <- paste( txt, collapse= " ")
library(Rwordseg)
uninstallDict()
seg <- segmentCN( txt , nature=T )  #带词性的分词


####根据词性对应的实际词确定语法过滤规则
#x <- segmentCN( as.character(txt.org[,1]), nature=T)
#x <- unlist(x)
#x <-as.data.frame( cbind( x, names(x)))
#y <- unique( as.character( x[,2]) )
#b <- data.frame()
#for ( i in 1: length(y) )  {
#  z <- paste( unique( as.character( x[ x$V2 == y[i] , 1] ) )[1:20], collapse="," )
#  a <- cbind.data.frame( y[i] , z, stringsAsFactors = F)
#  b <- rbind(a, b)
#} 
#write.csv(b, "d:/d.csv");rm(x,y,b,z,a)####需要概括N=, A=, V=

#词数据表
x <- unlist(seg)
y <- names(x)
worddata <- cbind.data.frame( x, y, stringsAsFactors = F )
worddata <- unique(worddata)
names(worddata) <- c("w","pt") ;rm(x,y)
worddata <- worddata[ !grepl( "uj|p|ug|c", worddata$pt) , ] #词性筛选
worddata$id <- 1:nrow(worddata) #排重
l <- aggregate( worddata$id, by=list( worddata$w), FUN=min) #排重后索引
l <- sort( l[, ncol(l)] )
worddata <- worddata[l, ]
worddata <- worddata[,-3] #除掉辅助变量id
worddata$wfrq <- sapply( worddata$w, function(x) length( gregexpr(x, txtplain)[[1]] ))
#生成术语候选集
require(tau)
terms <- textcnt(seg, split = " ", method = "string", n = 2) #2段切词
terms <- terms[ terms> 3 ]  #术语词频筛选
#生成术语筛选表
tname <- names(terms)
w1 <- sapply( tname, function(x) strsplit( x, " ") [[1]] [1])
w2 <- sapply( tname, function(x) strsplit( x, " ") [[1]] [2])
termdata <- cbind.data.frame( tname, w1, w2, stringsAsFactors = F )
termdata$tfrq <- sapply( termdata[,1], function(x) length( gregexpr( gsub(" " ,"", x) , txtplain)[[1]] ))
termdata <- termdata[ termdata$tfrq > 3,]
#术语+词 
data <- merge( termdata, worddata, by.x="w1", by.y="w")
data <- merge(data, worddata, by.x="w2", by.y="w")
data <- data[, c(3,4,5,7,2,6,1,8)]
names(data)[3:8] <- c("ptl","ptr","wl","frql","wr","frqr")
#tdata <- tdata[ tdata$tfreq > quantile(tdata$tfreq, prob = .25 ) , ] #根据词频筛选

tdata <- data
names(tdata)
tdata$intinfo <- tdata$tfrq/tdata$frql/tdata$frqr #互信息
tdata$termhood <- tdata$tfrq*( length(txt)-tdata$tfrq)/length(txt) #termhood

write.csv( tdata, "d:/term.csv", row.names=F) 
#导出，人工审阅之后再读进来，写进数据库
#######################################################################
model.data <- read.csv("d:/term.csv",header=T)
#存储
library(RODBC)
con <- odbcConnect( "aliecs" , uid ="dcshallot", pwd = "1m1nd1" )
#sqlQuery(con, "TRUNCATE dbh57f6095rv6n96.dim_zhaopin_term1")
sqlSave(con, model.data , tablename = "ods_termstudy_data1", append = T, rownames = F, addPK = FALSE)
close(con)
#术语学习模型研究
names(model.data)
model.data$y <- sapply( model.data$y, function(x) if (x>0) 1 else 0)
logi <- glm( y~ ptl*ptr, model.data, family="binomial")
b <- step( logi)
summary(b)
D D D
ye1 <- model.data[ model.data$y=="1", c(3,4)]