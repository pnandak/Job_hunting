
library(XML)

#baidu
#获取职位名称和基本信息
title.data <- data.frame()
for (i in 1:20 ) {
  ul <- paste( "http://talent.baidu.com/baidu/web/templet1000/index/corpwebPosition1000baidu!getPostListByConditionBaidu?pc.currentPage=",
               i,"&pc.rowSize=10&releaseTime=0&keyWord=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&positionType=0&trademark=1&workPlaceCode=0%2F4%2F7%2F9&positionName=&recruitType=2&brandCode=1&searchType=1&workPlaceNameV=&positionTypeV=0&keyWordV=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90",sep="")
  url1<-htmlTreeParse( ul, useInternalNodes=TRUE,encoding='utf8' ) 
  #标题
  o <- getNodeSet(url1,"//body//table[@id = 'hrs_joblistTable']//tr/td[1]/a")
  title <- sapply( o, xmlValue , encoding="utf8")
  titlelink <- sapply( o, function(el) xmlGetAttr(el, "href"))
  titlelink <- paste( "http://talent.baidu.com",titlelink, sep="")
  #性质
  o <- getNodeSet(url1,"//body//table[@id = 'hrs_joblistTable']//tr/td[2]/font")
  pmt <- sapply( o , xmlValue, encoding="utf8")
  #日期
  o <- getNodeSet(url1,"//body//table[@id = 'hrs_joblistTable']//tr/td[4]")
  date <- sapply( o, xmlValue)
  date <- gsub( "[[:blank:]]|\r\n", "", date)
  
  rm(o)
  tt <- as.data.frame( cbind( title, pmt, date, titlelink ))
  tt$location <- "北京"
  title.data <- rbind( title.data, tt)
}
rm( pmt,  date,  title, titlelink, ul, url1)

#过滤和排重
title.data <- unique(title.data)
title.data <- title.data[ grep( "数据|分析|挖掘" , title.data$title) , ]

#获取职位内容数据
lk <- as.character(title.data$titlelink)
jd.all <- data.frame()
for ( i in 1: length(lk))  {
  titlelink <- lk[i]
  url1<-htmlTreeParse( titlelink, useInternalNodes=TRUE,encoding='utf8' ) 
  o <- getNodeSet( url1, "//body//dl[@class = 'hrs_jobDuty']/div")
  duty <- sapply( o, xmlValue, encoding ='utf8')
  duty <- gsub( "\r|\n", " ", duty)
  duty <- gsub( "[[:space:]]+" , " ", duty)
  o <- getNodeSet( url1, "//body//dl[@class = 'hrs_jobRequire']/div")
  reqr <- sapply(o, xmlValue, encoding = 'utf8')
  reqr <- gsub( "[[:blank:]]+|\r|\n" , " ", reqr)
  o <- getNodeSet( url1, "//body//dl[@class = 'hrs_jobInfo']/dd[1]")
  dept <- sapply(o, xmlValue)
  jd <- as.data.frame( cbind( titlelink,  dept, duty, reqr) )
  jd.all <- rbind(jd.all, jd)
}

#数据拼装
data <- merge( title.data, jd.all, by ="titlelink")
data$ch <- "baidu"
data <- data[,-1]
data <- data[ order( as.Date(data$date) , decreasing = T) , ]

#存储和结束
library(RODBC)
con <- odbcConnect("aliyun", uid="dcshallot", pwd="43shallot" ) 
sqlSave( con, data, tablename = "ods_datanalyst_bat", append = T, rownames = F, addPK = FALSE)
close(con)
rm(list=ls());


#腾讯
#获取职位名称和基本信息
title.data <- data.frame()
for (i in 1:25 ) {
  ul <- paste( "http://hr.tencent.com/position.php?keywords=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&tid=0&lid=2218&start=",
               (i-1)*10,"#a",sep="")
  url1<-htmlTreeParse( ul, useInternalNodes=TRUE,encoding='utf8' ) 
  #标题
  o <- getNodeSet(url1,"//body//td[@class = 'l square']/a")
  title <- sapply( o, xmlValue , encoding="utf8")
  titlelink <- sapply( o, function(el) xmlGetAttr(el, "href"))
  titlelink <- paste( "http://hr.tencent.com/",titlelink, sep="")
  #性质
  o <- getNodeSet(url1,"//body//tr[@class = 'even' or 'odd']/td[2]")
  pmt <- sapply( o , xmlValue) [c(-1,-12)]
  #日期
  o <- getNodeSet(url1,"//body//tr[@class = 'even' or 'odd']/td[5]")
  date <- sapply( o , xmlValue) [c(-1,-12)]
  date <- gsub( "[[:blank:]]|\r\n", "", date)
  rm(o)
  tt <- as.data.frame( cbind( title, pmt, date, titlelink ))
  tt$location <- "深圳"
  if ( nrow(tt) ==10) 
    { title.data <- rbind( title.data, tt) } else {break}
}
rm( pmt,  date,  title, titlelink, ul, url1)

#过滤和排重
title.data <- unique(title.data)
title.data <- title.data[ grep( "数据|分析|挖掘" , title.data$title) , ]

#获取职位内容数据
lk <- as.character(title.data$titlelink)
jd.all <- data.frame()
for ( i in 1: length(lk))  {
  titlelink <- lk[i]
  url1<-htmlTreeParse( titlelink, useInternalNodes=TRUE,encoding='utf8' ) 
  
  o <- getNodeSet( url1, "//body//ul[@class = 'squareli']")
  duty <- sapply( o, xmlValue, encoding ='utf8')[1]
  reqr <- sapply(o, xmlValue, encoding = 'utf8')[2]
  dept <- "NA"
  jd <- as.data.frame( cbind( titlelink,  dept, duty, reqr) )
  jd.all <- rbind(jd.all, jd)
}

#数据拼装
data <- merge( title.data, jd.all, by ="titlelink")
data$ch <- "qq"
data <- data[,-1]
data <- data[ order( as.Date(data$date) , decreasing = T) , ]

#存储和结束
library(RODBC)
con <- odbcConnect("aliyun", uid="dcshallot", pwd="43shallot" ) 
sqlSave( con, data, tablename = "ods_datanalyst_bat", append = T, rownames = F, addPK = FALSE)
close(con)
rm(list=ls());


