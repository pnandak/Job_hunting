
library(XML)

#智联招聘
#获取职位名称和基本信息
title.data <- data.frame()

for (i in 1:30 ) {
ul <- paste( "http://sou.zhaopin.com/jobs/searchresult.ashx?jl=%E5%8C%97%E4%BA%AC%2B%E4%B8%8A%E6%B5%B7%2B%E5%B9%BF%E5%B7%9E%2B%E6%B7%B1%E5%9C%B3&kw=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&sm=0&p=",i,sep="")
url1<-htmlTreeParse( ul, useInternalNodes=TRUE,encoding='utf8' ) 
#标题
o <- getNodeSet(url1, 
  "//body//div[@class = 'search-result-cont']/table//td[@class = 'Jobname']/a")
title <- sapply( o, xmlValue)
titlelink <- sapply( o, function(el) xmlGetAttr(el, "href"))
#公司
o <- getNodeSet( url1, 
  "//body//div[@class = 'search-result-cont']/table//td[@class = 'Companyname']/a")
company <- sapply( o , xmlValue)
companylink <- sapply( o, function(el) xmlGetAttr(el, "href"))
#工作地点
o <- getNodeSet( url1, 
  "//body//div[@class = 'search-result-cont']/table//td[@class = 'Companyaddress']")
location <- sapply( o, xmlValue)
location <- gsub( "[[:space:]]", "", location )
location <- substr(location, 1,2)
#发布日期
o <- getNodeSet( url1, 
  "//body//div[@class = 'search-result-cont']/table//td[@class = 'releasetime']")
date <- sapply( o, xmlValue)
date <- gsub( "[[:space:]]", "", date )
rm(o)
tt <- as.data.frame( cbind( title, company, date, location, titlelink,  companylink ))
title.data <- rbind( title.data, tt)
}
rm( company, companylink, date, location, title, titlelink, ul, url1)
#根据题目过滤
title.data <- title.data[ grep( "数据分析" , title.data$title) , ]

#获取职位内容数据
lk <- as.character(title.data$titlelink)

jd.all <- data.frame()
for ( i in 1: length(lk))  {
  titlelink <- lk[i]
  url1<-htmlTreeParse( titlelink, useInternalNodes=TRUE,encoding='utf8' ) 
  o <- getNodeSet( url1, "//body//div[@class = 'terminalpage-content']")
  content <- sapply( o, xmlValue)
  content <- gsub( "[[:space:]]+", " ", content)
  if ( length(content) ==0)   content <- "NA"
  o <- getNodeSet( url1, "//body//table[@class = 'terminalpage-table']/tr[3]/td[2]")
  vol <- sapply(o, xmlValue)
  if ( length(vol) ==0)   vol <- "NA"
  o <- getNodeSet( url1, "//body//table[@class = 'terminalpage-table']/tr[4]/td[2]")
  kind <- sapply(o, xmlValue)
  if ( length(kind) ==0)   kind <- "NA"
  o <- getNodeSet( url1, "//body//table[@class = 'terminalpage-table']/tr[5]/td[2]/a[1]")
  ind1 <- sapply(o , xmlValue)
  if ( length(ind1) ==0)   ind1 <- "NA"
  o <- getNodeSet( url1, "//body//table[@class = 'terminalpage-table table-margin']/tr[1]/td[2]")
  jd <- as.data.frame( cbind( titlelink,  vol, kind, ind1, content) )
  jd.all <- rbind(jd.all, jd)
}

#数据拼装
data <- merge( title.data, jd.all, by ="titlelink")
data$ch <- "zhaopin.com"

#排重
data$id <- 1:nrow(data)
l <- aggregate( data$id, by=list(data$title, data$company), FUN=min) #排重后索引
l <- sort( l[, ncol(l)] )
data <- data[l, ]
data <- data[,-12] #除掉辅助变量id
data <- data[, c( 2,3,4,5,7,8,9,10,11,1,6) ]

library(RODBC)
con <- odbcConnect("aliyun", uid="dcshallot", pwd="43shallot" ) 
sqlSave( con, data, tablename = "ods_datanalyst_zhaopin", append = T, rownames = F, addPK = FALSE)
close(con)
rm(list=ls());


#前程无忧
#获取职位名称和基本信息
title.data <- data.frame()
for (i in 1:30 ) {
  ul <- paste( "http://search.51job.com/jobsearch/search_result.php?fromJs=1&jobarea=010000%2C020000%2C030200%2C040000%2C00&district=000000&funtype=0000&industrytype=00&issuedate=9&providesalary=99&keyword=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&keywordtype=1&curr_page=",
               i,"&lang=c&stype=1&postchannel=0000&workyear=99&cotype=99&degreefrom=99&jobterm=01&lonlat=0%2C0&radius=-1&ord_field=0&list_type=0&fromType=14",sep="")
  url1<-htmlTreeParse( ul, useInternalNodes=TRUE,encoding='gb2312' ) 
  #标题
  o <- getNodeSet(url1,"//body//td[@class='td1']/a")
  title <- sapply( o, xmlValue , encoding="utf8")
  titlelink <- sapply( o, function(el) xmlGetAttr(el, "href"))
  #公司
  o <- getNodeSet( url1, "//body//td[@class ='td2']/a")
  company <- sapply( o , xmlValue, encoding="utf8")
  companylink <- sapply( o, function(el) xmlGetAttr(el, "href"))
  #工作地点
  o <- getNodeSet( url1, "//body//td[@class ='td3']/span")
  location <- sapply( o, xmlValue, encoding="utf8")
  location <- substr(location, 1,2)
  #发布日期
  o <- getNodeSet( url1, "//body//td[@class ='td4']/span")
  date <- sapply( o, xmlValue)
  
  rm(o)
  tt <- as.data.frame( cbind( title, company, date, location, titlelink,  companylink ))
  title.data <- rbind( title.data, tt)
}
rm( company, companylink, date, location, title, titlelink, ul, url1)
#根据题目过滤
title.data <- title.data[ grep( "数据分析" , title.data$title) , ]

#排重
title.data$id <- 1:nrow(title.data)
l <- aggregate( title.data$id, by=list(title.data$title, title.data$company), FUN=min) #排重后索引
l <- sort( l[, ncol(l)] )
title.data <- title.data[l, ]
title.data <- title.data[,-7] #除掉辅助变量id

#获取职位内容数据
lk <- as.character(title.data$titlelink)

jd.all <- data.frame()
for ( i in 1: length(lk))  {
  titlelink <- lk[i]
  url1<-htmlTreeParse( titlelink, useInternalNodes=TRUE,encoding='gb2312' ) 
  o <- getNodeSet( url1, 
                   "//body//td[@class = 'txt_4 wordBreakNormal job_detail ']/div")
  content <- sapply( o, xmlValue, encoding ='utf8')
  content <- gsub( "[[:space:]]+", " ", content)
  if ( length(content) ==0)   content <- "NA"
  o <- getNodeSet( url1, 
                   "//body//div[@class = 's_txt_jobs']/table[1]/tr[3]/td")
  detail <- sapply(o, xmlValue, encoding = 'utf8') [2]
  detail <- gsub( "[[:blank:]]" , "", detail)
  a <- regexpr("公司规模：.*?人" , detail)
  vol <- substr(detail ,a[[1]]+5, a[[1]]+attr(a,'match.length')-1)
  if ( length(vol) ==0 )   vol <- "NA"
  a <- regexpr("公司性质：.*?公司" , detail)
  kind <- substr(detail ,a[[1]]+5, a[[1]]+attr(a,'match.length')-3)
  if ( length(kind) ==0)   kind <- "NA"  
  a <- regexpr("公司行业：.*?公司" , detail)
  ind1 <- substr(detail ,a[[1]]+5, a[[1]]+attr(a,'match.length')-3)
  if ( length(ind1) ==0)   ind1 <- "NA"  
  jd <- as.data.frame( cbind( titlelink,  vol, kind, ind1, content) )
  jd.all <- rbind(jd.all, jd)
}

#数据拼装
data <- merge( title.data, jd.all, by ="titlelink")
data$ch <- "51job.com"
data <- data[, c( 2,3,4,5,7,8,9,10,11,1,6)]

#存储和结束
library(RODBC)
con <- odbcConnect("aliyun", uid="dcshallot", pwd="43shallot" ) 
sqlSave( con, data, tablename = "ods_datanalyst_zhaopin", append = T, rownames = F, addPK = FALSE)
close(con)
rm(list=ls());


