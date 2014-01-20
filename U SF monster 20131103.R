#抓monster最近30天的data analyst

library(XML)

ul <- "http://jobsearch.monster.com/search/data-analyst_5?lv=Experienced-Non-Manager&tm=Last-30-days&pg=2"

url1<-htmlTreeParse( ul, useInternalNodes=TRUE,encoding='utf8' ) #encoding

u <- getNodeSet( url1, "//div[@id = 'primaryResults']//tr ")

s<-xmlElementsByTagName( u[[2]] ,'td')  #2-4是正文

r <- getNodeSet( u[[1]], "//div[@class = 'jobTitleContainer']/a")

sapply( u[[2]][3] , xmlValue)

link <- sapply ( o, function(el) xmlGetAttr(el, "href"))


o <- getNodeSet( url1, "//div[@id = 'primaryResults']//div[@class = 'companyContainer']//a[2]")

company <- sapply( o, xmlValue)

comp.link <- sapply ( o, function(el) xmlGetAttr(el, "href"))

o <- getNodeSet( url1, "//div[@id = 'primaryResults']//div[@class = 'fnt13']")

salary <- sapply( o, xmlValue)

o <- getNodeSet( url1, "//div[@id = 'primaryResults']//div[@class = 'fnt20']" )

posttime <- sapply( o , function(el) gsub( "[[:space:]]", "", xmlValue(el) ) )

o <- getNodeSet( url1, "//div[@id = 'primaryResults']//div[@class = 'jobLocationSingleLine']//a" )

location <- sapply( o, xmlValue)