#招聘数据的分析

library(RODBC)
con <- odbcConnect("aliyun", uid="dcshallot", pwd="1m1nd1" ) 
timeline <- sqlQuery(con, "select date,dbid from ods_datanalyst_zhaopin")
close(con)

timeline <- aggregate( dbid ~ date , data= timeline, length)
t <- ts(timeline[,2],start=as.Date(timeline[1,1] , "%Y-%m-%d" ) )
plot(timeline , type="l", lty=1 )

