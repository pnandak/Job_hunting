
#BM25 similarity simulation
#http://lucene.apache.org/core/4_0_0/core/org/apache/lucene/search/similarities/BM25Similarity.html

#data preparing 


library(RODBC)
con <- odbcConnect( "aliecs" , uid ="dcshallot", pwd = "1m1nd1" )
jb <- sqlQuery(con, "select content from ods_datanalyst_zhaopin limit 10,100") 
close(con); rm(con)

jb <- as.character( jb$content ) 
jb <- gsub( "[^0-9A-Za-z\u3007\u4E00-\u9FCB\uE815-\uE864]" ," " , as.character(jb) )
jb <- jb[ jb != ""]
q.all <- c("统计","数据","分析")
#BM25
k1 <- 1.2 ; b <- 0.75 #lucence default
out <- sim.bm25( jb, q.all)

#the function
sim.bm25 <- function( d, q) { #input 文档和查询都是vector
  #data restucture
  D.all <- cbind.data.frame( 1:length(d), as.character(d) , stringsAsFactors = F)
  names(D.all) <- c("id", "txt")
  Q <- q
  #global vars
  N <- nrow(D.all) ; avgdl <- mean( nchar(D.all[,2]))
  #query vars
  idf.qi <- sapply( Q, function(x) 
    log( 1+(N - length(grep(x,D.all[,2])) +0.5)/(length(grep(x, D.all[,2])) + 0.5 )) ) #adj by 1
  #doc and query vars
  D.abs <- sapply( D.all[,2], nchar)
  f.qiD <- matrix( , nrow=nrow(D.all), ncol = length(Q))
  for ( i in 1 : nrow(f.qiD) ) { #find freq of each qi in each doc
    for ( j in 1: ncol(f.qiD) ) {
      f.qiD[i,j] = if ( grepl(Q[j] , D.all[i,2])) { 
        length( gregexpr(Q[j], D.all[i,2])[[1]] )} else {0}
    }
  }
  tf.D <-  ( f.qiD * (k1+1))/( f.qiD + k1*(1-b+b*D.abs/avgdl))
  #output dataframe with id and score
  score <- cbind.data.frame(D.all[,1], tf.D %*% idf.qi)
  names(score) <- c("id","score")
  score # the final output
}

