library(readxl)
library(sqldf)
install.packages('conclust')
library(conclust)
ConsClusData <- read_excel("ConsClusOrnekVeri.xlsx", 
                                sheet = "Data")
View(ConsClusData)

posLink <- read_excel("ConsClusOrnekVeri.xlsx", 
                           sheet = "Pozitif")

negLink <- read_excel("ConsClusOrnekVeri.xlsx", 
                      sheet = "Negatif")

ConsClusData$ID<-1:600


join_string1 <- "select
                posLink.*
              , ConsClusData.ID as ID1
              from posLink
                join ConsClusData
                on posLink.item1 = ConsClusData.urunkod"
ara_join1 <- sqldf(join_string1, stringsAsFactors = FALSE)

join_string2 <- "select
                ara_join1.ID1
              , ConsClusData.ID as ID2
              from ara_join1
                join ConsClusData
                on ara_join1.item2 = ConsClusData.urunkod"
pos_join <- sqldf(join_string2, stringsAsFactors = FALSE)

join_string1 <- "select
                negLink.*
              , ConsClusData.ID as ID1
              from negLink
                join ConsClusData
                on negLink.item1 = ConsClusData.urunkod"
ara_join1 <- sqldf(join_string1, stringsAsFactors = FALSE)

join_string2 <- "select
                ara_join1.ID1
              , ConsClusData.ID as ID2
              from ara_join1
                join ConsClusData
                on ara_join1.item2 = ConsClusData.urunkod"
neg_join <- sqldf(join_string2, stringsAsFactors = FALSE)
rm(ara_join1)

clustData<-ConsClusData[,c(2:35)]

pred<-mpckm(clustData, 15, pos_join, neg_join, maxIter = 13)
pred
ConsClusData$mpckm_pred<-pred

pred<-ckmeans(clustData, 15, pos_join, neg_join, maxIter = 30)
pred
ConsClusData$ckmeans_pred<-pred


pred<-lcvqe(clustData, 15, pos_join, neg_join, maxIter = 13)
pred
ConsClusData$lcvqe_pred<-pred


pred<-ccls(clustData, 15, pos_join, neg_join, maxIter = 13)
pred
ConsClusData$ccls_pred<-pred

#----- Frequency of Clusters 

table(ConsClusData$mpckm_pred)
table(ConsClusData$ckmeans_pred)
table(ConsClusData$lcvqe_pred)
table(ConsClusData$ccls_pred)

#----- Checking Constraint Satisfaction

join4satis_pos <- "select
                a.ID1
              , a.ID2, b.lcvqe_pred as lclust1,c.lcvqe_pred as lclust2,
              b.ckmeans_pred as kclust1,c.ckmeans_pred as kclust2
              from pos_join as a
                join ConsClusData as b 
                on a.ID1 = b.ID 
                join ConsClusData as c 
                on a.ID2 = c.ID 
"
satisfyJoinpos <- sqldf(join4satis_pos, stringsAsFactors = FALSE)
a<-satisfyJoinpos[satisfyJoinpos$lclust1!=satisfyJoinpos$lclust2,]
aa<-satisfyJoinpos[satisfyJoinpos$kclust1!=satisfyJoinpos$kclust2,]
join4satis_neg <- "select
                a.ID1
              , a.ID2,  b.lcvqe_pred as lclust1,c.lcvqe_pred as lclust2,
              b.ckmeans_pred as kclust1,c.ckmeans_pred as kclust2
              from neg_join as a
                join ConsClusData as b 
                on a.ID1 = b.ID 
                join ConsClusData as c 
                on a.ID2 = c.ID 
"
satisfyJoinneg <- sqldf(join4satis_neg, stringsAsFactors = FALSE)
b<-satisfyJoinneg[satisfyJoinneg$lclust1==satisfyJoinneg$lclust2,]
bb<-satisfyJoinneg[satisfyJoinneg$kclust1==satisfyJoinneg$kclust2,]









