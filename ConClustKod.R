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

pred<-mpckm(clustData, 5, pos_join, neg_join, maxIter = 13)
pred
ConsClusData$mpckm_pred<-pred

pred<-ckmeans(clustData, 5, pos_join, neg_join, maxIter = 30)
pred
ConsClusData$ckmeans_pred<-pred


pred<-lcvqe(clustData, 5, pos_join, neg_join, maxIter = 13)
pred
ConsClusData$lcvqe_pred<-pred


pred<-ccls(clustData, 5, pos_join, neg_join, maxIter = 13)
pred
ConsClusData$ccls_pred<-pred




