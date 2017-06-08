#####keep in mind: which returns the row number for selecting;
#####keep in mind: sum only add TRUE itmes
#####keep in mind: length calcualte all the item in the list whatever it is 
## day1_no_na : dataset after processing "" missing value; day1_1: after handling track time "" and 0
## day1_2: dataset after processing songs name; day1_3: dataset after processing singers

################################## validate dataset ###################################
## we add fill=TRUE since the first two rows don't have elements.It will cause error
day1 <- read.table("20170301_play.log",fill = TRUE, sep = "\t", quote = "", stringsAsFactors = TRUE)
dim(day1) #3423005 rows and 9 cols
day2 <- read.table("20170302_1_play.log",fill = TRUE, sep = "\t", quote = "", stringsAsFactors = TRUE)
dim(day2) #2453733 9
day3 <- read.table("20170303_1_play.log",fill = TRUE, sep = "\t", quote = "", stringsAsFactors = TRUE)
dim(day3) #1853015 9
#day4 <- read.table("20170304_1_play.log",fill = TRUE, sep = "\t", quote = "", stringsAsFactors = TRUE)
#dim(day4) #1710074 9
#day5 <- read.table("20170305_1_play.log",fill = TRUE, sep = "\t", quote = "", stringsAsFactors = TRUE)
#dim(day5) #1608957 9
#day6 <- read.table("20170306_1_play.log",fill = TRUE, sep = "\t", quote = "", stringsAsFactors = TRUE)
#dim(day6) #1352373 9
#day7 <- read.table("20170307_1_play.log",fill = TRUE, sep = "\t", quote = "", stringsAsFactors = TRUE)
#dim(day7) #1289483 9
# based on observation, the last col representing songs type doesn't make sense
# so I decide to delete the last col
table(day1$V9)
table(day1$V4)
table(day2$V9)
day1=day1[,-9] #8 cols
day2=day2[,-9] #8 cols
day3=day3[,-9] #8 cols
#day4=day4[,-9]
#day5=day5[,-9]
#day6=day6[,-9]
#day7=day7[,-9]
colnames(day1) <- c("uid","os","rid","type","name","singer","play_time","track_time")
colnames(day2) <- c("uid","os","rid","type","name","singer","play_time","track_time")
colnames(day3) <- c("uid","os","rid","type","name","singer","play_time","track_time")
#colnames(day4) <- c("uid","os","rid","type","name","singer","play_time","track_time")
#colnames(day5) <- c("uid","os","rid","type","name","singer","play_time","track_time")
#colnames(day6) <- c("uid","os","rid","type","name","singer","play_time","track_time")
#colnames(day7) <- c("uid","os","rid","type","name","singer","play_time","track_time")
colnames(day1)
head(day1)

################################ data exploration and preprocessing ##############################
## check missing values and process
# the original dataset doesn't conclude NA but " " values instead
# 'apply' returns # of missing values for each row, but 'sapply' is grouped by cols
# 1.check using 'apply' to see how many rows are having missing
row_na <- apply(day1_no_na,1,function(x) sum(x==""))
table(row_na)  # # of rows with NAs = 25+1455+811+9+65 
length(which(row_na!=0))/dim(day1)[1] # dim(day1)[1] means # of rows of day1, 0.00069 showing NA is a tiny part
# 2. we check using sapply to see if te distribution of NAs in each col
col_na <- sapply(day1,function(x) sum(x=="")) # play_time and track_time match the cell_na "2365"
col_na
# comparing results from row_na and col_na, just delete all the recordings with " "
# which(row_na!=0) returns row number
day1_no_na <- day1[-which(row_na!=0),]
dim(day1_no_na) #3420640
summary(day1_no_na) #actually we can use summary to get max and min and a lot of info
# after summary, still there are cells with useless values
# play$track time: 0; name:'unknown singer'; os:0...; rid: 0.......; so check by column names

## check songs name and standardize names
day1_no_na$name<-gsub('(-\\(.*\\))|(\\(.*\\))|(\\{.*\\})|(（.*）)', '', day1_no_na$name)
day1_no_na$name<-gsub('\\s*$','',day1_no_na$name)
day1_no_na$name[1:100]

length(grep('^[0-9]+$',day1_no_na$name))/dim(day1_no_na)[1] #grep(pattern, x), search for a particular pattern in each element of a vector x.
day1_no_na[grep('^[0-9]+$',day1_no_na$name),]
day1_1<-day1_no_na[-grep('^[0-9]+$',day1_no_na$name),]

length(grep('\\.+.*\\.+',day1_1$name))/dim(day1_1)[1] # 0.1%, so delete
day1_2<-day1_1[-grep('\\.+.*\\.+',day1_1$name),]
day1_2$name<-gsub('(\\(.*）)|(（.*\\))','',day1_2$name)

length(grep('\\s*(\\(|（).*',day1_2$name))/dim(day1_2)[1] # 0.2% delete
abc<-grep('\\s*(\\(|（).*',day1_2$name) #returns a vector
day_abc<-day1_2[abc,]
day1_2$name<-gsub('\\s*(\\(|（).*','',day1_2$name)

day1_2$name<-gsub('\\[.*\\]','',day1_2$name)  # change names with []suffix to ""

length(grep('【|】',day1_2$name))/dim(day1_2)[1]
abc1 <- grep('【|】',day1_2$name)
day_abc1 <- day1_2[abc1,]
day1_2$name<-gsub('【|】','',day1_2$name)
dim(day1_2) 
# day1_2 is the dataset after handling songs name

## check track time&play time and process
str(day1_2)
# "0" value still here, and track_time & play_time are all factors as checked 
#length(which(day1_no_na$track_time=="0 "))/dim(day1_no_na)[1] #or: sum(with(day1_no_na, track_time=="0 "))
# here notice: 11% of the data has been moved becuase of used track-time level
# It is necessary remove o values of track time, first convert it into numeric
is.data.frame(day1_no_na)
day_check <- day1_2
day_check$play_time <- as.numeric(day_check$play_time)
day_check$track_time <- as.numeric(day_check$track_time)
day1_2$track_time <- as.numeric(paste(day1_2$track_time)) # add pasteto keep numbers not changed
day1_2$play_time <- as.numeric(paste(day1_2$play_time))
str(day1_2)
head(day1_2)
day1_num_na <- sapply(day1_2, function(x) sum(is.na(x))) #45686 11266 
#till now, only cols playtime and tracktime have NA
#check NA generated from converting factor variables
sum(is.na(day1_2$track_time))/dim(day1_2)[1] # 11266/3406745=0.33% delete
day1_3 <- day1_2[-which(is.na(day1_2$track_time)),]
day1_3 <- day1_3[-which(is.na(day1_3$play_time)),]
day1_3 <- day1_3[-which(day1_3["track_time"]==0),] # remove track_time == 0 items  11% itmes
#(3363684-2969865)/3363684 
dim(day1_3) #3361059
# add a new col indicating play_track ratio od each recording
library(data.table)
day1_add <- as.data.table (day1_3)
day1_add <- day1_add[, played_ratio:=day1_3$play_time/day1_3$track_time]
head(day1_add)
dim(day1_add)
is.data.frame(day1_add)
summary(day1_add$played_ratio)
# select rows meet condition: play_ratio <= 1, they are meaningless
# presentation1
day1_add <- day1_add[which(played_ratio >= 0 & played_ratio <= 1),] #2688455
# presentation2: 
#day1_add <- subset(day1_add, played_ratio <= 1)
head(day1_add)
#till now, day1_add is the dataset afer dealing with play&rtrck time and generating played_ratio var

## process singer
day1_add$singer[1:100]
day1_add$singer<-gsub('\\s*$','',day1_add$singer)
day1_add$singer[1:100]
length(grep('未知歌手',day1_add$singer))/dim(day1_add)[1] #2% unknown singers, useless, delete
day1_add[grep('未知歌手',day1_add$singer),]
day1_add<-day1_add[-grep('未知歌手',day1_3$singer),]
day1_add
length(which(day1_add$singer==""))/dim(day1_add)[1] #0.07% delete
day1_add<-day1_add[-which(day1_add$singer==""),]
#day1_3[grep('网络歌手',day1_3$singer),]
#day1_3[which(day1_3$uid=="154408074 "),]$singer
day1_add$singer<-gsub('\\[.*\\]','',day1_add$singer)
#till now, the dataset has been processed on missing values, songs name standard, unknown singers and meaningless time


############################################ exploratory analysis #############################################
## check user behavior
unique(day1_add$uid) # totally 175196 diff user_id  << 3423005 total recordings
dim(day1_add)
length(which(table(day1_add$uid)>=0))/dim(day1_add)[1] #0.001294829
#unique(day2[,1]) # totally 84658 diff user_id
#unique(day3[,1]) # totally 66171 diff user_id
# unique(day1$uid) #or use this one
# group data by uid and order the group size
library(data.table)
day1_add <- data.table(day1_add)
is.data.table(day1_add)
day1_uid<- day1_add[, .N, by = uid]
day1_uid <- as.data.frame(day1_uid) # since sort can only be used for df
colnames(day1_uid) <- c("uid", "counts")
day1_sort_uid <- day1_uid[order(day1_uid$counts, decreasing = TRUE),]
day1_sort_uid[1:300,] 
length(which(day1_sort_uid$counts>=50))/dim(day1_sort_uid)[1]
dim(day1_sort_uid) #159127 unique users from total 3420640 nonmissingn recordings

## check os distribution and get distribution plot
# use datset after missing values manipulation day1_no_na
table(day1_no_na$os)
day1_no_na <- as.data.table(day1_no_na)
day1_no_na$os =='ar '
day1_os <- day1_no_na[day1_no_na$os=='ar ' | day1_no_na$os=='ip ', ]
day1_os$os <- factor(day1_os$os)
table(day1_os$os) # ar      ip  :2920033  500607
head(day1_os)
# day1_os <- day1_no_na[day1_no_na$os %in% select, ]
# table(day1_os$os)
# consider os distribution in unique uid condition:
day1_add1 <- as.data.frame(day1_add)
day1_os1 <- merge(x=day1_sort_uid, y=day1_add1[,c(1,2)], by="uid")
day1_os1[1:20,]
day1_os_unique <- day1_os1[!duplicated(day1_os1$uid),]
head(day1_os_unique)
# drop levels of os again
select=c("ar ", "ip ")
day1_os_unique <- day1_os_unique[day1_no_na$os %in% select, ]
day1_os_unique$os <- factor(day1_os_unique$os)
table(day1_os_unique$os)#ar     ip  :13869938  20482 = 159127 unique uid
day1_os_unique <- na.omit(day1_os_unique)

## stacked plot for os distribution
ooss <- matrix(c(0.146, 0.129, 0.854, 0.871), ncol=2, byrow=TRUE)
colnames(ooss) <- c("songs", "uid")
rownames(ooss) <- c("iphone", "Android")
ooss<-as.table(ooss)
colors<-c("blue", "lightgreen")
plot_ooss <- barplot(ooss, col=colors, width=0.25, xlim=(0:1),xlab="classification", main="distribution of 'iphone' and 'adroid' users",
        cex.main=0.9)
legend("topright", rownames(ooss), fill=colors)
ypos<-apply(ooss,2, cumsum)
ypos<-ypos-ooss/2
ypos<-t(ypos)
text(plot_ooss, ypos, ooss)

## preparing dataset for clustering:
# including uid, total amount of songs played by each user, os 
head(day1_os_unique)
head(day1_add)
summary(day1_add$played_ratio)
library(data.table)
day1_add <- day1_add[, ratings:=ifelse(played_ratio<=0.2,'1',ifelse(played_ratio<=0.4,'2',ifelse(played_ratio<=0.6,'3',ifelse(played_ratio<=0.8,'4','5'))))]
day1_add$ratings <- as.numeric(paste(day1_add$ratings))
dim(day1_add)
library(plyr)
rate_mean <- ddply(day1_add, .(uid), summarize,  rate_mean=mean(ratings))
#rate_mean <- aggregate(day1_add[,10], list(day1_add$uid), mean)
rate_mean <- as.data.table(rate_mean)
day1_os_unique <- as.data.table(day1_os_unique)
setkey(rate_mean,uid)
setkey(day1_os_unique,uid)
# left join 
merge_pop <- merge(day1_os_unique,rate_mean,all.x=TRUE) 
head(merge_pop)
# remove outliers:3 IQR based on counts
summary(merge_pop$counts)
# detect and remove outliers:
data <- merge_pop$counts
FindOutliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}
# use the function to identify outliers
outliers <- FindOutliers(data)
merge_pop1 <- merge_pop[-outliers,]
merge_pop1 <- as.data.frame(merge_pop1)
merge_pop1 <- merge_pop1[, c(2,4)]
merge_pop1 <- scale(merge_pop1)
# merge_pop1 is the dataset should be put into clustering
merge_pop1 <- as.data.frame(merge_pop1)
write.csv(merge_pop1, file="cluster.csv")

## preparing dataset for recommender:
day1_add <- as.data.frame(day1_add)
reco <- day1_add[, c(1,3,10)]
write.csv(reco, file="recommender.csv")


## check rid and get information
unique(day1$rid) # 309798 different songs namely, larger than # of users in a day
table(day1$rid)
day1DT <- data.table(day1)
day1DT_rid <- day1DT[, .N, by = rid] #We notice 74 songs without rid, going to delete
day1df_unique_rid <- as.data.frame(day1DT_rid) # since sort can only be used for df
colnames(day1df_unique_rid) <- c("rid", "counts")
day1_sort_rid <- day1df_unique_rid[order(day1df_unique_rid$counts, decreasing = TRUE),]
head(day1_sort_rid) # 258082 recordings showing no value for rid
day1_top10 <- day1[day1_sort_rid$rid[1:10],]

#########################################not decided
#length(grep('-',day1_1$name))/dim(day1_1)[1]
#day1_1[grep('-',day1_1$name),]
# data3$name[grep('-',data3$name)][1:100]
#day1_1<-day1_1[-grep('-',day1_1$name),]
#############################################

## Select daily top10 songs 
unique(day6$rid) # 309798 different songs namely, larger than # of users in a day
day6_unique_rid <- day6[!duplicated(day6$rid),] 
dim(day6_unique_rid)
head(day6_unique_rid)
head(unique(day6$rid))
day6DT <- data.table(day6)
day6DT_rid <- day6DT[, .N, by = rid] #We notice 75 songs without rid, going to delete
day6df_unique <- as.data.frame(day6DT_rid) # since sort can only be used for df
colnames(day6df_unique) <- c("rid", "counts")
day6_sort_rid <- day6df_unique[order(day6df_unique$counts, decreasing = TRUE),]
head(day6_sort_rid)
day6_sort_rid$rid[1:13] # 258082 recordings showing no value for rid
day6_top10 <- day6_unique_rid[match(day6_sort_rid$rid[1:13],day6_unique_rid$rid),]

## Select hot music for the first three days
day123 <- rbind(day1, day2, day3)
str(day123)
#length(which(day1_no_na$track_time=="0 "))/dim(day1_no_na)[1] #or: sum(with(day1_no_na, track_time=="0 "))
# here notice: 11% of the data has been moved becuase of used track-time level
# It is necessary remove o values of track time, first convert it into numeric
is.data.frame(day123)
day123$track_time <- as.numeric(paste(day123$track_time))
day123$play_time <- as.numeric(paste(day123$play_time))
str(day123)
head(day123)
day123_na <- sapply(day123, function(x) sum(is.na(x))) 
head(day123_na)
day123_noNA <- day123[-which(is.na(day123$track_time)),]
day123_noNA <- day123_noNA[-which(is.na(day1_no_na$play_time)),]
day123_noNA <- day123_noNA[-which(day123_noNA["track_time"]==0),] # remove track_time == 0 items  11% itmes
dim(day123_noNA) 
# add a new col indicating play_track ratio od each recording
library(data.table)
day123_add <- as.data.table(day123_noNA)
day123_add <- day123_add[, played_ratio:=day123_noNA$play_time/day123_noNA$track_time]
head(day123_add)
dim(day123_add)
is.data.frame(day123_add)
summary(day123_add$played_ratio)
# select rows meet condition: play_ratio <= 1
# presentation1
day123_new <- day123_add[which(played_ratio <= 1),]
# presentation2: 
day123_new <- subset(day123_add, played_ratio <= 1)
dim(day123_new)
# get unique rid for songs and counts
uu <- unique(day123_new$rid)
day123_unique_rid <- day123_new[!duplicated(day123_new$rid),]
dim(day123_unique_rid)
head(day123_unique_rid)
day123DT <- data.table(day123)
day123DT_rid <- day123DT[, .N, by = rid] #We notice 75 songs without rid, going to delete
dim(day123DT_rid)
head(day123DT_rid)
day123df_unique <- as.data.frame(day123DT_rid) # since sort can only be used for df
colnames(day123df_unique) <- c("rid", "counts")
day123_sort_rid <- day123df_unique[order(day123df_unique$counts, decreasing = TRUE),]
dim(day123_sort_rid) #450442 rids
head(day123_sort_rid)
# calculate average played_ratio group by rid, using datatable
day123_new <- data.table(day123_new)
setkey(day123_new,rid)
day123_mean_ratio <- day123_new[,mean(played_ratio),by=rid]
head(day123_mean_ratio)
colnames(day123_mean_ratio) <- c("rid", "mean_ratio")
dim(day123_mean_ratio)
adc<-merge(day123_sort_rid, day123_mean_ratio, all=TRUE)
head(adc)
adc <-  adc[-which(is.na(adc$mean_ratio)),]
dim(adc)
adc$popularity <- with(adc, counts*mean_ratio)
head(adc) 
sort_adc <- as.data.table(adc)
sort_adc <- sort_adc[order(popularity, decreasing=TRUE),]
head(sort_adc)
sort_adc$rid[1:25]
popular_songs <- day123_new[match(sort_adc$rid[1:25],day123_new$rid),]

## discover active users
# generate data frame including unique uid separately
library(data.table)
day7DT <- data.table(day7)
day7DT <- day7DT[, .N, by = uid]
day7df_unique_uid <- as.data.frame(day7DT) # since sort can only be used for df
colnames(day7df_unique_uid) <- c("uid", "counts")
day7_sort_uid <- day7df_unique_uid[order(day7df_unique_uid$counts, decreasing = TRUE),]
head(day7_sort_uid) 
dim(day7_sort_uid)
day1_sort_uid
day2_sort_uid
day3_sort_uid
day4_sort_uid
day5_sort_uid
day6_sort_uid
day7_sort_uid
week_sort_uid <- rbind(day1_sort_uid,day2_sort_uid,day3_sort_uid,day4_sort_uid,day5_sort_uid,day6_sort_uid,day7_sort_uid)
head(week_sort_uid) 
#sum all the counts for uid played in these 7 days
week_uid <-aggregate(week_sort_uid$counts,by=list(uid=week_sort_uid$uid),FUN=sum)
head(week_uid)
week_uid_order <- week_uid[with(week_uid, order(-x)),]
head(week_uid_order)
week_uid_order$uid <- as.factor(week_uid_order$uid)
str(week_uid_order)
library(ggplot2)
x<-(1:500)
x<-as.factor(x)
ggplot(x)
top500 <- week_uid_order[1:500,]
xrange <- (1:20)
yrange <- week_uid_order$x[1:20]
plot(xrange,yrange,type = "l", xlab = "Users", ylab = "Played music accumulation",main = "20 Active Users Activity", col = "Blue")
