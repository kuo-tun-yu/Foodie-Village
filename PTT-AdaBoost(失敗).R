PTT1=read.csv("C:/Users/Student/Desktop/5-模型建立/RawData/PTTContent.csv")
PTT2=read.csv("C:/Users/Student/Desktop/5-模型建立/RawData/PTTComment.csv")
PTT_join = merge(PTT1,PTT2, by='url')

#只留下要用的欄位
PTT_join2=PTT_join[ ,c(3,4,5,6,7,9,11,12,13,14,15)]

#把time這欄做分類 分成上午下午晚上半夜
PTT_join2$time= substr(PTT_join2$time ,1,2)
PTT_join2$time = as.character(PTT_join2$time)

b1=which(PTT_join2$time=='07' |PTT_join2$time=='08'|PTT_join2$time=='09'|PTT_join2$time=='10'|PTT_join2$time=='11'|PTT_join2$time=='12' )
PTT_join2[b1,'time']='上午' 
b2=which(PTT_join2$time=='13' |PTT_join2$time=='14'|PTT_join2$time=='15'|PTT_join2$time=='16'|PTT_join2$time=='17'|PTT_join2$time=='18' )
PTT_join2[b2,'time']='下午' 
b3=which(PTT_join2$time=='19' |PTT_join2$time=='20'|PTT_join2$time=='21'|PTT_join2$time=='22'|PTT_join2$time=='23'|PTT_join2$time=='00' )
PTT_join2[b3,'time']='晚上' 
b4=which(PTT_join2$time=='01' |PTT_join2$time=='02'|PTT_join2$time=='03'|PTT_join2$time=='04'|PTT_join2$time=='05'|PTT_join2$time=='06' )
PTT_join2[b4,'time']='半夜' 
rm(b1,b2,b3,b4)

PTT_join2$area = as.character(PTT_join2$area)
ar0=which(PTT_join2$area==0)
PTT_join2[ar0,'area'] ='其他地區'
ar1=which(PTT_join2$area==1)
PTT_join2[ar1,'area'] ='北區'
ar2=which(PTT_join2$area==2)
PTT_join2[ar2,'area'] ='中區'
ar3=which(PTT_join2$area==3)
PTT_join2[ar3,'area'] ='南區'
ar4=which(PTT_join2$area==4)
PTT_join2[ar4,'area'] ='東區'
rm(ar0,ar1,ar2,ar3,ar4)

#文章關鍵字次數
library(stringr)
PTT_join2$chocolate <- as.integer(str_count(PTT_join2$content,"巧克力"))
PTT_join2$cookie <- as.integer(str_count(PTT_join2$content,"餅乾"))
PTT_join2$butter <- as.integer(str_count(PTT_join2$content,"奶油"))
PTT_join2$strawberry <- as.integer(str_count(PTT_join2$content,"草莓"))
PTT_join2$cheese <- as.integer(str_count(PTT_join2$content,"乳酪"))
PTT_join2$milk <- as.integer(str_count(PTT_join2$content,"牛奶"))
PTT_join2$egg <- as.integer(str_count(PTT_join2$content,"蛋"))
PTT_join2$coffee <- as.integer(str_count(PTT_join2$content,"咖啡"))
PTT_join2$lemon <- as.integer(str_count(PTT_join2$content,"檸檬"))
PTT_join2$beef <- as.integer(str_count(PTT_join2$content,"牛肉"))
rm(PTT_join,PTT1,PTT2)

#要開始攤平了
library(nnet)
PTT3<- as.data.frame(cbind(PTT_join2, 
                           class.ind(PTT_join2$time),
                           class.ind(PTT_join2$area)))

library(jiebaR)
PTT3$content = as.character(PTT3$content) 
PTT3$comment = as.character(PTT3$comment) 
PTT3$nchar1 =nchar(PTT3$content)  #原文字元數
PTT3$nchar2 =nchar(PTT3$comment)  #回文字元數

#無人回應y1就是1 (注意不要執行2次)
PTT3$y1 <- ifelse(PTT3$nchar2==0, 1, 0) 

PTT4 = PTT3[ ,c(1,2,3,4,8,9,10,11,12,13,14,15,
                16,17,18,19,20,21,22,23,24,25,
                26,27,28,29,30,31,33)]
#資料預處理結束

PTT4$y1 <- factor(PTT4$y1)

library(caTools)
set.seed(11)
split =sample.split(PTT4$y1 , SplitRatio =0.8)
trainingset = subset(PTT4, subset =(split==TRUE) )
testset = subset(PTT4, subset =(split==FALSE) )
rm(split)

# ADA建立模型
library(adabag)
library(rpart)
#跑好慢喔~
ADA1 <- boosting(y1 ~ .,
                 data = trainingset,
                 mfinal = 2) #種幾棵樹

# 預測
test_titanic_predicted <- predict(bag_fit, test_titanic)

# 績效
accuracy <- 1 - test_titanic_predicted$error
accuracy