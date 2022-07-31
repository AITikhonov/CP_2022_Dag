library(readr)
library(dplyr)
library(MASS)
library(lubridate)
library(caret)
library(quantreg)
library(pls)

#MEtric
Rec.all <- function(data, lev = NULL, model = NULL) {
  uni<-unique(data$obs)
  k<-length(uni)
  data$pred[data$pred>(k-1)]<-(k-1)
  b<-0
  for  (i in 1:k)
  {
    a<-MLmetrics::Recall (data$obs, (data$pred), positive=uni[i])
    if (is.na(a)) a<-0
    b<-b+a
  }
  c(Rec.all=b/i)
  
}  


setwd("E:/R/_cp2022/_dag")
train0<-read_csv("train_dataset_train.csv")
test0<-read_csv("test_dataset_test.csv")

train<-train0[,1:10]%>%mutate_all(as.character)
train[is.na(train)]<-"-"
train<-cbind(train, train0[,11:14])

#Group dubles
train<-train%>%group_by(id, Дата, Время, Место, Улица, Дом, Дорога, Километр,
                                                  Метр, `Вид ДТП`)%>%
  summarise_at(c("Погибло", "Погибло детей", "Ранено", "Ранено детей"),max)

train<-as.data.frame(train)

test<-test0%>%mutate_all(as.character)
test[is.na(test)]<-"-"

all<-rbind(train[,1:10],test)

all$Дата<-as.Date(all$Дата, "%d.%m.%Y")
all$month<-substr(all$Дата,4,5)
all$hour<-substr(all$Время,1,2)
all$wd<-wday(all$Дата)

all<-all[,c(4:8,10:13)]


#Group rare adress
cutF<-10

fr1<-as.data.frame(table(all$Место))
all$Место[all$Место%in%as.character(fr1$Var1[fr1$Freq<=cutF])]<-"rare"

fr2<-as.data.frame(table(all$Улица))
all$Улица[all$Улица%in%as.character(fr2$Var1[fr2$Freq<=cutF])]<-"rare"

fr3<-as.data.frame(table(all$Дом))
all$Дом[all$Дом%in%as.character(fr3$Var1[fr3$Freq<=cutF])]<-"rare"

fr4<-as.data.frame(table(all$Дорога))
all$Дорога[all$Дорога%in%as.character(fr4$Var1[fr4$Freq<=cutF])]<-"rare"

fr5<-as.data.frame(table(all$Километр))
all$Километр[all$Километр%in%as.character(fr5$Var1[fr5$Freq<=cutF])]<-"rare"

fr6<-as.data.frame(table(all$`Вид ДТП`))
all$`Вид ДТП`[all$`Вид ДТП`%in%as.character(fr6$Var1[fr6$Freq<=cutF])]<-"rare"

all<-all%>%mutate_all(as.factor)%>%mutate_all(as.numeric)%>%mutate_all(as.factor)

colnames(all)<-letters[1:9]
tr<-all[1:nrow(train),]
tst<-all[(nrow(train)+1):nrow(all),]


####Validation
'
#set.seed(123)
#  fold<-createDataPartition(tr$Y,10,p=(1/10),list=FALSE)

#system.time(model<-rq(Y~., data=tr,method="sfn", tau=(0:100)/100))
#system.time(model<-cubist(x=tr[,-10], y=tr$Y, committees = 100))
#system.time(model<-plsr(Y~., data=tr, ncomp=100))

#a1<-predict(model,tr, ncomp=100)
#table(round(a1))



#b<-apply(as.data.frame(a1),2, function(x) Rec.all(data=data.frame(obs=tr$Y, pred=x))) 


tr$Y<-train$`Погибло`
tr$Y<-train$`Погибло детей`
tr$Y<-train$`Ранено`
tr$Y<-train$`Ранено детей`

k<-5
ans<-NULL
d<-NULL
for (i in c(1:k)){#ncol(fold)) {
print(i)
  
set.seed(i*10)  
indx<-sample(nrow(train), round(nrow(train)/k))
  

tr0<-tr[-indx,]
val<-tr[indx,]

  
#tr0<-tr[-fold[,i],]
#val<-tr[fold[,i],]

set.seed(123)
#model<-rq(Y~., data=tr0,tau=(0:100)/100,method="sfn")
model1<-rq(Y~., data=tr0,tau=0.48,method="sfn")



set.seed(123)
#model2<-rq(Y~., data=tr0,tau=0.87,method="sfn")
model2<-lm(Y~., data=tr0)


set.seed(123)
#model3<-rq(Y~., data=tr0,tau=0.88,method="sfn")
model3<-plsr(Y~., data=tr, ncomp=200)



pred1<-(predict(model1,val))
pred2<-(predict(model2,val))
pred3<-(predict(model3,val,ncomp=200))[1:nrow(val)]

b2<-NULL
vec<-NULL
for (a1 in c(0,1)) {

  for (a2 in c(0,1)) {
    
    for (a3 in c(0,1)) {
      
s<-a1+a2+a3  
if (s>0) {
pred<-(a1*pred1+a2*pred2+a3*pred3)/s

pred<-round(pred)
pred[pred<0]<-0
b<-Rec.all(data=data.frame(obs=val$Y, pred=pred[1:nrow(val)]))
b2<-rbind(b2,b)
vec<-rbind(vec,paste0(a1,a2,a3))
}

    }
  }
}

##median
pred<-apply(data.frame(pred1,pred2,pred3),1,median)

pred<-round(pred)
pred[pred<0]<-0
b<-Rec.all(data=data.frame(obs=val$Y, pred=pred[1:nrow(val)]))
b2<-rbind(b2,b)

ans<-cbind(ans,b2)

#c<-c(a1,a2,a3,b)

#model<-lm(Y~., data=tr0)
#model<-plsr(Y~., data=tr, ncomp=50)
#model<-randomForest(Y~., data=tr0)
#pred<-round(predict(model,val, ncomp=50))

#model<-cubist(x=tr[,-10], y=tr$Y, committees = 1)
#pred<-round(predict(model,val, neighbors = 1))


#pred<-round(predict(model,val))
#pred[pred<0]<-0

#b<-apply(pred,2, function(x) Rec.all(data=data.frame(obs=val$Y, pred=x)))  
#b<-Rec.all(data=data.frame(obs=val$Y, pred=pred[1:3167]))
}

ans2<-cbind(ans,apply(ans,1,mean))
ans2<-cbind(ans2,apply(ans,1,median))
row.names(ans2)<-c(vec,"median")


plot.ts(ans2[,(ncol(ans2)-1)])
plot.ts(ans2[,ncol(ans2)])


which.max(ans2[,(ncol(ans2)-1)])#mean
which.max(ans2[,ncol(ans2)])#median
'
#END Validation

#Models
tr$Y<-train$`Погибло`
set.seed(123)
system.time(model1<-rq(Y~., data=tr, method="sfn", tau=c(0.965,0.97,0.975)))
a1<-round(apply(predict(model1, tst),1,mean))
a1[a1<0]<-0
a1[a1>1]<-1

tr$Y<-train$`Погибло детей`
system.time(model2<-rq(Y~., data=tr, method="sfn", tau=c(0.49,0.5,0.51)))
a2<-round(apply(predict(model2, tst),1,mean))

tr$Y<-train$`Ранено`
set.seed(123)
system.time(model3<-plsr(Y~., data=tr, ncomp=100))
a3<-round(predict(model3, tst,ncomp=100))[1:nrow(tst)]

tr$Y<-train$`Ранено детей`
set.seed(123)
system.time(model4<-rq(Y~., data=tr, method="sfn", tau=c(0.835,0.84,0.845)))
a4<-round(apply(predict(model4, tst),1,mean))
a4[a4<0]<-0

sample<-read_csv("sample_solution.csv")
sample$Погибло<-a1
sample$`Погибло детей`<-a2
sample$Ранено<-a3
sample$`Ранено детей`<-a4

write_csv(sample, "final_3007.csv")