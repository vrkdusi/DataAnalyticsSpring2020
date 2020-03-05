library(titanic)
library(randomForest)

set.seed(12345)
help(par)
par(mar=rep(0.2,4))
data_Matrix = matrix(rnorm(400), nrow=40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
help('heatmap')
help(rep)
heatmap(data_Matrix)
help('rbinom')
set.seed(678910)
for(i in 1:40){
  coin_Flip = rbinom(1, size=1, prob=0.5)
  if(coin_Flip){
    data_Matrix[i,] = data_Matrix[i,] + rep(c(0,3), each=5)
  }
}
par(mar=rep(0.2,4))
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
hh = hclust(dist(data_Matrix))
data_Matrix_Ordered = data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab='The Row Mean', ylab='Row', pch=19)
plot(colMeans(data_Matrix_Ordered), xlab='Column', ylab='Column Mean', pch=19)


data(Titanic)
#knitr::kable(head(titanic_train))
set.seed(100)
train = sample(nrow(titanic_train), 0.7*nrow(titanic_train), replace=FALSE)
TrainSet = titanic_train[train,]
ValidSet = titanic_train[-train,]
survived_new = titanic_train$Survived
for(i in 1:length(survived_new)){
  if(survived_new[i]=='0'){
    survived_new[i] = 'No'
  }
  if(survived_new[i]=='1'){
    survived_new[i] = 'Yes'
  }
  i = i + 1
}
titanic_train$Survived = survived_new
#model1 = randomForest(titanic_train$Survived ~ . , data = TrainSet, importance=TRUE)
tail(titanic_train)
summary(titanic_train)
#model1