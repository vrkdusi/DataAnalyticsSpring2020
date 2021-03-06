require(rpart)
data(Titanic)
head(titanic_train)
titanic_rpart = rpart(Survived ~., data=Titanic)
plot(titanic_rpart)
text(titanic_rpart)

require(party)
treeTitanic = ctree(Survived ~., data=Titanic)
plot(treeTitanic)
cforest(Survived~., data=Titanic, controls = cforest_control(mtry=2, mincriterion=0))

require(randomForest)
fitTitanic = randomForest(Survived ~. , data=Titanic)
print(fitTitanic)
varImpPlot(fitTitanic)
plot(fitTitanic)
getTree(fitTitanic,1, labelVar=TRUE)
