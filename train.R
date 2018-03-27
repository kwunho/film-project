library(caret)
tc=trainControl(method="cv",number = 10)
model=train(form,data=Film1,trControl=tc,method="rpart",tuneGrid=data.frame(cp=seq(0.01,0.05,0.01)))
library(partykit)
rt1.p=as.party(model$finalModel)
plot(rt1.p)




model=train(form,data=Film1,trControl=tc,method="rf",tuneGrid=data.frame(mtry=3))
library(partykit)
rt1.p=as.party(model$finalModel)
plot(rt1.p)
