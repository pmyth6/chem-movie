training.data <- read.csv("training_set.csv", header=TRUE) 
test.data <- read.csv("test_set.csv", header=TRUE)

width=nrow(test.data)

categoryfound=matrix(0,1,width)

for(i in 1:width){
  if(test.data$I[i]>103){
    categoryfound[i]="A"
  }
  if(test.data$III[i]>20){
    categoryfound[i]="E"
  }
}

ind = which(categoryfound == 0)
n.test <- length(ind)
categoryfound[ind] = sample(c("B", "C", "D", "F", "G", "H", "J", "K", "L", "M", 
                              "N", "X"), n.test, replace = TRUE)
g.hat = 0
for (i in 1:100){
  g.hat[i] = categoryfound[i]
}

library(rpart)
library(rpart.plot)
fit = rpart(Impurity.Percent~ I+II+III+IV+V+Temp, data = training.data)
rpart.plot(fit, box.palette = "auto")


predictions = predict(fit, test.data, type = "vector")

y.hat = 0
for (i in 1:100){
  y.hat[i] = predictions[i]
}

ind2 = which(g.hat == "X")
y.hat[ind2] = 0
ind3 = which(y.hat == 0)

write.csv(cbind(g.hat,y.hat), file = "chemical_predictions_group_F_week_1.csv", 
          row.names=FALSE)
