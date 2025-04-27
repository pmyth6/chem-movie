##refined gam model 

install.packages("mgcv")
install.packages("gam")
library(gam)
library(mgcv)

##load data 
training.data <- read.csv("training_set.csv", header=TRUE)
test.data <- read.csv("test_set.csv", header=TRUE)

#create model (analysis and cross validation not included here)
refined_gam = gam(Impurity.Percent ~ s(I, k = 3, bs = "cr")+s(II, bs = "cr")+s(III, bs = "ps")+s(IV, bs = "cr")+s(V, k = 15, bs = "cr")+s(Temp,bs = "cr"), data = training.data)

#predictions
predictions.percent = predict(refined_gam, test.data)
