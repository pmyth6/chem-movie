###cross validation 

no.folds = 10
folds = sample(rep(1:no.folds, length.out = nrow(training.nocat)))

k.vals = seq(3,20, by = 1)
errors = numeric(length(k.vals))


for(i in seq_along(k.vals)){
  k.value = k.vals[i]
  fold_rmse = numeric(no.folds)
  
  for(j in 1:no.folds){
    train_data = training.nocat[folds != j,]
    validation_data = training.nocat[folds == j,]
    
    gam_model = gam(Impurity.Percent ~ s(I, k = k) + s(II, k = k) + s(III, k = k)+ s(IV, k = k)+s(V, k=k)+s(Temp, k= k), data = train_data)
    
    predictions = predict(gam_model, validation_data)
    actual.vals = validation_data$Impurity.Percent
    
    fold_rmse[j] = sqrt(mean((predictions-actual.vals)^2))
  }
  
  errors[i] = mean(fold_rmse)
}

best_k = k.vals[which.min(errors)]

###this implies the best value of k if you leave all the same is 5. now we will run to find each optimal value of k:

##I
no.folds = 7
folds = sample(rep(1:no.folds, length.out = nrow(training.nocat)))

k.vals = seq(1,20, by = 1)
errors = numeric(length(k.vals))


for(i in seq_along(k.vals)){
  k.value = k.vals[i]
  fold_rmse = numeric(no.folds)
  
  for(j in 1:no.folds){
    train_data = training.nocat[folds != j,]
    validation_data = training.nocat[folds == j,]
    
    gam_model = gam(Impurity.Percent ~ s(I, k = k) + s(II, k = 5) + s(III, k = 5)+ s(IV, k = 5)+s(V, k=5)+s(Temp, k= 5), data = train_data)
    
    predictions = predict(gam_model, validation_data)
    actual.vals = validation_data$Impurity.Percent
    
    fold_rmse[j] = sqrt(mean((predictions-actual.vals)^2))
  }
  
  errors[i] = mean(fold_rmse)
}

best_k_I = k.vals[which.min(errors)]

##II


for(i in seq_along(k.vals)){
  k.value = k.vals[i]
  fold_rmse = numeric(no.folds)
  
  for(j in 1:no.folds){
    train_data = training.nocat[folds != j,]
    validation_data = training.nocat[folds == j,]
    
    gam_model = gam(Impurity.Percent ~ s(I, k = 5) + s(II, k = k) + s(III, k = 5)+ s(IV, k = 5)+s(V, k=5)+s(Temp, k= 5), data = train_data)
    
    predictions = predict(gam_model, validation_data)
    actual.vals = validation_data$Impurity.Percent
    
    fold_rmse[j] = sqrt(mean((predictions-actual.vals)^2))
  }
  
  errors[i] = mean(fold_rmse)
}

best_k_II = k.vals[which.min(errors)]


##III
for(i in seq_along(k.vals)){
  k.value = k.vals[i]
  fold_rmse = numeric(no.folds)
  
  for(j in 1:no.folds){
    train_data = training.nocat[folds != j,]
    validation_data = training.nocat[folds == j,]
    
    gam_model = gam(Impurity.Percent ~ s(I, k = 5) + s(II, k = 5) + s(III, k = k)+ s(IV, k = 5)+s(V, k=5)+s(Temp, k= 5), data = train_data)
    
    predictions = predict(gam_model, validation_data)
    actual.vals = validation_data$Impurity.Percent
    
    fold_rmse[j] = sqrt(mean((predictions-actual.vals)^2))
  }
  
  errors[i] = mean(fold_rmse)
}

best_k_III = k.vals[which.min(errors)]

#IV
for(i in seq_along(k.vals)){
  k.value = k.vals[i]
  fold_rmse = numeric(no.folds)
  
  for(j in 1:no.folds){
    train_data = training.nocat[folds != j,]
    validation_data = training.nocat[folds == j,]
    
    gam_model = gam(Impurity.Percent ~ s(I, k = 5) + s(II, k = 5) + s(III, k = 5)+ s(IV, k = k)+s(V, k=5)+s(Temp, k= 5), data = train_data)
    
    predictions = predict(gam_model, validation_data)
    actual.vals = validation_data$Impurity.Percent
    
    fold_rmse[j] = sqrt(mean((predictions-actual.vals)^2))
  }
  
  errors[i] = mean(fold_rmse)
}

best_k_IV = k.vals[which.min(errors)]


###V

for(i in seq_along(k.vals)){
  k.value = k.vals[i]
  fold_rmse = numeric(no.folds)
  
  for(j in 1:no.folds){
    train_data = training.nocat[folds != j,]
    validation_data = training.nocat[folds == j,]
    
    gam_model = gam(Impurity.Percent ~ s(I, k = 5) + s(II, k = 5) + s(III, k = 5)+ s(IV, k = 5)+s(V, k=k)+s(Temp, k= 5), data = train_data)
    
    predictions = predict(gam_model, validation_data)
    actual.vals = validation_data$Impurity.Percent
    
    fold_rmse[j] = sqrt(mean((predictions-actual.vals)^2))
  }
  
  errors[i] = mean(fold_rmse)
}

best_k_V = k.vals[which.min(errors)]

###temp

for(i in seq_along(k.vals)){
  k.value = k.vals[i]
  fold_rmse = numeric(no.folds)
  
  for(j in 1:no.folds){
    train_data = training.nocat[folds != j,]
    validation_data = training.nocat[folds == j,]
    
    gam_model = gam(Impurity.Percent ~ s(I, k = 5) + s(II, k = 5) + s(III, k = 5)+ s(IV, k = 5)+s(V, k=5)+s(Temp, k= k), data = train_data)
    
    predictions = predict(gam_model, validation_data)
    actual.vals = validation_data$Impurity.Percent
    
    fold_rmse[j] = sqrt(mean((predictions-actual.vals)^2))
  }
  
  errors[i] = mean(fold_rmse)
}

best_k_temp = k.vals[which.min(errors)]


###try k = 3 for I

mod_gamk = gam(Impurity.Percent ~ s(I,k=3)+s(II)+s(III)+s(IV)+s(V)+s(Temp), data = training.data)
predictionsk = predict(mod_gamk, train.data)
resid2 = predictionsk-training.data$Impurity.Percent
mean(abs(resid2))
AIC(mod_gamk)
gam.check(mod_gamk)

##try with cubic regression splines

mod_gam.cub = gam(Impurity.Percent ~ s(I,k=3, bs = "cr")+s(II, bs = "cr")+s(III, bs = "cr")+s(IV, bs = "cr")+s(V, bs = "cr")+s(Temp, bs = "cr"), data = training.data)
preds = predict(mod_gam.cub, train.data)
resids = preds-training.data$Impurity.Percent
mean(abs(resids))
AIC(mod_gam.cub)

gam.check(mod_gam.cub) ##gam.check shows k value of s(V) too low. increase k till it is above 1 

mod_gam.cub2 = gam(Impurity.Percent ~ s(I,k=3, bs = "cr")+s(II, bs = "cr")+s(III, bs = "ps")+s(IV, bs = "cr")+s(V, k = 15, bs = "cr")+s(Temp,bs = "cr"), data = training.data)
gam.check(mod_gam.cub2)
AIC(mod_gam.cub2) #improvement

preds = predict(mod_gam.cub2, train.data)
resids = preds-training.data$Impurity.Percent
mean(abs(resids)) ##better again 
plot(mod_gam.cub2) 

##try with penalised splines - disregarded

# mod_gam.pen = gam(Impurity.Percent ~ s(I, bs = "ps")+s(II, bs = "ps")+s(III, bs = "ps")+s(IV, bs = "ps")+s(V, bs = "ps")+s(Temp, bs = "ps"), data = training.data)
# preds = predict(mod_gam.pen, train.data)
# resids = preds-training.data$Impurity.Percent
# mean(abs(resid2))
# AIC(mod_gam.pen)

###AIC worse so don't use this one
