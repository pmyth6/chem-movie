training.data <- read.csv("training_set.csv", header=TRUE) 
test.data <- read.csv("test_set.csv", header=TRUE)
readingswhole=matrix(92,5)
readingdata=matrix(NA,2,5)

traininground=round(training.data[3:7])

for(i in 1:5){
  readingdata[1,]=apply(traininground,2,median)
  readingdata[2,i]=sqrt(var(training.data[i+2]))
} #record means and standard deviations for each reading

readingsadjust=matrix(0,5,92)
for(i in 1:92){
  readingsadjust[1,i]=training.data$I[i]
  readingsadjust[2,i]=training.data$II[i]/(training.data$Impurity.Percent[i])
  readingsadjust[3,i]=training.data$III[i]/(training.data$Impurity.Percent[i])
  readingsadjust[4,i]=training.data$IV[i]/(training.data$Impurity.Percent[i])
  readingsadjust[5,i]=training.data$V[i]/(training.data$Impurity.Percent[i])
}

width=nrow(training.data)
categorytrain=c(1:width)
#setting up receiver matrix

t=4
# for(i in 1:width){
#   
#   categorytrain[i]="LF" #use L and F by default
# 
#   if(training.data$I[i]>readingdata[1,1]+readingdata[2,1]){
#     categorytrain[i]="A"
#   }
#   
#   if((training.data$II[i]>readingdata[1,2]+(readingdata[2,2])/2)){
#     categorytrain[i]="G"
#   }
#   
#   if(training.data$III[i]>readingdata[1,3]+readingdata[2,3]){
#     categorytrain[i]="E"
#   }
#   
#   if(training.data$IV[i]>readingdata[1,4]+readingdata[2,4]){
#     categorytrain[i]="BH" #difference between B and H difficult
#   }
#   
#   if(training.data$V[i]>readingdata[1,5]+(readingdata[2,5])/2){
#     categorytrain[i]="CN" #difference between C and N difficult
#   }
#   
#   if(training.data$IV[i]>readingdata[1,4]){
#     if(training.data$V[i]>readingdata[1,5]+(readingdata[2,5])/2){
#       categorytrain[i]="K" #K classifier
#     }
#   }
#   
#   if((training.data$II[i]>readingdata[1,2]+(readingdata[2,2])/2)){
#       if(training.data$V[i]>readingdata[1,5]+(readingdata[2,5])/2){
#         categorytrain[i]="DJM" #difference between D, J and M difficult
#       }
#   }
#   
# }

#categorytrain

categoryfound=matrix(0,1,width)
for(i in 1:width){
  
  if(training.data$I[i]>103){
    categoryfound[i]="A"
  }
  if(training.data$III[i]>20){
    categoryfound[i]="E"
  }
  
}

ind = which(categoryfound)




#letters left=CFHKL
let1=c("D","G", "J", "M")
let2=c("B", "H")
let3=c("C", "F", "H", "K", "L")#sort unsure letters into groups

for(i in 1:width){
  if(categoryfound[i]=="DGJM"){
    categoryfound[i]=let1[round(runif(1,0.5,4.5))]#randomly generate within groups
  } else if(categoryfound[i]=="BH"){
    categoryfound[i]=let2[round(runif(1,0.5,2.5))]
  } else if(categoryfound[i]==0){
    categoryfound[i]=let3[round(runif(1,0.5,5.5))]
  }
}

#repeat for test data

width=100

categorytest=matrix(0,1,width)
for(i in 1:width){
  
  if(test.data$II[i]>16.7){
    categorytest[i]="DGJM"
  }
  
  if(test.data$IV[i]>18){
    categorytest[i]="BH"
  }
  
  if(test.data$I[i]>103){
    categorytest[i]="A"
  }
  if(test.data$III[i]>20){
    categorytest[i]="E"
  }
  
}
#letters left=CFHKL
let1=c("D","G", "J", "M")
let2=c("B", "H")
let3=c("C", "F", "H", "K", "L")#sort unsure letters into groups

for(i in 1:width){
  if(categorytest[i]=="DGJM"){
    categorytest[i]=let1[round(runif(1,0.5,4.5))]#randomly generate within groups
  } else if(categorytest[i]=="BH"){
    categorytest[i]=let2[round(runif(1,0.5,2.5))]
  } else if(categorytest[i]==0){
    categorytest[i]=let3[round(runif(1,0.5,5.5))]
  }
}

print(categorytest)

