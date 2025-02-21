ratings.train <- read.csv("ratings_train.csv", header=TRUE) 
ratings.test <- read.csv("ratings_test.csv", header=TRUE)
movies<- read.csv("movies.csv", header=TRUE)
moviegenres<-matrix(0,9125,19)
moviegenres[,1]<-movies$movieId
#set out movie genres
colnames(moviegenres)<-c("movieID","Action", "Adventure", "Animation","Children", "Comedy","Crime", "Documentary","Drama", "Fantasy", "Horror", "IMAX","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western")
moviegenrenames<-c("movieID","Action", "Adventure", "Animation","Children", "Comedy","Crime", "Documentary","Drama", "Fantasy", "Horror", "IMAX","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western")
#transform genre presence into binary variable
for(i in 1:9125){
  for(j in 2:19){
    if(grepl(moviegenrenames[j],movies$genres[i])==TRUE){
      moviegenres[i,j]=1
    }
  }
}
movieratings<-matrix(NA,9125,690)
movieratings[,1:19]=moviegenres
colnames(movieratings)<-c("movieID","Action", "Adventure", "Animation","Children", "Comedy","Crime", "Documentary","Drama", "Fantasy", "Horror", "IMAX","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western",1:671)
#assigning every user ratings to each movie
for(i in 20:690){
  a=which(ratings.train$movieId==movieratings[i,1])#list of locations of users that have rated movie i
  b=c(1:length(a))
  for(j in 1:length(a)){
    movieratings[i,ratings.train$userId[b[j]]]=ratings.train$rating[b[j]]#every user that rated each movie recorded 
  }
  }

categoryratings=matrix(9125,689)
categoryratings=movieratings[,2:690]#removing movieids
catmeans=colMeans(categoryratings,na.rm=TRUE)
rem=c(0)
#recording users that did not rate a movie
for(i in 1:689){
  if(is.nan(catmeans[i])==TRUE){
    rem=append(rem,i)
  }
}

#catratings removes users that did not rate a movie
catratings=categoryratings
catratings=catratings[,-rem]

#mean ratings for reach movie
meanratings=matrix(data=NA,9125,19)
meanratings[,1:18]=categoryratings[,1:18]
justratings=catratings[,19:161]
meanratings[,19]=rowSums(justratings,na.rm = TRUE) #some movies were not rated! removing

#finding unrated movies
rem2=c(0)
for(i in 1:9125){
  if(meanratings[i,19]==0){
    rem2=append(rem2,i)
  }
}
meanratings[,19]=rowMeans(justratings,na.rm = TRUE)#means of ratings. in future explore on user-by-user basis, would have to deal with NAs

meanratings=meanratings[-rem2,]#removing unrated movies

p=prcomp(meanratings, center=TRUE, scale=TRUE)#pca
eigvs=p$sdev^2
plot(1:length(eigvs), eigvs/sum(eigvs)*100, ylab="Propotion of variance explained", xlab='component number')#screeplot
p$rotation
mean(meanratings[,19]) #average movie rating ignoring number of people who rated movies

