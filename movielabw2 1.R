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
plot(p$rotation[1,],p$rotation[2,])

#variance of each movie

varratings=matrix(data=NA,9125,19)
varratings[,1:18]=categoryratings[,1:18]
for(i in 1:9125){
  varratings[i,19]=var(justratings[i,],na.rm=TRUE)
  if(is.na(varratings[i,19])==TRUE){#catching NAs from only 1 rating
    varratings[i,19]=0
  }
}
varrated=varratings[-rem2,]#removing unrated movies

ratedmovies=c(1:9125)
ratedmovies=ratedmovies[-rem2]#vector with rated movies

#vectors storing movie types
ActionFilms=c()
AdventureFilms=c()
AnimationFilms=c()
ChilderenFilms=c()
ComedyFilms=c()
CrimeFilms=c()
DocumentaryFilms=c()
DramaFilms=c()
FantasyFilms=c()
HorrorFilms=c()
IMAXFilms=c()
MusicalFilms=c()
MysteryFilms=c()
RomanceFilms=c()
SciFiFilms=c()
ThrillerFilms=c()
WarFilms=c()
WesternFilms=c()
for(i in 1:671){#only use rated films
  if(catratings[ratedmovies[i],1]==0){
    ActionFilms=append(ActionFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],2]==0){
    AdventureFilms=append(AdventureFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],3]==0){
    AnimationFilms=append(AnimationFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],4]==0){
    ChilderenFilms=append(ChilderenFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],5]==0){
    ComedyFilms=append(ComedyFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],6]==0){
    CrimeFilms=append(CrimeFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],7]==0){
    DocumentaryFilms=append(DocumentaryFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],8]==0){
    DramaFilms=append(DramaFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],9]==0){
    FantasyFilms=append(FantasyFilms,ratedmovies[i])
    
  }
  if(catratings[ratedmovies[i],10]==0){
    HorrorFilms=append(HorrorFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],11]==0){
    IMAXFilms=append(IMAXFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],12]==0){
    MusicalFilms=append(MusicalFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],13]==0){
    MysteryFilms=append(MysteryFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],14]==0){
    RomanceFilms=append(RomanceFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],15]==0){
    SciFiFilms=append(SciFiFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],16]==0){
    ThrillerFilms=append(ThrillerFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],17]==0){
    WarFilms=append(WarFilms,ratedmovies[i])
  }
  if(catratings[ratedmovies[i],18]==0){
    WesternFilms=append(WesternFilms,ratedmovies[i])
  }
}

genres=(list(ActionFilms))
genres=append(genres,list(AdventureFilms),after=0)
genres=append(genres,list(AnimationFilms),after=0)
genres=append(genres,list(ChilderenFilms),after=0)
genres=append(genres,list(ComedyFilms),after=0)
genres=append(genres,list(CrimeFilms),after=0)
genres=append(genres,list(DocumentaryFilms),after=0)
genres=append(genres,list(DramaFilms),after=0)
genres=append(genres,list(FantasyFilms),after=0)
genres=append(genres,list(HorrorFilms),after=0)
genres=append(genres,list(IMAXFilms),after=0)
genres=append(genres,list(MusicalFilms),after=0)
genres=append(genres,list(MysteryFilms),after=0)
genres=append(genres,list(RomanceFilms),after=0)
genres=append(genres,list(SciFiFilms),after=0)
genres=append(genres,list(ThrillerFilms),after=0)
genres=append(genres,list(WarFilms),after=0)
genres=append(genres,list(WesternFilms),after=0)
#using list to avoid matrix with NAs


genratings=matrix(NA,18,15000)
ce=matrix(0,18,1)
for(i in 1:18){
  for(j in 1:9125){
    if(j%in%genres[[19-i]]==TRUE){
      for(k in 19:161){
        if(is.na(catratings[j,k])==FALSE){
          ce[i,1]=ce[i,1]+1
          genratings[i,ce[i,1]]=catratings[j,k]
        }
      }
    }
  }
}

genstats=matrix(NA,18,3)#this matrix has the mean,mode and std deviation for each genre
for(i in 1:18){
  genstats[i,1]=mean(genratings[i,],na.rm=TRUE)
  genstats[i,2]=var(genratings[i,],na.rm=TRUE)
}


usergenratings=matrix(NA,18,143,dimnames=list(colnames(catratings)[1:18],colnames(catratings)[19:161]))#this matrix has the mean ratings for each user for each genre
usergenweightings=matrix(NA,18,143,dimnames=list(colnames(catratings)[1:18],colnames(catratings)[19:161]))#this matrix has the number of movies of each genre each user rated
fillvector=matrix(NA,nrow=1,ncol=671)
for(i in 1:143){
  for(j in 1:18){
    for(k in 1:671){
      if(is.na(catratings[ratedmovies[k],i+18])==FALSE){
        if(catratings[ratedmovies[k],j]==1){
          fillvector[k]=catratings[ratedmovies[k],i+18]
        }
      }
    }
    usergenratings[j,i]=mean(fillvector,na.rm=TRUE)
    usergenweightings[j,i]=sum(!is.na(fillvector))
    fillvector=matrix(NA,nrow=1,ncol=671)
  }
}

for (i in 1:18){
  plot(c(1:sum(!is.na(usergenratings[i,]))),na.omit(usergenratings[i,]))
}

