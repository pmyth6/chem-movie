library(dplyr)
library(ggplot2)
library(stringr)
load("titanic_data.RData")

passenger_survival = rename(passenger_survival, "PassengerId"="Passenger ID")
passenger_survival = arrange(passenger_survival, PassengerId)
passengers = inner_join(passenger_details, passenger_survival)

apply(passengers, MARGIN = 2, function(x) sum(is.na(x)))
passengers = select(passengers, -Cabin)

select(passengers, Name)
titles <- str_extract(passengers$Name, ",\\s*[^.]+\\.")
titles <- str_trim(str_remove(titles, ","))
titles <- str_trim(str_remove(titles, "[.]"))
passengers$Name = titles

count(group_by(passengers, Name, Survived)) %>% print(n=40)
table(passengers$Name, passengers$Survived)

male_noble_names <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Rev", 
                      "Sir")
male_noble_index = passengers$Name %in% male_noble_names 
male_noble_index = which(male_noble_index == TRUE) 
passengers$Name[ male_noble_index ] <- "male_noble"

female_noble_names <- c("Lady", "Mlle", "Mme", "Ms", "the Countess")
female_noble_index = passengers$Name %in% female_noble_names 
female_noble_index = which(female_noble_index == TRUE) 
passengers$Name[ female_noble_index ] <- "female_noble"

other_names=count(passengers, Name)$Name[which(count(passengers, Name)[2]<4)]

passengers=mutate(passengers, Age_Bracket = ifelse(Age < 18, 'Minor','Adult'))

ggplot(passengers, aes(x = Age, fill = Survived)) + 
  geom_histogram() + 
  geom_vline(xintercept = 18)

ggplot(na.omit(passengers), aes(x = Age_Bracket, fill = Survived)) + 
  geom_bar()



