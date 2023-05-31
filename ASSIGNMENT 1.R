library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringer)
#Question 1
##a##
html<-read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
tables<-html %>% html_table()
tables<-tables[[1]]
## we can now arrange it into a dataframe
tables<-data_frame(tables,row.names=NULL)
tables_new<- tables %>% select(-1,-14,-15)



##b##
html1<-read_html("https://www.moneyworks4me.com/indianstocks/large-cap/telecom/telecommunication-service-provider/bharti-airtel/company-info")
data<-html1 %>% html_table(fill=TRUE,convert=FALSE,header=TRUE)
table_1<-data[[1]]
table_2<-data[[3]]
table_1<-data.frame(table_1,row.names=NULL)
names(table_1) <- table_1[1,]
table_1<-table_1[-c(1:5),]
table_1<-table_1[,-c(12,13,14)]
table_2<- data.frame(table_2,row.names = NULL)
names(table_2) <- table_2[1,]
table_2<- table_2[-1,]
table_2 <- table_2[,-c(12,13)]
table_final<-rbind(table_1,table_2)
##5 companies required but code will be same ..just replace the html1 URL as per covinience


##c##
#1#
tennis <- function(p) {
  A <- 0
  B <- 0
  set <- 0
  while (set < 5) {
    if (runif(1) < p) {
       A<- A + 1
    } else {
      B <- B + 1
    }
    set <- set + 1
    if (A == 3 || B == 3) {
      break
    }
  }
  return(set)
}
##p is probability of B winning the match and A and B denotes the no. of matches won by each of them


#2#
matches <- numeric(length = 1000)
for(i in 1:1000)
{
  matches[i] <- tennis(0.7)
}
ans <- mean(matches)

##d##
##1##
MontyHall <- function() {
  door_car <- sample(1:3, 1)#door in which car is present
  chosen_door <- sample(1:3, 1)#door which is chosen by the player
  Monty_opened <- sample(setdiff(1:3, c(door_car, chosen_door)), 1)#door opened by Monty
  switched_door <- setdiff(1:3, c(chosen_door, Monty_opened))#door when switched
  if (switched_door[1] == door_car) {
    return(1)  # Win
  } else {
    return(0)  # Lose
  }
}
##2##
exp <- numeric(length = 1000)
for(i in 1:1000)
{
  exp[i] <- MontyHall()
}
prob <- mean(exp)


##e##



html2 <- "https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/"
html <- read_html(html2)
ranking <- html_text(html_nodes(html, ".countdown-index"))
movie_names <- html_text(html_nodes(html, ".article_movie_title a"))
tomato_score <- html_text(html_nodes(html, ".tMeterScore"))
movie_year <- html_text(html_nodes(html, ".subtle.start-year")) %>% substr(2, 5)
movies_data <- data.frame(
  Ranking = ranking,
  Name = movie_names,
  Tomato_Score = tomato_score,
  Year_of_movie = movie_year
)
print(movies_data)


