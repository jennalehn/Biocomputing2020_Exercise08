#exercise 8
# 1. make graph based on sports
# load data and name variable
bball=read.delim("UWvMSU_1-22-13.txt", header=TRUE, stringsAsFactors = F)
head(bball)

# separate scores of team
UWscores=bball[bball$team=="UW",3]
MSUscores=bball[bball$team=="MSU",3]

# get cumulative sums of scores for each team
UWcm=cumsum(UWscores)
MSUcm=cumsum(MSUscores)

# get times for scores of each team
UWtime=bball[bball$team=="UW",1]
MSUtime=bball[bball$team=="MSU",1]

# plot graph of MSU time vs cumulative scores- time on x, score on y
# make line blue and label title/axes
plot(UWtime,UWcm, type="l", 
     main= "UW vs. MSU",
     xlab="Time", 
     ylab="Score", 
     col="blue")

# add a line for MSU and label colors
lines(MSUtime,MSUcm, type="l")
legend("topleft", c("UW","MSU"), fill=c("blue","black"))

#done for now help me
output=numeric(length=length(v1))
v1=(c(3,10,4,12,55))
for (i in 1:length(v1)){
  output[i]=sum(v1[1:i])
}
UWtime=0
UWcum=0
UWcum=numeric(length=length(UWscores))
MSUcum=0
timeUW=data.frame(x=numeric(i), stringsAsFactors = F)
for(i in 1:nrow(bball)){
  if (bball$team[i]=="UW"){
  UWcum[i]=sum(bball$score[1:i], na.rm=TRUE)
  UWtime=bball$time[1:i]
  }else{
  MSUcum=cumsum(bball$score[i])
  }}
UWcum
UWfinscore=UWcum[!is.na(UWcum)]
UWfinscore
UWtime
MSUcum
UWscores
sum(UWscores)
sum(bball$scores)
timeUW
?data.frame()
class(UWcum)

# 2. write guess my number game
# create vector from 1 to 100
allnumbers=sample(1:100, 1)
allnumbers


game<-function(guess) {
  cat("I'm thinking of a number 1-100...")
  answer=sample(1:100, 1) # generates a random # from 1-100
  print(answer)
  guess=readline(prompt="Guess: ") # gives the prompt for game
  count=0
  while (guess!=answer){ # gives the game multiple guesses
    count=count+1
    if (count==10)
      break
    if (guess > answer){
    cat("Lower")
    guess=readline(prompt="Guess: ")
  }else if (guess < answer){
    cat("Higher")
    guess=readline(prompt="Guess: ")
  }else cat("Correct!")
  } 
}
game()




