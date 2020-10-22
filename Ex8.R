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

# 2. write guess my number game
# create function to guess numbers

game<-function(guess) {
  cat("I'm thinking of a number 1-100...")
  answer=sample(1:100, 1) # generates a random # from 1-100
  guess=as.numeric(readline(prompt="Guess: ")) # gives the prompt for game
  count=1 # variable to count number of attempts
    
  while (guess!=answer){ # gives the game multiple guesses
    if (guess > answer){ # if guess is higher than answer, say "lower" and guess again
    cat("Lower")
    guess=as.numeric(readline(prompt="Guess: "))
    }else { # if guess is lower than answer, say "higher" and guess again
    cat("Higher")
    guess=as.numeric(readline(prompt="Guess: "))
    }
    
    if (guess==answer){ # if guess is correct, say "correct" and game ends 
      cat("Correct!")
    }
    
  count=count+1# counts number of attempts every time a guess is made
  if (count==10) # when 10 guesses are made, the game ends
    break
    } 
}
game() # run game
