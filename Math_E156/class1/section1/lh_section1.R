# A flight out of Mogadishu airport has 100 passengers, all male.
# 20% of them wear explosive shoes, and within this group 60% have beards. Among 
# the 80% who wear non-explosive shoes, only 5% have beards.
# Create a data frame with one row per passenger and two columns “Shoes” and “Beard.” 

# shoes beards into a df
explosive_shoes <-c(rep('Yes',20),rep('No',80)); 
beard1 <- c(rep('Yes',12),rep('No',8)) #beard status for explosive shoes
beard2 <- c(rep('Yes',4),rep('No',76)) #beard status for non-explosive shoes
Beards <- c(beard1,beard2)
# make dataframe
df <- data.frame(explosive_shoes,Beards) 
# Make a 2x2 contingency table using these columns.
table(df)

# Then extract the subset of bearded passengers. 
# Use it to calculate the conditional probability that 
# if you inspect a randomly chosen passengerwith 
# a beard, he will turn out to have explosive shoes.
dfBearded <- df[df$Beards == 'Yes',]
nBearded <- nrow(dfBearded)
nBeardedExplosive <- nrow(dfBearded[dfBearded$explosive_shoes == 'Yes',])
# conditional probability
cP <- nBeardedExplosive / nBearded ;cP

### part 2 ###

# The Red Sox and Cardinals are playing another World Series.
# Games 1, 2, 6, and 7 are in Boston, where the Red Sox have a probability 
# of 2/3 of winning. Games 3, 4, and 5 are in St. 
# Louis, where their probability of a Red Sox victory is only 1/2.

# Use expand.grid() to make a data frame with 3423 = 648 rows that correspond 
# to equally-likely outcomes if the teams play 7 games,
rsWin <- c(1,1,0)
slWin <- c(1,0)
games <- expand.grid(rsWin,rsWin,slWin,slWin,slWin,rsWin,rsWin)

# use it to make a histogram of the number of 
# Red Sox vctories if the teams play all seven games
rsGameWins <- apply(games,1,sum)
hist(rsGameWins)

# Of course, an actual World Series ends when one team has won four games. 
# pear down games to only Series that sum to 4 or 3
realSeries <- games[rsGameWins == 3 | rsGameWins == 4,]

# Extract the subset that corresponds to an 
# event like “the Red Sox win the series in six games,” and figure 
# out the probability of that event.
game_6_win <- function(x) {
    if ((x[6] ==1) & (sum(x[-7]) == 4)){
        TRUE
    } else {
        FALSE
    }
}
SeriesWon <- apply(realSeries,1,game_6_win)

# probability series won at game 6
p6 <- sum(SeriesWon) / nrow(realSeries); p6

# There are eight such events. 
# Different members of the class can analyze different events, 
# and someone should check that the probabilites sum to 1.


calc_improvement <- function(x) {
    if (x[1] > x[2]){
        0
    } else if (x[1] == x[2]) {
        1
    } else if (x[1] < x[2]){
        2
    }
}


deck <- expand.grid(Suit,Rank); deck  #Make a single deck as a list of vectors
games <- expand.grid(Suit,Rank); deck  #Make a single deck as a list of vectors

TestResults<-expand.grid(c(0,0,0,1),c(0,0,0,1), stringsAsFactors = FALSE)