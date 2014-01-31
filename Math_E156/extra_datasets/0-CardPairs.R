1-CardPairs
#Make a probability data frame for drawing a single card
Suit<-c("Clubs","Diamonds","Hearts", "Spades")
Rank<-c("Ace","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Jack","Queen","King")
deck <- expand.grid(Suit,Rank); deck  #Make a single deck as a list of vectors
names(deck) <- c("Suit" , "Rank")
Deck <- data.frame(deck);  #adds the column names and converts strings to factors
mode(Deck); is.data.frame(Deck)   #perplexing
table(Deck$Suit,Deck$Rank)
write.csv(Deck,"Cards.csv")    #save it for future use
Reload<-read.csv("Cards.csv"); table(Reload$Suit,Reload$Rank) #reverts to alphabetical
Deck2<-expand.grid(Suit,Rank, Suit,Rank, stringsAsFactors = FALSE)  #a giant Cartesian product!
names(Deck2) <- c("Suit1", "Rank1", "Suit2", "Rank2") #specify the column names
head(Deck2); nrow(Deck2); 52^2    #it is big!
#If we want to simulate drawing two cards without replacement, we need to eliminate duplicates
#We can do this because the columns are strings, not factors
pairs<-subset(Deck2, ((Suit1 < Suit2) | ((Suit1 == Suit2) & (Rank1 < Rank2))))
names(pairs) <- c("Suit1", "Rank1", "Suit2", "Rank2") #specify the column names again
head(pairs); nrow(pairs); 52*51/2   #original row names were preserved
CardPairs<-as.data.frame(pairs, row.names = 1:nrow(pairs)); head(CardPairs)
write.csv(CardPairs,"CardPairs.csv")
CP<-read.csv("CardPairs.csv")
is.factor(CardPairs$Suit1)  #strings are converted to factors
levels(CardPairs$Rank1)   #alphabetical order
#Now we can solve all sorts of standard probability problems
#Probability of drawing two spades
mean((CP$Suit1 == "Spades") | (CP$Suit2 =="Spades")); 15/34
#Probability that both cards are of the same suit
mean(CP$Suit1 == CP$Suit2); 4/17
#For Blackjack we convert ranks to numbers by attaching labels
CP$Value1 <- as.numeric(as.character(factor(CP$Rank1, labels = c(11,8,5,4,10,10,9,10,7,6,10,3,2))))
CP$Value2 <- as.numeric(as.character(factor(CP$Rank2, labels = c(11,8,5,4,10,10,9,10,7,6,10,3,2))))
#The coercion means that the new columns are numbers, not strings or factors
mode(CP$Value1)
CP$Blackjack<-CP$Value1+CP$Value2
head(CP)
table(CP$Blackjack)
#Probability of getting a blackjack
mean(CP$Blackjack== 21)  #a bit less than 5% - remember this
#We can approach the same problems by sampling
Deck<-read.csv("Cards.csv")    #load a single deck
index<-sample(1:52,2); index     #a pair of row numbers
hand<-Deck[index,]; hand       #select rows from data frame
suits<-hand$Suit ; suits      #a vector
suits[1] == suits[2] 
#Now we can do this 10000 times
N<- 10^4; samesuit <- 0
for (i in 1:N) {
  index<-sample(1:52,2)    #a pair of row numbers
  hand<-Deck[index,]      #select rows from data frame
  suits<-hand$Suit      #a vector
  samesuit <- samesuit+(suits[1] == suits[2])
}
samesuit/N; 4/17     #numbers will be close but not exactly the same
#Same approach for probability of a blackjack
#First try everything with a single sample
tenCards<-c("Ten", "Jack", "Queen", "King")    #this vector can be used as a set
is.element("Jack", tenCards); is.element("Nine", tenCards)
index<-sample(1:52,2)     #a pair of row numbers
hand<-Deck[index,]      #select rows from data frame
ranks<-as.character(hand$Rank); ranks      #a vector
ranks[1] == "Ace" && is.element(ranks[2], tenCards)


N<- 10^4; blackjack <- 0
for (i in 1:N) {
  index<-sample(1:52,2)     #a pair of row numbers
  hand<-Deck[index,]      #select rows from data frame
  ranks<-as.character(hand$Rank)      #a vector of strings
#Careful: must use the non-vectorized "and", which is &&, not &
  blackjack <- blackjack + (ranks[1] == "Ace" && is.element(ranks[2], tenCards)) + (ranks[2] == "Ace" && is.element(ranks[1], tenCards))
}
blackjack/N    #should be a bit less than 5%


                     

