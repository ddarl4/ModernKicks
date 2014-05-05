
# read in 
# Data <- read.csv("File3.csv")
Data <- read.csv("clique_compound_classes_n4716x978.txt")

# random data for a single group
DataNumber <- sample(2:ncol(Data), 1)
hist(Data[[DataNumber]], xlim = c(-10, 10), main = "Random drug data - histogram of z-scores", breaks = 100, prob = TRUE)

Annotations <- unique(read.csv("File4.csv"))
pharm_groups <- unique(Annotations$pharm_group)

# pull signatures for one pharm groups - estrogen
is_estrog <- Annotations$pharm_group == 'estrogen_antagonist'
est.annt <- Annotations[is_estrog,]
est.compounds <- unique(Annotations[is_estrog,]$brd)
est.sigs <- Annotations[is_estrog,]$sig_id1
est.data <- Data[,est.sigs]

### all pairwise correlations of any two drugs

# two HDAC inhibitors
# pert1 <- 'BRD-K02130563'
# pert2 <- 'BRD-K00627859'
# two tubulin inhibitors
# pert1 <- 'BRD-K26997899'
# pert2 <- 'BRD-A55594068'
# two estrogen compounds 
pert1 <- 'BRD-K94832621'
pert2 <- 'BRD-K93754473'

## get the unique ids for each drug:
is_match1 <- (Annotations$brd == pert1)
ids1 <- Annotations[is_match1,]$sig_id1
is_match2 <-  (Annotations$brd == pert2)
ids2 <- Annotations[is_match2,]$sig_id1
# ids null
n_pert2 <- length(ids2)
ids_null <- sample(Annotations$sig_id1,n_pert2)

get_pairwise_corr_vec <- function(ids1, ids2) {
    ###
    # This function obtains all pairwise correations ##
    # for the given input signature ids
    ###
    ids1 <- as.character(ids1)
    ids2 <- as.character(ids2)
    n1 <- length(ids1)
    n2 <- length(ids2)
    idsCombo <- unlist(list(ids1[1:n1], ids2[1:n2]))
    # correlate all pairwise combinations of the two drugs
    match_data <- Data[,idsCombo]
    cor_mtrx <- cor(match_data)
    pairwise_cor <- cor_mtrx[ids1,ids2]
    pairwise_vector <- as.vector(pairwise_cor)
    pairwise_vector
}

pairwise_obs <- get_pairwise_corr_vec(ids1, ids2)
pairwise_null <- get_pairwise_corr_vec(ids1, ids_null)
length(pairwise_obs)

plot(ecdf(pairwise_obs), col = "dark red")
plot(ecdf(pairwise_null), col = "black", add=TRUE)



# For example, if pos.scores is a vector containing a score of the 
# positive examples, and neg.scores is a vector containing the negative
#  examples then the AUC is approximated by:
auc.est <- mean(sample(pairwise_obs,10000,replace=T) > sample(pairwise_null,10000,replace=T))

### use MCMC to calculate area under 
N <- 10^4
x.unif <- runif(N,-1,1)
g <- ecdf(pairwise_obs)
ecdf.eval <- g(x.unif)
obs.area <- mean(ecdf.eval)

g <- ecdf(pairwise_null)
ecdf.eval <- g(x.unif)
null.area <- mean(ecdf.eval)
area.diff <- null.area - obs.area




### loop for all group memebers
group.name <- pharm_groups[4]
is_grp <- Annotations$pharm_group == group.name
group.compounds <- unique(Annotations[is_grp,]$brd)

n_group <- length(group.compounds)
class_matrix <- matrix(0,nrow=n_group,ncol=n_group)
colnames(class_matrix) <- group.compounds
row.names(class_matrix) <- group.compounds

for (pert1 in group.compounds) {
    for (pert2 in group.compounds) {
        is_match1 <- (Annotations$brd == pert1)
        ids1 <- Annotations[is_match1,]$sig_id1
        is_match2 <-  (Annotations$brd == pert2)
        ids2 <- Annotations[is_match2,]$sig_id1
        # ids null
        n_pert2 <- length(ids2)
        ids_null <- sample(Annotations$sig_id1,n_pert2)
        # examine pairwise corr
        pairwise_obs <- get_pairwise_corr_vec(ids1, ids2)
        pairwise_null <- get_pairwise_corr_vec(ids1, ids_null)
        # test metric
        auc.est <- mean(sample(pairwise_obs,10000,replace=T) > sample(pairwise_null,10000,replace=T))
        class_matrix[pert1, pert2] <- auc.est
    } 
}

heatmap(class_matrix-.5, Rowv=NA, Colv=NA, col = heat.colors(256), symm=TRUE, margins=c(10,10))

# install.packages("gplots")
library("gplots")

heatmap(class_matrix)
heatmap.2(class_matrix, trace="none",dendrogram='none', margins=c(10,10),, Rowv=NA, Colv=NA)

# variance estimate
# aucs = replicate(1000,mean(sample(pos.scores,1000,replace=T) > sample(neg.scores,1000,replace=T)))




# match two fields from a data frame
# for(cell in unique(est.annt$cell_line)){
#     g<-1
#     is_match1 <- (Annotations$cell_line == cell) & (Annotations$brd == pert1)
#     ids1 <- Annotations[is_match1,]$sig_id1
#     is_match2 <- (Annotations$cell_line == cell) & (Annotations$brd == pert2)
#     ids2 <- Annotations[is_match2,]$sig_id1
# }
#matrix subset
# AAdelays<-subset(FD, select = Delay, subset = (Carrier == "AA"), drop = TRUE)




### scratch
# organize them
ids1 <- as.character(ids1)
ids2 <- as.character(ids2)
n1 <- length(ids1)
n2 <- length(ids2)
# idsCombo <- c(ids1[1:n1], ids2[1:n2])
idsCombo <- unlist(list(ids1[1:n1], ids2[1:n2]))
# correlate all pairwise combinations of the two drugs
match_data <- Data[,idsCombo]
cor_mtrx <- cor(match_data)
dim(cor_mtrx)
pairwise_cor <- cor_mtrx[ids1,ids2]
dim(pairwise_cor)
pairwise_vector <- as.vector(pairwise_cor)
hist(pairwise_vector)
length(pairwise_vector)




### pairwise correlation plots

ids1 <- as.character(ids1)
ids2 <- as.character(ids2)
n1 <- length(ids1)
n2 <- length(ids2)
idsCombo <- unlist(list(ids1[1:n1], ids2[1:n2]))
# correlate all pairwise combinations of the two drugs
match_data <- Data[,idsCombo[1:6]]

panel.hist <- function(x, ...) {
  # function to draw a histogram on a panel of a pairs plot
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...) # col=plot_color
}

panel.cor <- function(x, y, digits=2, cex.cor) {
  # function to include correlation on panel of a pairs plot
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method="spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y, method="spearman")
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.25, paste("r=",txt), cex=1.5)
  text(.5, .75, Signif, cex=1.5)
}

pairs(match_data,lower.panel=panel.cor)
# pairs(match_data,lower.panel=panel.hist)
