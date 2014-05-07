#########################
### Examine dose data ### 
#########################

# load and order data
Data <- read.csv("MCF7_dose_matrix_n665x978.txt",sep = "\t")
Annotations <- unique(read.csv("MCF7_dose_matrix_annt.v2.txt",sep="\t"))
pharm_groups <- unique(Annotations$pcl_name)
all.compounds <- unique(Annotations$pert_id)

### create qq plots for each drug class and drug ###
### save graphs ###
for (group.name in pharm_groups) {
    is_grp <- Annotations$pcl_name == group.name
    group.compounds <- unique(Annotations[is_grp,]$pert_id)
    for (pert1 in group.compounds) {
        # get all dose signatures for a single compound
        is_match1 <- (Annotations$pert_id == pert1)
        est.annt <- Annotations[is_match1,]
        iname <- as.character(est.annt[1,'pert_iname']) 
        # order drug doses
        dose.order <- est.annt[with(est.annt, order(pert_dose)), ]
        ids1 <- dose.order$mod_sig_id
        ### make qq plot
        x.null <- rnorm(978)
        x.null <- x.null[order(x.null)] # random numbers will be expectation
        # plot lowest dose
        x <- Data[,ids1[1]]
        x <- x[order(x)]
        ix <- which(Annotations$mod_sig_id == ids1[1])
        dose <- Annotations[ix,'pert_dose']
        jpeg(paste('./',group.name,'-',iname,'.jpg'), width = 1000, height = 1000)
        transp <- dose/10
        plot(x.null,x,col=rgb(0,0,1,1), pch=19, cex=transp)
        # add higher doses using 'points'
        for (id in ids1) {
            print(id)
            x <- Data[,id]
            x <- x[order(x)]
            ix <- which(Annotations$mod_sig_id == id)
            dose <- Annotations[ix,'pert_dose']
            transp <- dose/10
            points(x.null,x,col=rgb(0,0,1,1), pch=19, cex=transp) #, add=TRUE)
        }
        abline(0,1)
        title(iname)
        dev.off()
    }
}

##########################################
### pairwise correlations of two drugs ### 
##########################################

Data <- read.csv("clique_compound_classes_n4716x978.txt")
Annotations <- unique(read.csv("clique_compound_classes.v2.txt",sep="\t"))
pharm_groups <- unique(Annotations$pharm_group); pharm_groups

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

### pairwise correlation plots
ids1 <- as.character(ids1)
ids2 <- as.character(ids2)
n1 <- length(ids1)
n2 <- length(ids2)
idsCombo <- unlist(list(ids1[1:n1], ids2[1:n2]))
# correlate all pairwise combinations of the two drugs
match_data <- Data[,idsCombo[1:6]]

panel.cor <- function(x, y, digits=2, cex.cor) {
  # function to include correlation on panel of a pairs plot
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method="pearson"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y, method="pearson")
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.25, paste("r=",txt), cex=1.5)
  text(.5, .75, Signif, cex=1.5)
}
pairs(match_data,lower.panel=panel.cor)
title('pairwise profile correlations')
# pairs(match_data,lower.panel=panel.hist)

### distribution of pairwise comparisons
get_pairwise_corr_vec <- function(ids1, ids2) {
    ###
    # This function obtains all pairwise correations ##
    # for the given input signature ids - returns a vector of r values ##
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

#run function
pairwise_obs <- get_pairwise_corr_vec(ids1, ids2)
pairwise_null <- get_pairwise_corr_vec(ids1, ids_null)
length(pairwise_obs)

# plot ecdfs
plot(ecdf(pairwise_obs), col = "dark red")
plot(ecdf(pairwise_null), col = "black", add=TRUE)
legend("topleft",c("null-pairs","observed-pairs"),col=c("black","dark red"),lty=c("dotted","dotted"))

# variance estimate
auc.obs <- mean(sample(pairwise_obs,10000,replace=T) > sample(pairwise_null,10000,replace=T)); auc.obs
auc.boot <- replicate(1000,mean(sample(pairwise_obs,10000,replace=T) > sample(pairwise_null,10000,replace=T))); 
quantile( auc.boot, c(.025, .975))

##########################################
### pairwise correlations of a drug family ### 
##########################################

group.name <- pharm_groups[4]; group.name # specificy drug family
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
# install.packages("gplots")
library("gplots")
heatmap.2(class_matrix-.5, trace="none",dendrogram='none', margins=c(10,10),, Rowv=NA, Colv=NA, col = redblue(64))
# heatmap.2(class_matrix-.5, Rowv=NA, Colv=NA, col = redblue(64), symm=TRUE, margins=c(10,10))
