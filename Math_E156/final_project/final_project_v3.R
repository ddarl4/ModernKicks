Data <- read.csv("MCF7_dose_matrix_n665x978.txt",sep = "\t")

Annotations <- unique(read.csv("MCF7_dose_matrix_annt.v2.txt",sep="\t"))
pharm_groups <- unique(Annotations$pcl_name)


all.compounds <- unique(Annotations$pert_id)

## make qq-dose plots for every compound
for (group.name in pharm_groups) {
    is_grp <- Annotations$pcl_name == group.name
    group.compounds <- unique(Annotations[is_grp,]$pert_id)

    for (pert1 in group.compounds) {
        # match ids
        is_match1 <- (Annotations$pert_id == pert1)
        est.annt <- Annotations[is_match1,]
        iname <- as.character(est.annt[1,'pert_iname']) 
        dose.order <- est.annt[with(est.annt, order(pert_dose)), ]
        ids1 <- dose.order$mod_sig_id

        # make qq plot
        x.null <- rnorm(978)
        x.null <- x.null[order(x.null)]
        x <- Data[,ids1[1]]
        x <- x[order(x)]
        ix <- which(Annotations$mod_sig_id == ids1[1])
        dose <- Annotations[ix,'pert_dose']
        jpeg(paste('./qq_graphs/',group.name,'-',iname,'.jpg'), width = 1000, height = 1000)
        transp <- dose/10
        plot(x.null,x,col=rgb(0,0,1,1), pch=19, cex=transp)
        for (id in ids1) {
            print(id)
            x <- Data[,id]
            x <- x[order(x)]
            # hist(x)
            ix <- which(Annotations$mod_sig_id == id)
            dose <- Annotations[ix,'pert_dose']
            transp <- dose/10
            # qqnorm(x, col=rgb(0,0,1,transp),pch=19,add=TRUE);
            points(x.null,x,col=rgb(0,0,1,1), pch=19, cex=transp) #, add=TRUE)
        }
        abline(0,1)
        title(iname)
        dev.off()
    }
}


