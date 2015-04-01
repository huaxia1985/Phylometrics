#' calculate the tip age rank sum (TARS) metric
#'
#' This function calculates the TARS metric. To conduct significance test on TARS, use 'tars' as input of 'func' in 'treestat'
tars <- function (state,phy) {
	nspecies<-length(state)
	aa<-order(phy$edge[,1],decreasing=T)
	phy$edge<-phy$edge[aa,]
	phy$edge.length<-phy$edge.length[aa]
	BL<-phy$edge.length[order(phy$edge[,2])][1:nspecies]
	tiprank<-rank(BL)
	sumrank<-sum(tiprank[state==1])	
}
