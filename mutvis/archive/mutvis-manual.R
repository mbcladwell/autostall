# reconcile-seqs.R
# Peter LaPan
# April, 2012
rm(list=ls(all=TRUE))
library(seqinr)
library( ggplot2 )


  working.dir <- "/home/mbc/projects/mutvis/"


l1.aln <- read.alignment(file = paste( working.dir, "lib2.aln", sep=""), format="clustal")
##l1.aln <- read.alignment(file = "/home/mbc/temp/SEZ6L/PL130620seq/round2analysis/lib2.aln", format="clustal")

aligns <- as.matrix(l1.aln)


levels(SEQINR.UTIL$CODON.AA$L)
aas <- c(levels(SEQINR.UTIL$CODON.AA$L), 'X')
freqs <- matrix(  ncol=dim(aligns)[2], nrow=length(aas))
rownames(freqs) <- aas

for( col in 1:dim(aligns)[2]){
  for( row in 1:length(aas)){
    freqs[row, col] <- length(which(toupper(aligns[,col])==aas[row]))/dim(aligns)[1]
  }
}


plot( which(freqs[7,]>0), freqs[7, freqs[7,]>0], pch=rownames(freqs)[7], ylab="Frequency", xlab="Position", cex=0.7)
plot.window( xlim = c(1, dim(freqs)[2]), ylim= c(0,1),  ylab="Frequency", xlab="Position")
#plot.window( xlim = c(1, 40), ylim= c(0,1),  ylab="Frequency", xlab="Position")


for( i in 1:length(aas)){
  points( which(freqs[i,]>0), freqs[i, freqs[i,]>0], pch=rownames(freqs)[i], cex=0.7)
}

#ref <-aligns[rownames(aligns)=="15_F1",]


ref <-aligns[rownames(aligns)==rownames(aligns)[1],]

##https://github.com/leipzig/berryLogo

for(i in 1:length(ref)){

  if(  length( freqs[rownames(freqs)[rownames(freqs)==toupper(ref[i])],i] ) > 0){
    
    if(freqs[rownames(freqs)[rownames(freqs)==toupper(ref[i])],i] > 0){
      points( i,freqs[rownames(freqs)[rownames(freqs)==toupper(ref[i])],i]  , pch=toupper(ref[i]), cex=0.7, col="red")
    }


    
  }
}



    
