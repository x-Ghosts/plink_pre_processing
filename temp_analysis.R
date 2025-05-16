lmiss <- read.table(file = "callrate/missing.lmiss", header = TRUE, stringsAsFactors = FALSE)
summary(lmiss)
imiss <- read.table(file = "callrate/missing.imiss", header = TRUE, stringsAsFactors = FALSE)
summary(imiss)


hist(lmiss$F_MISS*100, col = "cyan", main = "Missingness Histogram - Spain - SS: 1040", xlab = "% of Locus Missingness",xlim = c(0,100))
abline(v=c(2,5,10), col="red", lwd=2, lty=3)
hist(imiss$F_MISS*100, col = "cyan", main = "Missingness Histogram - Spain - SS: 1040", xlab = "% of Individual Missingness",xlim = c(0,100))
abline(v=c(2,5,10), col="red", lwd=2, lty=3)



frq <- read.table(file = "freq/frequency.frq", header = TRUE, stringsAsFactors = FALSE)
hist(frq$MAF, main = "Histogram of Minor Allele Frequency - 0.05", xlab = "MAF of SPAIN - 1040", xlim = c(0,0.5), col = "green", breaks = 50)
abline(v=c(0,0.01,0.02,0.05), col="blue", lwd=2, lty=3)
frq_filtered <- read.table(file = "freq/frequency_2.frq", header = TRUE, stringsAsFactors = FALSE)
hist(frq$MAF, main = "Histogram of Minor Allele Frequency - Unfiltered", xlab = "MAF of SPAIN - 1040", xlim = c(0,0.5), col = "green", breaks = 50)
abline(v=c(0,0.01,0.02,0.05), col="blue", lwd=2, lty=3)



ibs <-read.table(file = "ibs/ibs.genome", header = TRUE, stringsAsFactors = FALSE)
summary(ibs)
hist(ibs$DST, xlab = "PI_HAT", main = "Number of Pairs sharing their genomes IBS Creole @ 0.02 Filtered data", col = "yellow", xlim = c(0.9,1))
ibs_090 <- ibs[which(ibs$PI_HAT >= 0.5), ]     
hist(ibs_090$DST, main = "IBS of Spain - 1040", xlab = "Related or duplicates in sampling", col = "yellow", xlim = c(0.7,1))




pc1 <-1
pc2 <-2
  

pc_val <- read.table(file = "pca/pc_spain.eigenval", header = FALSE, stringsAsFactors = FALSE)
pc_vec <- read.table(file = "pca/pc_spain.eigenvec", header = FALSE, stringsAsFactors = FALSE)

total_pc_val <- sum(pc_val$V1)
pc_val$V2 <- round(pc_val$V1/total_pc_val*100,4)

pc_vecvec <- pc_vec[, -c(1,2)]
pc_vecfam <- pc_vec[, c(1,2)]

pc_vecfam$pch <- as.numeric(as.factor(pc_vecfam$V1))

plot(pc_vecvec[,pc1], pc_vecvec[,pc2],
     main = "PCA of Spain - SS 1033", 
     xlab = paste0("PC", pc1 , "(", pc_val$V2[pc1], "%)"), 
     ylab = paste0("PC", pc2 , "(", pc_val$V2[pc2], "%)"), 
     pch = pc_vecfam$pch, cex= 0.7)
