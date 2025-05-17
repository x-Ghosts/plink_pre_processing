library(ggplot2)


pc1 <-1
pc2 <-2



lmiss <- read.table(file = "callrate/missing.lmiss", header = TRUE, stringsAsFactors = FALSE)
summary(lmiss)
imiss <- read.table(file = "callrate/missing.imiss", header = TRUE, stringsAsFactors = FALSE)
summary(imiss)


hist(lmiss$F_MISS*100, col = "cyan", main = "Missingness Histogram - PK_TED (51)", breaks = 100, xlab = "% of Locus Missingness",xlim = c(0,100))
abline(v=c(2,5,10), col="red", lwd=2, lty=3)
hist(imiss$F_MISS*100, col = "cyan", main = "Missingness Histogram - PK_TED (51)", breaks = 50, xlab = "% of Individual Missingness",xlim = c(0,100))
abline(v=c(2,5,10), col="red", lwd=2, lty=3)



frq <- read.table(file = "freq/frequency.frq", header = TRUE, stringsAsFactors = FALSE)
hist(frq$MAF, main = "Histogram of Minor Allele Frequency - 0.05", xlab = "MAF of PK_TED (51)", xlim = c(0,0.5), col = "green", breaks = 50)
abline(v=c(0,0.01,0.02,0.05), col="blue", lwd=2, lty=3)
frq_filtered <- read.table(file = "freq/frequency_2.frq", header = TRUE, stringsAsFactors = FALSE)
hist(frq_filtered$MAF, main = "Histogram of Minor Allele Frequency - filtered", xlab = "MAF of PK_TED (51)", xlim = c(0,0.5), col = "green", breaks = 50)
abline(v=c(0,0.01,0.02,0.05), col="blue", lwd=2, lty=3)



ibs <-read.table(file = "ibs/genome.genome", header = TRUE, stringsAsFactors = FALSE)
summary(ibs)
hist(ibs$DST, xlab = "IBS of PK_TED (51)", main = "Number of Pairs sharing their genomes IBS Filtered data", col = "yellow", xlim = c(0.6,1))
ibs_090 <- ibs[which(ibs$PI_HAT >= 0.5), ]     
hist(ibs_090$DST, main = "IBS of GIR_italy - SS 59", xlab = "Related or duplicates in sampling", col = "yellow", xlim = c(0.7,1))




pc_val <- read.table(file = "pca/pc_ted.eigenval", header = FALSE, stringsAsFactors = FALSE)
pc_vec <- read.table(file = "pca/pc_ted.eigenvec", header = FALSE, stringsAsFactors = FALSE)

total_pc_val <- sum(pc_val$V1)
pc_val$V2 <- round(pc_val$V1/total_pc_val*100,4)

pc_vecvec <- pc_vec[, -c(1,2)]
pc_vecfam <- pc_vec[, c(1,2)]

pc_vecfam$pch <- as.numeric(as.factor(pc_vecfam$V1))

plot(pc_vecvec[,pc1], pc_vecvec[,pc2],
     main = "PCA of PK_TED (51)",
     xlab = paste0("PC", pc1 , "(", pc_val$V2[pc1], "%)"), 
     ylab = paste0("PC", pc2 , "(", pc_val$V2[pc2], "%)"), 
     pch = pc_vecfam$pch, cex= 0.7)




pc_df <- data.frame(
  IID = pc_vec$V2,
  PC1 = pc_vecvec[, pc1],
  PC2 = pc_vecvec[, pc2]
)

ggplot(pc_df, aes(x = PC1, y = PC2, label = IID)) +
  geom_point() +
  geom_text(size = 2, vjust = -0.5) +
  theme_minimal() +
  xlab(paste0("PC", pc1, " (", pc_val$V2[pc1], "%)")) +
  ylab(paste0("PC", pc2, " (", pc_val$V2[pc2], "%)")) +
  ggtitle("PCA of PK_TED (51)")
