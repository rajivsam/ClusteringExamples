library(cluster)
fp = "/home/admin123/homals_analysis/Concrete_Data_Discretized.csv"
cdf = read.csv(fp, header=TRUE)

asw <- numeric(20)
for (k in 2:20)
  asw[k] <- pam(cdf, k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")
plot(1:20, asw, type= "h", main = "pam() clustering assessment",
     xlab= "k (# clusters)", ylab = "average silhouette width")
axis(1, k.best, paste("best",k.best,sep="\n"), col = "red", col.axis = "red")

fp1 = "/home/admin123/homals_analysis/Concrete_Data.csv"
cdnf = read.csv(fp1, header= TRUE)
cdnf = cdnf[,-8]
col.names = col.names = c("Cement_Comp_1", "Blast_Furnace_Slag_Comp2",
                          "Fly_Ash_Comp_3", "Water_Comp_4",
                          "Superplasticizer_Comp_5",
                          "Coarse_Aggregate_Comp_6",
                          "Fine_Aggregate_Comp_7",
                          "Concrete_CS")
names(cdnf) = col.names
wss <- (nrow(cdnf)-1)*sum(apply(cdnf,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cdnf,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

library(homals)
var.levels = rep("nominal", 8)
fit = homals(cdf, ndim = 7,rank = 1, level = var.levels)
O = as.data.frame(fit$objscores)

wss <- (nrow(O)-1)*sum(apply(O,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(O,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Try the expectation maximization algorithm using mclust
fit.em = Mclust(cdnf)
summary(fit.em)
fit.em.trans = Mclust(O)
summary(fit.em.trans)
# Examine the optimal solution for PAM
fit.pam.8 = pam(cdf, 8)
randProj(cdnf)