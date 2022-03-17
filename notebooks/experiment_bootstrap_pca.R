library(bootSVD)
library(tidyverse)


set.seed(0)

forced_choice_df = read_csv('../clean_data/forced_choice_uw_students_vectors.csv')

myvars <- c('happiness', 'neutral', 'surprise', 'sadness', 'disgust', 'anger', 'fear', 'uncertain')
Y <- forced_choice_df[myvars]
Y.m <- as.matrix(Y)

b<-bootSVD(Y.m, B=50, K=2, output= 
             c('initial_SVD', 'HD_moments', 'full_HD_PC_dist',
               'HD_percentiles'), verbose=interactive())
b

matplot(b$initial_SVD$V[,1:2],type='l',main='Fitted PCs',lty=1)
legend('bottomright',paste0('PC',1:2),col=1:2,lty=1,lwd=2)


# 1st moment is the expected value
# 2nd moment is the variance
# 3th moment is the skewness
# 4th moment is the kurtosis


# b$LD_moments$EPCs - expected value principal component
# b$LD_moments$EPCs[[1]][1] - expected value 1st principal component
# b$LD_moments$EPCs[[1]][2] - expected value 2nd principal component


# b$LD_moments$sdPCs - standard deviation principal component

Y.t <- t(Y.m)

svdY.t <-fastSVD(Y.t)
svdY <-fastSVD(Y.m)


tcrossprod(diag(svdY$d[1]),svdY$u[1])

U<-svdY.t$u
V<-svdY.t$v
d<-svdY.t$d

b<-bootSVD(Y.t, U = U, V=V, d=d, B=50, K=2, output= 
          c('initial_SVD', 'HD_moments', 'full_HD_PC_dist',
            'HD_percentiles'), verbose=interactive())

matplot(b$initial_SVD$V[,1:2],type='l',main='Fitted PCs',lty=1)
legend('bottomright',paste0('PC',1:2),col=1:2,lty=1,lwd=2)

######################
# look specifically at 2nd PC
k<-2

######
#looking at HD variability

#plot several draws from bootstrap distribution
VsByK<-reindexMatricesByK(b$full_HD_PC_dist)
matplot(t(VsByK[[k]][1:20,]),type='l',lty=1,
		main=paste0('20 Draws from bootstrap\ndistribution of HD PC ',k))

#plot pointwise CIs
matplot(b$HD_moments$momentCI[[k]],type='l',col='blue',lty=1,
		main=paste0('CIs For HD PC ',k))
matlines(b$HD_percentiles[[k]],type='l',col='darkgreen',lty=1)
lines(b$initial_SVD$V[,k])
legend('topright',c('Fitted PC','Moment CIs','Percentile CIs'),
		lty=1,col=c('black','blue','darkgreen'))
abline(h=0,lty=2,col='darkgrey')

######
# looking at LD variability

# plot several draws from bootstrap distribution
AsByK<-reindexMatricesByK(b$full_LD_PC_dist)
matplot(t(AsByK[[k]][1:50,]),type='l',lty=1,
		main=paste0('50 Draws from bootstrap\ndistribution of LD PC ',k),
	xlim=c(1,10),xlab='PC index (truncated)')

# plot pointwise CIs
matplot(b$LD_moments$momentCI[[k]],type='o',col='blue',
		lty=1,main=paste0('CIs For LD PC ',k),xlim=c(1,10),
		xlab='PC index (truncated)',pch=1)
matlines(b$LD_percentiles[[k]],type='o',pch=1,col='darkgreen',lty=1)
abline(h=0,lty=2,col='darkgrey')
legend('topright',c('Moment CIs','Percentile CIs'),lty=1,
		pch=1,col=c('blue','darkgreen'))
#Note: variability is mostly due to rotations with the third and fourth PC.

# Bootstrap eigenvalue distribution
dsByK<-reindexVectorsByK(b$d_dist)
boxplot(dsByK[[k]]^2,main=paste0('Covariance Matrix Eigenvalue ',k),
		ylab='Bootstrap Distribution',
		ylim=range(c(dsByK[[k]]^2,b$initial_SVD$d[k]^2)))
points(b$initial_SVD$d[k]^2,pch=18,col='red')
legend('bottomright','Sample Value',pch=18,col='red')



##########################
##########################


# B. perform appropriate linear model analysis on each principal component axis.

df.pca = read_csv('../clean_data/pca_2d_forced_all.csv')

df.pca

test.1=summary(aov(x_pca~label, data=df.pca))
test.2=summary(aov(y_pca~label, data=df.pca))


# C. Get Sums of Squares from each test to get overall results.

ssTreat.x = test.1[[1]][1,2]
ssError.x = test.1[[1]][2,2]
dfTreat.x = test.1[[1]][1,1]
dfError.x = test.1[[1]][2,1]

ssTreat.y = test.2[[1]][1,2]
ssError.y = test.2[[1]][2,2]
dfTreat.y = test.2[[1]][1,1]
dfError.y = test.2[[1]][2,1]

# D. Add Sums of Square for overall test
sumSS.trtmnt=(ssTreat.x+ssTreat.y)/(dfTreat.x+dfTreat.y)
sumSS.error=(ssError.x+ssError.y)/(dfError.x+dfError.y)

# F-test
F.all =sumSS.trtmnt/mean.error
F.df1 = dfTreat.x+dfTreat.y
F.df2 = dfError.x+dfError.y

1-pf(F.all,F.df1,F.df2)


#################################

# install.packages("remotes")
# remotes::install_github("villardon/MultBiplotR")
library(MultBiplotR)


pcaboot=PCA.Bootstrap(Y.m, dimens=2, Scaling = "Standardize columns", B=1000)
plot(pcaboot, ColorInd=as.numeric(grupo))
summary(pcaboot)

library(plyr)

## Not run: 
forced_choice_df_DT = as.data.frame(forced_choice_df)
forced_choice_df_DT <- forced_choice_df_DT %>% drop_na()


forced_choice_df_DT$group <- forced_choice_df_DT$label
forced_choice_df_DT$group  <- mapvalues(forced_choice_df_DT$group , from=c("happiness", "neutral","surprise","sadness", "disgust", "anger", "fear", "uncertain"), to=c(1, 2, 3, 4, 5, 6, 7, 8))


X=forced_choice_df_DT[,1:8]
grupo=forced_choice_df_DT$group
rownames(X)=paste(1:192, grupo, sep="-")
pcaboot=PCA.Bootstrap(X, dimens=2, Scaling = "Standardize columns", B=1000)
plot(pcaboot, ColorInd=as.numeric(grupo))
summary(pcaboot)

# 
# X=wine[,4:21]
# grupo=wine$Group
# rownames(X)=paste(1:45, grupo, sep="-")
# pcaboot=PCA.Bootstrap(X, dimens=2, Scaling = "Standardize columns", B=1000)
# plot(pcaboot, ColorInd=as.numeric(grupo))
# summary(pcaboot)

pcaboot

as.numeric(wine$Group)
