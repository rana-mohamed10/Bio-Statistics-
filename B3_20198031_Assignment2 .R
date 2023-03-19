#if (!requireNamespace("BiocManager", quietly = TRUE))
 # install.packages("BiocManager")
#BiocManager::install()

#BiocManager::install("antiProfilesData")

#Quesion -1
datasetName <- antiProfilesData::apColonData


fdata <- fData(datasetName) #feature data

pdata <- pData(datasetName) #phenotype data

edata <- as.data.frame(exprs(datasetName)) #expression data

#to show the type of each column
class(fdata)
class(pdata)
class(edata)

#names of columns and rows

colnames(fdata)
row.names(fdata)

colnames(pdata)
row.names(pdata)

colnames(edata)
row.names(edata)
#another way to get names of columns and rows
dimnames(fdata)
dimnames(pdata)
dimnames(edata)


#Calculate summary of each column
summary(fdata)
summary(pdata)
summary(edata)

#Show frequency of categorical data, taking into the consideration, NA values frequency if any.

freqof_p_category<- Filter(is.character,pdata)
table(freqof_p_category,useNA="ifany")


#Calculate the correlation and covariance between the first 10 columns only of our data set and draw full correlation matrix.

library("corrplot")
cov(edata[,1:10], y = NULL, use = "everything", method = "spearman")

e_cor<-cor(edata[,1:10], y = NULL,  method = "spearman")
corrplot(e_cor,type = "upper",tl.col = "purple")

col <-colorRampPalette(c("red","white","lightblue"))(20)
heatmap(e_cor,col=col,symm = TRUE)

#For both genes: GSM95478,GSM95473 show the plot with a line of their relation.

gene1<- edata[,"GSM95478"]

gene2<-edata[,"GSM95473"]

lM<-lm(gene2~gene1)
tidy(lM)

plot(gene1,gene2,col="lightblue")
abline(lM,col="pink",lwd=5)


#Question 2

#Using PCA and SVD, Prove by plotting and values that both can return the same result by suitable normalization


#before
edata_centered <- edata - rowMeans(edata)
svd1 <- svd(edata_centered)
names(svd1)
pca1 <- prcomp(edata)
plot(pca1$rotation[, 1], svd1$v[, 1],col="blue")
pca1$rotation[, 1]
svd1$v[, 1]

#after normalization
#the colMeans is the right normalization
edata_centered2 <- t(t(edata) - colMeans(edata))
svd2 <- svd(edata_centered2)
names(svd1)
pca1 <- prcomp(edata)
plot(pca1$rotation[, 1], svd2$v[, 1],col="blue")
pca1$rotation[, 1]
svd2$v[, 1]


#Question 3

zodicsign <- as.factor(c(rep("Aries" ,29),rep("Taurus",24) , rep("Gemini",22), rep("Cancer",19), rep("Leo",21), rep("Vigro",18), rep("Libra",19), rep("Scorpio",20), rep("Sagittarius",23), rep("Capricorn",18), rep("Aquarius",20), rep("Pisces",23)))

p <- c(1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12)
p<- p/sum(p)
chisq.test(table( zodicsign),p=p)
#H0 : accepted 
#H1 : rejected 
# as the P-Value is 0.9265 > "the 0.05 significance level "so we will accept null hypothesis "H0" and reject the "H1" 
#And if it's less than 0.05 we will accept the "H1" and reject the null hypothesis "H0"

#Question 4

#Plot hierarchical clusters on our first 10 columns of edata and apply the kmeans to all the edata columns and show the centroid of the result.
par(mfrow=c(1,1))
mydist <- dist(t(edata[,1:10]))
hclust<-hclust(mydist)
plot(hclust,hang = -1)


km<-kmeans(edata,centers=3)
centroid<-km$centers
centroid
clusters<-km$cluster
table(clusters)












