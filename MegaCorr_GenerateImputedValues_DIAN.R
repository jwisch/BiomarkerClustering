library(corrplot)
library(mice)
library(UpSetR)
library(tableone)
df<-read.csv("C:/Users/julie.wisch/Documents/MegaCorr/DIAN_cleaned.csv")

df<-df[df$Mutation.x == 1,]
#####################################################################
#Visualizing - checking for correlations between all the measures
M<-df[0, 10:19]
p<-M
for(i in 10:19){
  for(j in 10:19){
    M[i-9, j-9]<- cor.test(df[,i], df[,j], na.rm = TRUE, method = "spearman")$estimate 
    p[i-9, j-9]<- cor.test(df[,i], df[,j], na.rm = TRUE, method = "spearman")$p.value 
  }
}
row.names(M)<-names(M)

#This creates a correlation plot for ONLY the real values that we have. Skips missing values. Uses spearman.
corrplot(as.matrix(M), order = "hclust",  p.mat = as.matrix(p), insig = "blank", type = "upper")

rm(M, p, i, j)
#####################################################################

#####################################################################
#Visualizing what we've got....
VENN<-df[,c(10:19)]
VENN[!is.na(VENN)] <- 1
VENN[is.na(VENN)] <- 0

VENN<-data.frame(cbind(df$newid11, VENN))
colnames(VENN)[1]<-"ID"

upset(VENN, nsets = 9, mainbar.y.label = "Number of Participants", sets.x.label = "Measurement Counts", order.by = "freq")
df$apoe4<-ifelse(df$apoe == 22 | df$apoe == 23 | df$apoe == 33, 0, 1)
df$CDRlevels<-ifelse(df$cdrglob == 0, "0", ifelse(df$cdrglob == 0.5, "0.5", "1+"))
CreateTableOne(vars = c("visitage", "gender", "apoe4", "DIAN_EYO"), 
               data = df, factorVars = c("gender", "apoe4"), strata = "CDRlevels")

rm(VENN)
#####################################################################

md.pairs(df[,c(6, 10:19)])$rr

#Getting the proportion of usable cases for predictions
p <- md.pairs(df[,c(6, 10:19)])
round(p$mr/(p$mr + p$mm), 3)

imp <- mice(df[,c(6, 10:19)], seed = 23109, m = 5)
print(imp)


pred<-imp$predictorMatrix


imp50 <- mice(df[,c(6, 10:19)], seed = 23109, m = 50, pred = pred, minpuc = 0.20,
              include = "visitage") #Only using value for prediction if at least 20% of the things are usable
print(imp50)

plot(imp50, "FDGSummary")
densityplot(imp50, scales = list(x = list(relation = "free")),
            layout = c(5, 1))
com <- mice::complete(imp50, "long", include = TRUE)
com$ID<-rep(df$newid11, length(com$visitage)/length(df$newid11))

com<-com[complete.cases(com),]

df.imputed<-aggregate(com[, 3:13], list(com$ID), mean)
colnames(df.imputed)[1]<-"ID"
df.imputed<-merge(df[,c(1:3, 5:8, 20:21)], df.imputed, by.x = "newid11", by.y = "ID")



#Now creating a corrplot for the imputed values
#Visualizing - checking for correlations between all the measures
M<-df.imputed[0, 11:20]
p<-M
for(i in 11:20){
  for(j in 11:20){
    M[i-10, j-10]<- cor.test(df.imputed[,i], df.imputed[,j], na.rm = TRUE, method = "spearman")$estimate 
    p[i-10, j-10]<- cor.test(df.imputed[,i], df.imputed[,j], na.rm = TRUE, method = "spearman")$p.value 
  }
}
row.names(M)<-names(M)

#This creates a correlation plot for ONLY the real values that we have. Skips missing values. Uses spearman.
corrplot(as.matrix(M), order = "hclust",  p.mat = as.matrix(p), insig = "blank", type = "upper")
#GREAT news. looks pretty much the same, except that some of the correlations are stronger

rm(M, p, i, j)

# library("ggdendro")
# library(tidyverse)  # data manipulation
# library(cluster)    # clustering algorithms
# library(factoextra) # clustering visualization
# library(dendextend) # for comparing two dendrograms
df.imputed.scaled<-df.imputed

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
df.imputed.scaled<-df.imputed.scaled %>% mutate_at(names(df.imputed.scaled[c(11:20)]), scale2)

write.csv(df.imputed.scaled, "C:/Users/julie.wisch/Documents/MegaCorr/ImputedValues_DIAN.csv", row.names = FALSE)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################