library(corrplot)
library(mice)
library(UpSetR)
library(tableone)
library(dplyr)
FILEPATH_DATA<-"C:/Users/julie.wisch/Documents/MegaCorr/"
df<-read.csv(paste(FILEPATH_DATA, "ADRC_cleaned_withplasma.csv", sep = ""))
df$NFL<-log(df$NFL)

#####################################################################
#Visualizing - checking for correlations between all the measures
M<-df[0, 18:36]
colnames(M)[10:11]<-c("AV45", "PIB")
p<-M
for(i in 18:36){
  for(j in 18:36){
    M[i-17, j-17]<- cor.test(df[,i], df[,j], na.rm = TRUE, method = "spearman")$estimate 
    p[i-17, j-17]<- cor.test(df[,i], df[,j], na.rm = TRUE, method = "spearman")$p.value 
  }
}
row.names(M)<-names(M)

#This creates a correlation plot for ONLY the real values that we have. Skips missing values. Uses spearman.
corrplot(as.matrix(M), order = "hclust",  p.mat = as.matrix(p), insig = "blank", type = "upper")

rm(M, p, i, j)
#####################################################################

#####################################################################
#Visualizing what we've got....
VENN<-df[,c(18:36)]
VENN[!is.na(VENN)] <- 1
VENN[is.na(VENN)] <- 0

VENN<-data.frame(cbind(df$ID, VENN))
colnames(VENN)[1]<-"ID"

upset(VENN, nsets = 18, mainbar.y.label = "Number of Participants", sets.x.label = "Measurement Counts", order.by = "freq")
df$apoe4<-ifelse(df$apoe == "22" | df$apoe == "23" | df$apoe == "33", 0, 1)
CreateTableOne(vars = c("AgeatLP", "GENDER", "EDUC", "apoe4", "race2"), 
               data = df, factorVars = c("GENDER", "apoe4", "race2"), strata = "CDR")

rm(VENN)
#####################################################################

md.pairs(df[,c(8, 18:36)])$rr

#Getting the proportion of usable cases for predictions
p <- md.pairs(df[,c(8, 18:36)])
round(p$mr/(p$mr + p$mm), 3)

imp <- mice(df[,c(8, 18:36)], seed = 23109, m = 5)
print(imp)

xyplot(imp, SNAP25 ~ YKL40 | .imp, pch = 20, cex = 1.4)

pred<-imp$predictorMatrix
pred[,c("FDGSummary")]<-0 #Not using FDG for predictions as relatively few people actually have it




imp50 <- mice(df[,c(8, 18:36)], seed = 23109, m = 50, pred = pred, minpuc = 0.20,
              include = "AgeatLP") #Only using value for prediction if at least 20% of the things are usable
print(imp50)

#This is how you check how doing multiple rounds of MICE causes values to converge/variance to decrease
plot(imp50, "VILIP1")
densityplot(imp50, scales = list(x = list(relation = "free")),
            layout = c(5, 1))

#Getting all of the mice algorithm results
com <- mice::complete(imp50, "long", include = TRUE)
com$ID<-rep(df$ID, length(com$AgeatLP)/length(df$ID))

com<-com[complete.cases(com),]

df.imputed<-aggregate(com[, 3:22], list(com$ID), mean)
colnames(df.imputed)[1]<-"ID"
df.imputed<-merge(df[,c(1:7, 9:17, 37)], df.imputed, by = "ID")
df.imputed<-df.imputed[,names(df)]



#Now creating a corrplot for the imputed values
#Visualizing - checking for correlations between all the measures
M<-df.imputed[0, 18:36]
colnames(M)[10:11]<-c("AV45", "PIB")
p<-M
for(i in 18:36){
  for(j in 18:36){
    M[i-17, j-17]<- cor.test(df.imputed[,i], df.imputed[,j], na.rm = TRUE, method = "spearman")$estimate 
    p[i-17, j-17]<- cor.test(df.imputed[,i], df.imputed[,j], na.rm = TRUE, method = "spearman")$p.value 
  }
}
row.names(M)<-names(M)

#This creates a correlation plot for ONLY the real values that we have. Skips missing values. Uses spearman.
corrplot(as.matrix(M), order = "hclust",  p.mat = as.matrix(p), insig = "blank", type = "upper")
#GREAT news. looks pretty much the same, except that some of the correlations are stronger

rm(M, p, i, j)


df.imputed.scaled<-df.imputed

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

df.imputed.scaled<-df.imputed %>% mutate_at(names(df.imputed[c(19:35)]), scale2)

#Generates the imputed values dataframe and writes it out so it can be used in analysis
write.csv(df.imputed.scaled, paste(FILEPATH_DATA, "ImputedValues_withplasma.csv" sep = ""), row.names = FALSE)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################