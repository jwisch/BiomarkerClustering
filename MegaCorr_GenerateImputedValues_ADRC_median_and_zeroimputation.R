library(corrplot)
library(UpSetR)
library(tableone)

FILEPATH_DATA<-"C:/Users/julie.wisch/Documents/MegaCorr/"

df<-read.csv(paste(FILEPATH_DATA, "ADRC_cleaned.csv", sep = ""))
df$NFL<-log(df$NFL)

#####################################################################
#Visualizing - checking for correlations between all the measures
M<-df[0, 18:35]
colnames(M)[10:11]<-c("AV45", "PIB")
p<-M
for(i in 18:35){
  for(j in 18:35){
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
VENN<-df[,c(18:35)]
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
library(mice)
library(dplyr)
md.pairs(df[,c(8, 18:35)])$rr

#Getting the proportion of usable cases for predictions
p <- md.pairs(df[,c(8, 18:35)])
round(p$mr/(p$mr + p$mm), 3)

df.imputed<-df[,18:35] %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))

df.imputed<-data.frame(cbind(df[,1:17], df.imputed, df[,36]))
colnames(df.imputed)<-names(df)

#Now creating a corrplot for the imputed values
#Visualizing - checking for correlations between all the measures
M<-df.imputed[0, 18:35]
colnames(M)[10:11]<-c("AV45", "PIB")
p<-M
for(i in 18:35){
  for(j in 18:35){
    M[i-17, j-17]<- cor.test(df.imputed[,i], df.imputed[,j], na.rm = TRUE, method = "spearman")$estimate 
    p[i-17, j-17]<- cor.test(df.imputed[,i], df.imputed[,j], na.rm = TRUE, method = "spearman")$p.value 
  }
}
row.names(M)<-names(M)

#This creates a correlation plot for ONLY the real values that we have. Skips missing values. Uses spearman.
corrplot(as.matrix(M), order = "hclust",  p.mat = as.matrix(p), insig = "blank", type = "upper")
#Correlations are much weaker

rm(M, p, i, j)


df.imputed.median<-df.imputed

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
df.imputed.median<-df.imputed.median %>% mutate_at(names(df.imputed.median[c(18:29, 33)]), scale2)

write.csv(df.imputed.median, paste(FILEPATH_DATA, "ImputedValues_ADRC_Median.csv", sep = ""), row.names = FALSE)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
df.imputed<-df[,18:35]
df.imputed[is.na(df.imputed)] <- 0

df.imputed<-data.frame(cbind(df[,1:17], df.imputed, df[,36]))
colnames(df.imputed)<-names(df)

df.imputed.zero<-df.imputed

df.imputed.zero<-df.imputed.zero%>% mutate_at(names(df.imputed.zero[c(18:29, 33)]), scale2)

write.csv(df.imputed.zero, paste(FILEPATH_DATA, "ImputedValues_ADRC_zero.csv", sep = ""), row.names = FALSE)

M<-df.imputed.zero[0, 18:35]
colnames(M)[10:11]<-c("AV45", "PIB")
p<-M
for(i in 18:35){
  for(j in 18:35){
    M[i-17, j-17]<- cor.test(df.imputed.zero[,i], df.imputed.zero[,j], na.rm = TRUE, method = "spearman")$estimate 
    p[i-17, j-17]<- cor.test(df.imputed.zero[,i], df.imputed.zero[,j], na.rm = TRUE, method = "spearman")$p.value 
  }
}
row.names(M)<-names(M)

#This creates a correlation plot for ONLY the real values that we have. Skips missing values. Uses spearman.
corrplot(as.matrix(M), order = "hclust",  p.mat = as.matrix(p), insig = "blank", type = "upper")
#Correlations are much weaker

rm(M, p, i, j)
