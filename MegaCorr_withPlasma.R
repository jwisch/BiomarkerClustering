
library(igraph)
library(reshape2)
library(dplyr)
library(ggforce)
library(psych)
library(corrplot)

FILEPATH_CODE<-"C:/Users/julie.wisch/Documents/MegaCorr/"
FILEPATH_DATA<-"C:/Users/julie.wisch/Documents/MegaCorr/"
source(paste(FILEPATH_CODE, "MegaCorr_Funcs.R", sep = ""))
##########################################################################################################
df.imputed.scaled<-read.csv(paste(FILEPATH_DATA, "ImputedValues_withPlasma.csv", sep = ""))


#Creating Flags for imputed values.
Original<-read.csv(paste(FILEPATH_DATA, "ADRC_cleaned_withplasma.csv", sep = ""))
Original[is.na(Original)] <- 0
Original[,18:36] <- ifelse(Original[,18:36] == 0, 0, 1)
Original<-Original[,c("ID", "LP_date", "Plasma_Ab42_Ab40", "E_ab42", "E_tau", "E_ptau", "VILIP1", "NGRN", "SNAP25", "YKL40",
                      "NFL", "sTREM2", "AV45_fSUVR_rsf_TOT_CORTMEAN", "PIB_fSUVR_rsf_TOT_CORTMEAN",
                      "Tauopathy", "FDGSummary", "ADsig", "HippoVol", "WMH_Volume", "FCSummary", "PACC")]

colnames(Original)[3:21] <- paste("Real", colnames(Original[3:21]), sep = "_")

df.imputed.scaled<-merge(df.imputed.scaled, Original, by = c("ID", "LP_date"), all = FALSE)


df<-df.imputed.scaled[,c("Plasma_Ab42_Ab40", "E_ab42", "E_tau", "E_ptau", "VILIP1", "NGRN", "SNAP25", "YKL40",
                         "NFL", "sTREM2", "AV45_fSUVR_rsf_TOT_CORTMEAN", "PIB_fSUVR_rsf_TOT_CORTMEAN",
                         "Tauopathy", "FDGSummary", "ADsig", "HippoVol", "WMH_Volume", "FCSummary", "PACC",
                         "Real_Plasma_Ab42_Ab40",
                         "Real_E_ab42", "Real_E_tau","Real_E_ptau","Real_VILIP1" ,"Real_NGRN","Real_SNAP25",
                         "Real_YKL40","Real_NFL","Real_sTREM2","Real_AV45_fSUVR_rsf_TOT_CORTMEAN",
                         "Real_PIB_fSUVR_rsf_TOT_CORTMEAN","Real_Tauopathy","Real_FDGSummary",
                         "Real_ADsig","Real_HippoVol","Real_WMH_Volume","Real_FCSummary","Real_PACC")]

df.imputed.scaled$PIBpos<-ifelse(df.imputed.scaled$E_ptau/df.imputed.scaled$E_ab42 >0.0198, 1, 0)
df.imputed.scaled$Group<-as.factor(paste(df.imputed.scaled$CDR, df.imputed.scaled$PIBpos, sep = "-"))
levels(df.imputed.scaled$Group)<-c("CogNorm_Aneg", "CogNorm_Apos", "SNAP", "CogImp", "SNAP", "CogImp",
                                   "NoCDR_Aneg", "NoCDR_Apos")

write.csv(df.imputed.scaled, paste(FILEPATH_DATA, "ShinyApp/ImputedValues_ADRC_ForShiny_WithFlags_andplasma.csv", sep = ""), row.names = FALSE)
##########################################################################################################
##########################################################################################################
##########################################################################################################

M<-df.imputed.scaled[0, 18:35]
colnames(M)[10:11]<-c("AV45", "PIB")
p<-M
for(i in 18:35){
  for(j in 18:35){
    M[i-17, j-17]<- cor.test(df.imputed.scaled[,i], df.imputed.scaled[,j], na.rm = TRUE, method = "spearman")$estimate 
    p[i-17, j-17]<- cor.test(df.imputed.scaled[,i], df.imputed.scaled[,j], na.rm = TRUE, method = "spearman")$p.value 
  }
}
row.names(M)<-names(M)

#This creates a correlation plot for ONLY the real values that we have. Skips missing values. Uses spearman.
corrplot(as.matrix(M), order = "hclust",  p.mat = as.matrix(p), insig = "blank", type = "upper")
#GREAT news. looks pretty much the same, except that some of the correlations are stronger

rm(M, p, i, j)
##########################################################################################################
##########################################################################################################
##########################################################################################################
#Set which cognitive group you're interested in.
#Choices are: CogNorm_Aneg  CogNorm_Apos  SNAP  CogImp  NoCDR_Aneg  NoCDR_Apos
#Or don't subset and just view it for everything

GROUP<-"CogNorm_Apos"

#Doing hierarchical cluster analysis
ic <- iclust(df.imputed.scaled[df.imputed.scaled$Group == GROUP,c(18:36)],
             n.iterations = 10) #Clustering produces 2 clusters

iclust.diagram(ic)



##########################################################################################################

#Generating Spring Plots

#Testing how many significant clusters there are in the spring embedded plots
screeplot(prcomp(df.imputed.scaled[df.imputed.scaled$Group == GROUP,c(18:36)]))
#Kind of up to judgement
#My opinion...CogNorm_Aneg = 2 groups, Cognorm_Apos = 2 groups, SNAP = 2...maybe 3 groups, CogImp= 3 groups for sure
#Everyone...could make case for 1, 2, or 3 groups.


GeneratePDF<-function(GROUP){
  df<-df.imputed.scaled[df.imputed.scaled$Group == GROUP,]
  #df<-df.imputed.scaled
  df<-df[complete.cases(df[,c(18:36)]),]
  
  net<-SpringPlotSetUp_plusplasma(df)
  
  l <- layout_with_fr(net)
  
  ##Grabbing the stuff and moving it around myself
  # tkid <- tkplot(net) #tkid is the id of the tkplot that will open
  # l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
  # tk_close(tkid, window.close = T)
  
  #outputting the graph theory figure
  pdf(paste(FILEPATH_DATA, "ADRC_MegaCorr", GROUP, ".pdf", sep= ""))
  plot(net, layout=l, vertex.label.cex= 0.6)
  legend(x=-1.5, y=-1.1, c("Amyloid","Tau", "Neurodegeneration", "Cognition"), pch=21,
         col="#777777", pt.bg=c( "tomato", "gold", "gray50","white"), pt.cex=2, cex=.8, bty="n", ncol=1)
  dev.off() }

levels(df.imputed.scaled$Group)
GeneratePDF("CogNorm_Aneg")
GeneratePDF("CogNorm_Apos")
GeneratePDF("SNAP")
GeneratePDF("CogImp")
GeneratePDF("NoCDR_Aneg")
GeneratePDF("NoCDR_Apos")

##########################################################################################################
