
library(igraph)
library(reshape2)
library(dplyr)
library(ggforce)
source("C:/Users/julie.wisch/Documents/MegaCorr/MegaCorr_Funcs_DIAN.R")
##########################################################################################################
df.imputed.scaled<-read.csv("C:/Users/julie.wisch/Documents/MegaCorr/ImputedValues_DIAN.csv")
df<-df.imputed.scaled[,11:20]

##########################################################################################################
##########################################################################################################
##########################################################################################################

M<-df.imputed.scaled[0, 11:20]
p<-M
for(i in 11:20){
  for(j in 11:20){
    M[i-10, j-10]<- cor.test(df.imputed.scaled[,i], df.imputed.scaled[,j], na.rm = TRUE, method = "spearman")$estimate 
    p[i-10, j-10]<- cor.test(df.imputed.scaled[,i], df.imputed.scaled[,j], na.rm = TRUE, method = "spearman")$p.value 
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
#Doing hierarchical cluster analysis
ic <- iclust(df.imputed.scaled[,c(11:20)]) #Clustering produces 2 clusters

iclust.diagram(ic)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
#Generating Spring Plots
#df<-df.imputed.scaled[df.imputed.scaled$CDR > 0,]

df<-df[complete.cases(df),]

net<-SpringPlotSetUp(df.imputed.scaled)

l <- layout_with_fr(net)
plot(net, layout=l)

##Grabbing the stuff and moving it around myself
tkid <- tkplot(net) #tkid is the id of the tkplot that will open

l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot

tk_close(tkid, window.close = T)

#outputting the graph theory figure
pdf("C:/Users/julie.wisch/Documents/MegaCorr/MegaCorrAllCDR_DIAN.pdf")
plot(net, layout=l)
legend(x=-1.5, y=-1.1, c("Amyloid","Tau", "Neurodegeneration", "Cognition"), pch=21,
        col="#777777", pt.bg=c( "tomato", "gold", "gray50","white"), pt.cex=2, cex=.8, bty="n", ncol=1)
dev.off()

png("C:/Users/julie.wisch/Documents/MegaCorr/MegaCorrAllCDR_DIAN.png")
plot(net, layout=l)
legend(x=-1.5, y=-1.1, c("Amyloid","Tau", "Neurodegeneration", "Cognition"), pch=21,
       col="#777777", pt.bg=c( "tomato", "gold", "gray50","white"), pt.cex=2, cex=.8, bty="n", ncol=1)
dev.off()


##########################################################################################################
#Prepping the dataframe to generate the circle plots

cor_mat<-cor(df.imputed.scaled[,c(11:20)])
colnames(cor_mat)<-c("AB42", "pTau", "Tau", "PIB", "FDG", "CortVol", "HippoVol", "WMH", "FC", "PACC")
rownames(cor_mat)<-names(cor_mat)
diag(cor_mat) <- 0
forPlots <- melt(cor_mat)

forPlots$Var1<-rep(c("AB42", "pTau", "Tau", "PIB", "FDG", "CortVol", "HippoVol", "WMH", "FC", "PACC"), 10)

forPlots$Fill<-rep(c("tomato4", "gold3", "grey85", "tomato2", "grey78", "grey71", "grey64", "grey57", 
                     "grey50", "white"), 10)
  

forPlots <- forPlots %>%
  # rearrange the df in the order we want (1,2,3,4,5)
 mutate(Var1 = factor(Var1, unique(Var1))) %>% # this line reorders the factor in the same order
 mutate(Fill = factor(Fill, unique(Fill)))
##########################################################################################################



# CreateCirclePlot("AB42", "CSF AB42")
# CreateCirclePlot("Tau", "CSF Tau")
# CreateCirclePlot("pTau", "CSF pTau")
# CreateCirclePlot("PIB", "PET - PIB")
# CreateCirclePlot("AV1451", "PET - AV1451")
# CreateCirclePlot("FDG", "PET- FDG")
# CreateCirclePlot("CortVol", "Cortical Volume")
# CreateCirclePlot("HippoVol", "Hippocampus\nVolume")
# CreateCirclePlot("WMH", "WMH")
# CreateCirclePlot("FC", "Resting State")
# CreateCirclePlot("PACC", "PACC")
# 
# 
# 
# CreateBarPlot("AB42")
# CreateBarPlot("WMH")

library("png")


l<-rbind(c(1, 2, 2, 2), c(1, 2, 2, 2), c(1, 2, 2, 2), c(3, 4, 4, 4))

p<-GenerateBigPlot("Tau", "pTau", "CSF tTau", "CSF pTau")
pdf("C:/Users/julie.wisch/Documents/MegaCorr/MegaCorrAllCDR_DIAN_Correlation.pdf")
grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], layout_matrix = l)

dev.off()
