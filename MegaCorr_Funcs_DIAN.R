CreateCirclePlot<-function(VARIABLE, LABEL){
  forPlots_AB42<-forPlots[forPlots$Var2 == VARIABLE,]
  forPlots_AB42$value<-abs(forPlots_AB42$value)
  forPlots_AB42$Percentage<-(forPlots_AB42$value / sum(forPlots_AB42$value)) * 100
  forPlots_AB42<-forPlots_AB42[with(forPlots_AB42, order(-Percentage)),]
  forPlots_new<-data.frame(forPlots_AB42[1:7,])
  forPlots_new$Var1<-as.character(forPlots_new$Var1)
  forPlots_new[length(forPlots_new$Var1) + 1,"Var1"]<-"Other"
  forPlots_new[length(forPlots_new$Var1) ,"Var2"]<-forPlots_new$Var2[1]
  forPlots_new[length(forPlots_new$Var1),"Percentage"]<- (100 - sum(forPlots_new$Percentage, na.rm = TRUE))
  forPlots_new$Percentage<-round(forPlots_new$Percentage, 0)
  forPlots_new$Fill<-as.character(forPlots_new$Fill)
  forPlots_new[length(forPlots_new$Var1), "Fill"] <- "black"
  
  forPlots_new <- forPlots_new %>%
    # rearrange the df in the order we want (1,2,3,4,5)
    mutate(Var1 = factor(Var1, (Var1))) %>% # this line reorders the factor in the same order
    mutate(Fill = factor(Fill, (Fill)))
  
  # forPlots_new$Var1<-as.factor(forPlots_new$Var1)
  # forPlots_new$Var1 = factor(forPlots_new$Var1,levels(forPlots_new$Var1)[c(1:4, 6:8, 5)])
  # forPlots_new$Fill<-as.factor(forPlots_new$Fill)
  
  
  p<-ggplot(forPlots_new ) +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1, amount = Percentage, fill = Var1), alpha = 0.8, stat = "pie") + 
    scale_fill_manual(values = as.character(forPlots_new$Fill))+
    annotate("text", x = 0, y = 0.15, label = "Parameters most\ncorrelated with") +
    annotate("text", x = 0, y = -0.15, label = LABEL, size = 8) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(), legend.position = "bottom", legend.title = element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  
  return(p)}


SpringPlotSetUp<-function(DF){
  cor_mat<-cor(DF[,c(11:20)])
  colnames(cor_mat)<-c("AB42", "pTau", "Tau", "PIB", "FDG", "CortVol", "HippoVol", "WMH", "FC", "PACC")
  rownames(cor_mat)<-names(cor_mat)
  diag(cor_mat) <- 0
  forPlots <- melt(cor_mat)
  cor_mat[cor_mat > -0.5 & cor_mat < 0.5]<-0
  
  
  
  cor_mat[upper.tri(cor_mat)] <- 0
  my_cor_df <- melt(cor_mat)
  my_cor_df$Var1<-rep(c("AB42", "pTau", "Tau", "PIB", "FDG", "CortVol", "HippoVol", "WMH", "FC", "PACC"), 10)
  forLines<-melt(cor_mat)
  
  
  
  my_cor_df$value<-abs(my_cor_df$value)
  my_cor_df<-my_cor_df[my_cor_df$ value > 0,]
  
  my_adj_list <- my_cor_df %>% filter(value > 0.50)
  names(my_adj_list)<-c("from", "to", "weight")
  net <- graph.data.frame(my_cor_df, directed = FALSE)
  
  NAMES<-data.frame("Names" = as.character(names(V(net))), "Colors" = NA)

  
  colors<-data.frame("Names" = as.character(c("AB42", "pTau", "Tau", "PIB", "FDG", "CortVol", "HippoVol", "WMH", "FC", "PACC")),
                     "Colors" = as.character(c("tomato4", "gold3", "grey85", "tomato2", "grey78", "grey71", "grey64", "grey57", 
                                               "grey50", "white")))
  
NAMES$Names<-as.character(NAMES$Names)
NAMES$Colors<-as.character(NAMES$Colors)
colors$Names<-as.character(colors$Names)  
colors$Colors<-as.character(colors$Colors)

index<-as.vector(NA)
for(i in 1:length(NAMES$Names)){
  for(j in 1:length(colors$Colors)){
    index[j]<-NAMES[i, 1] == colors[j, 1]
    
  }
  NAMES[i, 2]<-colors[which(index == TRUE)[[1]], "Colors"]
  }
  
  colrs <- c("gray50", "tomato", "gold", "white")

  V(net)$color <- as.character(NAMES$Colors)
  
  # Set node size based on audience size:
  
  V(net)$size <- 30
  V(net)$label.color <- "black"
  V(net)$label.size <- 6
  
  
  #change arrow size and edge color:
  
  E(net)$arrow.size <- .2
  forLines<-forLines[forLines$value > 0.5 | forLines$value < -0.5,]
  forLines$Color<-ifelse(forLines$value < 0, "grey", "black")
  E(net)$edge.color <- forLines$Color
  
  #Black for positive correlation, grey for negative correlation
  E(net)$color <- forLines$Color
  E(net)$width <- c(my_cor_df$value*5)
  
  
  return(net)}

CreateBarPlot<-function(NAME, ORIENT = "X"){
  hold<-forPlots[forPlots$Var2 == NAME,]
  hold<-hold[!(hold$value == 0),]
  p<-ggplot(hold, aes(x = reorder(Var1, -value), y = value, fill = Var1)) + geom_bar(stat = "identity") +
    scale_fill_manual(values = as.character(hold$Fill))+
    ylim(c(-0.87, 0.87)) + xlab("") + ylab("") 
  ifelse(ORIENT == "X", 
         p<-p +
    theme(
      panel.background = element_rect(fill = "black",
                                      colour = "black"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "black"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
      
    ),  p<-p +
      theme(
        panel.background = element_rect(fill = "black",
                                        colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")+ coord_flip())
  return(p)}


GenerateBigPlot<-function(PARAM1, PARAM2, Label1, Label2){
  
  p1<-CreateBarPlot(PARAM2, "Y") 
  p2<-ggplot(df.imputed.scaled, aes(x = df.imputed.scaled[,PARAM1], y = df.imputed.scaled[,PARAM2])) + geom_point(colour = "black") + geom_smooth(method = "lm", colour= "grey37")+
    xlab(Label1) + ylab(Label2) + theme(legend.position= "none", 
                                        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                        colour = "white"), 
                                        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                        colour = "white"))
  p4<-CreateBarPlot(PARAM1, "X")
  
  p3<-ggplot(data = forPlots, aes(x = Var1, y = Var2))+ 
    geom_label(aes(x = 0.8, y = 0.33, label = "CSF AB42"), 
               fill = "tomato4", label.size = NA, size = 2) +
    geom_label(aes(x = 0.8, y = 0.66, label = "PIB"), 
               fill = "tomato2", label.size = NA, size = 2) +
    geom_label(aes(x = 0.8, y = 1, label = "CSF pTau"), 
               fill = "gold3", label.size = NA, size = 2) +
    geom_label(aes(x = 0.8, y = 1.33, label = "PACC"), 
               fill = "white", label.size = NA, size = 2) +
    geom_label(aes(x = 0.8, y = 1.66, label = "FC"), 
               fill = "grey50", label.size = NA, size = 2) +
    geom_label(aes(x = 1.7, y = 0.33, label = "CSF tTau"), 
               fill = "grey85", label.size = NA, size = 2) +
    geom_label(aes(x = 1.7, y = 0.66, label = "FDG"), 
               fill = "grey78", label.size = NA, size = 2) +
    geom_label(aes(x = 1.7, y = 1, label = "Cortical Vol"), 
               fill = "grey71", label.size = NA, size = 2) +
    geom_label(aes(x = 1.7, y = 1.33, label = "Hippocampus"), 
               fill = "grey64", label.size = NA, size = 2) +
    geom_label(aes(x = 1.7, y = 1.66, label = "WMH"), 
               fill = "grey57", label.size = NA, size = 2) + xlim(c(0.4, 2.1)) + ylim(c(0, 2)) +
    theme(panel.background = element_rect(fill = "black",
                                          colour = "black"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "black"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "black"),
          axis.text = element_blank(),
          axis.ticks = element_blank()) + xlab("") + ylab("")
  
  return(list(p1, p2, p3, p4))}