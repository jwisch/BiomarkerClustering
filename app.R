library(plotly)
df.imputed.scaled<-read.csv("ImputedValues_ADRC_ForShiny_WithFlags_andplasma_noNFL.csv")
# colnames(df.imputed.scaled[,18:35])<-c("CSF AB42", "CSF tTau", "CSF pTau", "VILIP", "Neurogranin",
#                      "SNAP25", "YKL40", "log(NFL)", "sTREM2", "PET AV45", "PET PIB", 
#                      "PET AV1451", "FDG", "Cortical Thickness", "Hippocampal Volume",
#                      "White Matter Hyperintensity", "Resting State Summary", "PACC")
df.corr<-df.imputed.scaled[,18:35]
correlation<-round(cor(df.corr, method = "spearman"), 3)


ui <- fluidPage(
  #titlePanel("Alzheimer Disease Biomarker Correlations"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("typeInput", "Cognitive Status",
                   choices = c("All" = "All",
                               "CDR = 0, Amyloid Negative" = "CogNorm_Aneg" ,
                               "CDR = 0, Amyloid Positive" = "CogNorm_Apos",
                               "CDR > 0, Amyloid Negative" = "SNAP",
                               "CDR > 0, Amyloid Positive" = "CogImp"),
                   selected = "All"),
      radioButtons("typeInput2", "Type of Data Shown",
                   choices = c("All" = "All", "Actual Collected Values Only" = "1"),
                   selected = "All")),
    mainPanel(   plotlyOutput("heat"),
                 
                 plotlyOutput("scatterplot"))))
  
  server <- function(input, output, session) {
    
    output$heat <- renderPlotly({
      plot_ly(colors = "RdBu", source = "heat_plot") %>%
      #source = "heat_plot") %>%
        add_heatmap(
          x = names(df.corr),
          y = names(df.corr),
          z = correlation
        )
    })
    
    
    
    output$scatterplot <- renderPlotly({
      
      # if there is no click data, render nothing!
      clickData <- event_data("plotly_click", source = "heat_plot")
      if (is.null(clickData)) return(NULL)
      
      # Obtain the clicked x/y variables and fit linear model
      vars <- c(clickData[["x"]], clickData[["y"]], 
                paste("Real_", clickData[["x"]], sep = ""),
                paste("Real_", clickData[["y"]], sep = ""))  
      
      #print(vars)

      
      ifelse(input$typeInput == "All", df.scatter<-df.imputed.scaled,
             df.scatter<-df.imputed.scaled[df.imputed.scaled$Group == input$typeInput,])
      d <- setNames(df.scatter[vars], c("x", "y", "xFlag", "yFlag"))
      d$Flag<-paste(d$xFlag, d$yFlag, sep = "-")
      
      #if(input$typeInput2 == "0"){d<-d[d$Flag == "1-1",]}
      ifelse(input$typeInput2 == "All", d<-d, d<-d[d$Flag == "1-1",])

      
      #print(head(d))
      
      
      #yhat <- fitted(lm(y ~ x, data = d))

      # scatterplot with fitted line
      ggplot(d, aes(x = x, y = y)) + geom_point() + geom_smooth(method = "lm", colour = "grey19") +
        xlab(as.character(clickData[["x"]])) + ylab(as.character(clickData[["y"]])) + theme_classic()
      # plot_ly(d, x = ~x) %>%
      #   add_markers(y = ~y) %>%
      #   add_lines(y = ~yhat) %>%
      #   layout(
      #     xaxis = list(title = clickData[["x"]]),
      #     yaxis = list(title = clickData[["y"]]),
      #     showlegend = FALSE
      #   )
    })
    
  }
  
  shinyApp(ui, server)

  
  
      