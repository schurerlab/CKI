
library(shiny)
library(plotly)
library(plyr)
library(dplyr)
library(reshape2)
library(shinythemes)
library(survival)
library(data.table)
library(shinydashboard)
library(DT)
library(devtools)
library(superheat)
library(plotly)
library(highcharter)
library(survminer)
library(tidyr)
library(reshape2)
library(shinyLP)
library(crosstalk)
library(shinyjs)
#setwd("/TCGARecount2/ShinyApp/")
rank_data <- data.frame(readRDS("data/Kinase_full_information_for_App.rds"))

################
#ui
##################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Dark Kinome",
                           tabPanel("About",
                                    #img(src = "new_logo_transparent.png", width = 900),
                                    img(src = "logoCKI2.png", width = 900),
                                    tags$p(class = "lead", "Welcome to", strong("Clinical Kinome Index (CKI)"),": a web-app for exploring and visualizing clinical data from the TCGA datasets."),
                                    h4(class = "outer", "How to use the CKI?"),                                    
                                    tags$p(class = "outer"," Select the “Gene”:"),
                                    tags$ol(
                                      
                                      tags$li('Search for your kinase of interest,  tab and enter your kinase gene name; or use the drop down menu to select.'),
                                      tags$li('The query may be filtered by Target Development Level (TDL), Kinase Type, Kinase Phylogenic Group or Approved Target.'),
                                      tags$li('The results will be populated in a table which contain the kinase names and TCGA cancer. Results may be sorted by Kinase_Score to explore which cancers this kinase ranks high in.')
                                      
                                      
                                    ),
                                    
                                    tags$p(class = "outer","To search by “Disease”:"),
                                    
                                    tags$ol(
                                      
                                      tags$li('Select the “Disease” tab and select the TCGA cancer cohort on the drop down menu.'),
                                      tags$li('A Volcano plot will be generated which represents all differential genes for the selected cohort.'),
                                      tags$li('Selecting the “Study” Sub-tab will generate box-plots for the selected gene for T,N and M staging.'),
                                      tags$li('“Survival” will generate a KM survival plot for the selected gene.')
                                      
                                      
                                    ),
                                    
                                    br(),
                                    br(),
                                    br(),
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons("Kinase", "Choose differentially expressed gene set ", selected = NULL , choices = c("Understudied kinases*", "All kinases") ),
                                        br(),
                                        br(),
                                        tags$footer(tags$a("*IDG designated understudied kinases", align = "left", href="https://pharos.nih.gov/targets?facet=Collection%2FKinase%253A%2520IDG%2520Consortium%2520(Targets)", target="_blank", style = "position:absolute; absolute; width:100%; color: blue; height:50px;" ))),
                                    
                                      mainPanel(
                                        tags$p(class = 'text-center',strong("Differentially expressed kinases in TCGA")),
                                        plotlyOutput("heat", width = "100%", height = "500px"),
                                        br(),
                                        br(),
                                        downloadButton("downloadData", "Download Kinases Data"),
                                        br(),
                                        br(),
                                        br(),
                                        verbatimTextOutput("selected1")
                                        
                                        
                                        
                                      )
                                    )),
                                    
                                  
                              
                                                

                           
                           
                           ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
                           #### UI TAB2 #### 
                           ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
                          
                           tabPanel('Gene',
                                    fluidRow( 
                                      column(3,
                                             wellPanel( id = "GenePage",
                                               
                                                 selectInput("Gene", "Choose the Kinase", c("All", sort(unique(as.character(rank_data[,1])))), selected = "All", multiple = FALSE),
                                                 
                                                 # hr(),
                                                 # radioButtons("Kinase", "Status", selected = NULL , choices = c("Understudied", "Studied") ),
                                                 useShinyjs(),
                                                 hr(),
                                                 checkboxGroupInput("SelectTDL", "TDL", selected = NULL, choices = c("Tdark","Tbio", "Tclin", "Tchem" ) ),
                                                 hr(),
                                                 checkboxGroupInput("kinaseType",  label = "Kinase Type", choices = c("Protein kinases" = "protein kinase" ,"Non-protein kinase"), selected = NULL),
                                                 
                                                 hr(),
                                                 checkboxGroupInput("kinaseGroup",  label = "Kinase Group", choices = c("AGC group" ,"Atypical group", "CAMK group",  "Carbohydrate kinase" = "carbohydrate kinase", "CK1 group", "CMGC group", "Lipid kinase" = "lipid kinase", "Nucleoside/nucleotide kinase" = "nucleoside/nucleotide kinase","RGC group",  "STE group", "TK group", "TKL group", "Unclassified protein", "Other group" = "other group", "Other small molecule kinase" = "other small molecule kinase"), selected =  NULL ),
                                                 
                                                 hr(),
                                                 checkboxGroupInput("MOA",  label = "MOA Targets", choices = c("Yes" ,"No"), selected = NULL),
                                                 hr(),
                                                 
                                                 actionButton("reset_input", "Reset")
                                               #) 
                                               
                                             )
                                      ),   
                                      column(6, plotlyOutput("scatter")), #, textOutput("selected"))
                                      #column(8, DT::dataTableOutput("mytable2"), textOutput("selected")),
                                      column(8, dataTableOutput("mytable2"), textOutput("selected")),
                                      br(),
                                      
                                      column(8, tags$p(class = 'text-center', downloadButton('filt_table', 'Download Filtered Data'))
                          
                                    ))),
                           ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
                           #### UI TAB3 #### 
                           ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
                           #volcano plot https://2-bitbio.com/2017/12/clickable-volcano-plots-in-shiny.html                          
                           tabPanel('Disease',
                                    
                                    fluidPage(
                                      #sidebarPanel(
                                      fluidRow(
                                        column(3,
                                               wellPanel( id = "PlotsPage",
                                                 selectInput("Cancer", "Choose the Cancer", sort(unique(as.character(rank_data[,2]))), selected = "BLCA"),
                                                 br(),
                                                 
                                                 actionButton("reset_input1", "Reset")
                                               )),
      
                                        mainPanel(
                                          tabsetPanel( 
                                            tabPanel("Volcano", br(), br(), uiOutput("sd"),
                                                   
                                                     br(),
                                                     plotOutput('volcanoPlot',click='plot_click', height = 600), tableOutput('clickedPoints')),
                                            
                                            
                                            tabPanel("study", 
                                              
                                                     uiOutput("gg"),
                                                     
                                                     
                                                     br(),
                                                     br(),
                                                     #tableOutput('mytable3')
                                                     tags$p(class = 'text-center',strong("Boxplot showing Stage T")),
                                          
                                                    plotlyOutput('T_stage', height = "300px"),
                                                     #plotlyOutput('T_stage', width="100%")),
                                                     br(),
                                                     br(),
                                                     
                                                     hr(), 
                                                     
                                                     tags$p(class = 'text-center',strong("Boxplot showing Stage M")),
                                                     plotlyOutput('M_stage', width="100%"),
                                                     hr(), 
                                                    tags$p(class = 'text-center',strong("Boxplot showing Stage N")),
                                          
                                                     plotlyOutput('N_stage', width="100%")),
                                            
                                            
                                            tabPanel("survival", 
                                                     #selectInput("Gen", "Choose the gene", gen(), selected = NULL),
                                                     uiOutput("gg_s"),
                                                     br(),
                                                     br(),
                                                     tags$p(class = 'text-center', strong("Overall Survival Analysis (OS)")),
                                                     br(),
                                                     highchartOutput("Survival1") , width = "100%", click='plot_click1'), tableOutput('clickedPoints1')), 
                                            br(),
                                            br(),
                                            textOutput("selected_var"))
                                          
                                          
                                        
                                        
                                      )
                                    )
                           ),
                           
                           
                           tabPanel('Download Data',
                                    
                                    
                                    
                                    tags$p(class = "lead", "Welcome to", strong("Clinical Kinome Index (CKI)"),": a web-app for exploring and visualizing clinical data from the TCGA datasets."),
                                    fluidRow(class = "myRow1",
                                             panel_div(class_type = "primary",panel_title = "Download complete datasets", content = strong("1. Scoring kinases")),
                                             downloadButton('scoring_table', 'Download Data'),
                                             br(),
                                             br(),
                                             
                                             panel_div(class_type = "primary",panel_title = "Download complete dataset", content = strong("2. Differentially expressed kinases")), 
                                             downloadButton('DE_table', 'Download Data'),
                                             br(),
                                             br(), 
                                             
                                             panel_div(class_type = "primary",panel_title = "Download complete dataset", content = strong("3. Stage data TNM for cancer type ACC, BLCA,  BRCA,  CESC,  CHOL, COAD, ESCA, HNSC, KICH, KIRC, KIRP,  LUAD, LUSC, PAAD, PRAD, READ, SKCM, STAD, THCA")), 
                                             selectInput("Cancer", "Choose the Cancer", sort(unique(as.character(rank_data[,2]))), selected = "BLCA"),
                                             downloadButton('Stage_table', 'Download Data'),
                                             br(),
                                             br()
                                    )         
                                    
                                    
                           ) 
                           
                )
)

# Get the Survival data

#understudiedKinases <- data.frame(read.csv("/projects/ccs/schurerlab/Rimpi/TCGARecount2/ShinyApp/data/Survival_Understudied.csv"))
#get the understudied Kinases and Cancer data for the plot
understudiedKinases <-data.frame(readRDS("data/Upregulated_understudiedKinases_cancer.rds"))
#rownames(understudiedKinases) <- understudiedKinases$X
#understudiedKinases <- understudiedKinases[,c(2:21)]
#get the all Kinases Cancer data for the plot
Kinases <- data.frame(readRDS("data/Upregulated_Kinases_cancer.rds"))
# rownames(Kinases) <- Kinases$X
# Kinases <- Kinases[,c(2:21)]
#survival Data
Survival_Kinases <- data.frame(readRDS("data/Survival_Kinase.rds"))
colnames(Survival_Kinases) <- c("times", "patient.vital_status", "expr", "cohort",  "gene" ,  "IDG_Status")
#rank and score data
rank_data <- data.frame(readRDS("data/Kinase_full_information_for_App.rds"))
colnames(rank_data) <- c("Gene", "Cancer",   "TDL",  "IDG_Status",  "Kinase_Score", "Rank",     "Kinase_Type",  "Kinase_Group",  "Kinase_Family", "Kinase_Protein",  "MOA_Targets")
rank_data$Kinase_Score <- round(rank_data$Kinase_Score, 2)
neworder <- c("Tdark", "Tbio" , "Tchem", "Tclin")
library(plyr)  ## or dplyr (transform -> mutate)
rank_data <- arrange(transform(rank_data,
                           TDL=factor(TDL,levels=neworder)),TDL)
#rank_data$Kinase_score <- as.numeric(rank_data$Kinase_score)
#rank_data <- rank_data[,c(1,10,2,3,11,12, 5:8)]
Kin <- unique(rank_data$Gene)
#Differentially expressed genes data
DEAnlysis <- data.frame(read.csv("data/All_cancers_genes_Pvalue_FC.csv"))

#Target MOA data 
TargetMOA <- data.frame(read.csv("data/TargetMOA.csv"))
####                                                                                                                                                                                                                                                                                                                                       

server <- shinyServer(function(input, output, session) {
  
  # =========== BUILDING THE INPUTS ===========
  ###########################
  # tab 1 results
  #######################
  
  
  df1 <- reactive({ switch(input$Kinase,
                           "Understudied kinases*" = understudiedKinases,
                           "All kinases" = Kinases)
  })
  
  
  p <- reactive({
    
  })
  
  output$heat <- renderPlotly({
    
    df1 <- df1()
    df1_1 <- t(df1)
    df1_1 <- df1_1[order(rowSums(df1_1),decreasing=T),]
    df1_2 <- df1_1
    colnames(df1_2) <- NULL
    dd1 <- list(
      x = as.character(colnames(df1_1)),
      y = as.character(rownames(df1_1)),
      z = lapply(as.list(1:dim(df1_2)), function(x) df1_2[x[1],]),
      
      colorbar = list(
        x = 0.99,
        y = 0.35,
        len = 0.75,
        thickness = 15
      ),
      colorscale = "Grays",
      reversescale = FALSE,
      type = "heatmap"
    )
    
    dd2 <- list(
      x = as.character(colnames(df1_1)),
      y = colSums(df1_1),
      name = "Kinase",
      type = "bar",
      xaxis = "x1",
      yaxis = "y2"
      
    )
    
    dd3 <- list(
      x = rowSums(df1_1),
      y = as.character(rownames(df1_1)),
      name = "Cancer",
      orientation = "h",
      type = "bar",
      xaxis = "x2",
      yaxis = "y1"
    )
    
    data <- list(dd1, dd2, dd3)
    layout <- list(
      font = list(
        family = "Dark_kinome",
        size = 9
      ),
      #title = "under-studied Kinases",
      xaxis = list(
        domain = c(0, 0.75),
        title = "Kinases",
        zeroline = TRUE, categoryarray = names, categoryorder = "array"
      ),
      xaxis2 = list(domain = c(0.8, 1), 
                    title = "No. of genes in a cancer", 
                    zeroline = TRUE, categoryarray = names, categoryorder = "array"),
      yaxis = list(
        domain = c(0, 0.75),
        title = "Cancer type",
        zeroline = TRUE, categoryarray = names, categoryorder = "array"
      ),
      yaxis2 = list(domain = c(0.8, 1),
                    title = "Gene in different cancers", 
                    zeroline = TRUE, categoryarray = names, categoryorder = "array")
    )
    
    p <- plot_ly( source = "subset" )
    p <- add_trace(p, x= dd1$x, y=dd1$y, z=dd1$z,  colorscale=dd1$colorscale, type=dd1$type, reversescale=dd1$reversescale, showscale = FALSE)  #colorbar=dd1$colorbar,colorbar=dd1$colorbar,
    p<- add_trace(p, x=dd2$x, y=dd2$y, type=dd2$type, xaxis=dd2$xaxis, yaxis=dd2$yaxis, marker = list(color = 'red'), showlegend = FALSE)
    p<- add_trace(p, x=dd3$x, y=dd3$y,  orientation=dd3$orientation, type=dd3$type, xaxis=dd3$xaxis, yaxis=dd3$yaxis,  marker = list(color = 'red'), showlegend = FALSE)
    
    p <- layout(p, font=layout$font, title=layout$title,  xaxis=layout$xaxis, xaxis2=layout$xaxis2,  yaxis2=layout$yaxis2, yaxis=layout$yaxis) 
    print(p)
    
  })
  
  
  output$selected1 <- renderPrint(
    event_data("plotly_click", source = "subset")
  )
  
  #output those points into a table
  
  # output$view <- renderTable({
  #   s <- event_data("plotly_click", source = "subset")
  #   if(is.null(s)) return(NULL)
  #   else
  #     df1()
  # })
  # 
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("output", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df1() , file, row.names = TRUE,sep="\t")
    }
  )
  
  ###########################
  # tab 2 results
  #######################
  
  
  #take the rank_data file for 
  
  datasetInput2 <- reactive({ #tab 1 option 1 select the kinases
    
    data <- rank_data
    #dat <- rank_data[rank_data$IDG_Status == input$dataset1 ]
  })
  
  
  
  observeEvent(input$Gene, {
    if (input$Gene == "All") enable("SelectTDL") & enable("kinaseType") & enable("kinaseGroup") & enable("MOA")
      else disable("SelectTDL") & disable("kinaseType") & disable("kinaseGroup") & disable("MOA")
  })
  
  observeEvent(input$reset_input, {
      reset("GenePage")
  
  })
  
  mytable1 <- reactive({
  #output$mytable2 <-renderDT({
  #output$mytable2 <- DT::renderDataTable({ 

    
    datatable(data <- as.data.frame(rank_data, selection='single'))
    
    filtered <- data
    if(input$Gene == "All"){
      filtered <- data
      
      if (!is.null(input$SelectTDL)) {
        filtered <- filter(data, TDL %in% input$SelectTDL)
      }
      
      if (!is.null(input$kinaseType)) {
        
        filtered <- filter(data, Kinase_Type  %in%  input$kinaseType)
      }
      
      if(!is.null(input$SelectTDL) & !is.null(input$kinaseType)) {
        
        filtered <- filter(data, TDL %in% input$SelectTDL & Kinase_Type  %in%  input$kinaseType)
      }
      
      if (!is.null(input$kinaseGroup)) {
        filtered <- filter(data, Kinase_Group  %in%  input$kinaseGroup)
      }
      
      if(!is.null(input$SelectTDL) & !is.null(input$kinaseGroup)) {
        
        filtered <- filter(data, TDL %in% input$SelectTDL & Kinase_Group  %in%  input$kinaseGroup)
      }
      
      
      if(!is.null(input$SelectTDL) & !is.null(input$kinaseType) & !is.null(input$kinaseGroup)) {
        
        filtered <- filter(data, TDL %in% input$SelectTDL & Kinase_Type  %in%  input$kinaseType & Kinase_Group  %in%  input$kinaseGroup)
      }
      
      if (!is.null(input$MOA)) {
        
        filtered <- filter(data, MOA_Targets  %in%  input$MOA)
      }
      
      if (!is.null(input$MOA) & !is.null(input$SelectTDL)) {
        
        filtered <- filter(data, TDL %in% input$SelectTDL & MOA_Targets  %in%  input$MOA)
      }
      
      
      if (!is.null(input$MOA) & !is.null(input$kinaseGroup)){
        
        filtered <- filter(data, MOA_Targets  %in%  input$MOA & Kinase_Group  %in%  input$kinaseGroup)
      }
      
      if (!is.null(input$MOA) & !is.null(input$kinaseType)){
        
        filtered <- filter(data, MOA_Targets  %in%  input$MOA & Kinase_Type  %in%  input$kinaseType)
      }
      
      
      if(!is.null(input$SelectTDL) & !is.null(input$kinaseGroup) & !is.null(input$MOA)) {
        
        filtered <- filter(data, TDL %in% input$SelectTDL & Kinase_Group  %in%  input$kinaseGroup & MOA_Targets  %in%  input$MOA)
      }
      
      if(!is.null(input$SelectTDL) & !is.null(input$kinaseType) & !is.null(input$kinaseGroup) & !is.null(input$MOA)) {
        
        filtered <- filter(data, TDL %in% input$SelectTDL & Kinase_Type  %in%  input$kinaseType & Kinase_Group  %in%  input$kinaseGroup & MOA_Targets  %in%  input$MOA)
      }
      
      filtered 
     
      }else {
      
      filtered <- filter(data, Gene %in% input$Gene)

    }
      
    
  })
    
  
  output$mytable2 <-renderDT({
    mytable1()
  })
  
  output$filt_table <- downloadHandler(
    filename = function() {
      paste("filt_output", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mytable1() , file, row.names = TRUE,sep="\t")
    }
  )

  
  
  
  user_iris <- reactive({
    rank_data[rank_data$Gene %in% input$Gene,]
  })
  
  
  user_iris1 <- reactive({
    rank_data[rank_data$TDL %in% input$SelectTDL,]
  })
  
  user_iris2 <- reactive({
    dd <- user_iris1()
    dd[dd$Kinase_Type %in% input$kinaseType,]
  })
  
  user_iris3 <- reactive({
    dd <- user_iris2()
    dd[dd$Kinase_Group %in% input$kinaseGroup,]
  })
  
  user_iris4 <- reactive({
    dd <- user_iris3()
    dd[dd$MOA_Targets %in% input$MOA,]
  })
  
  user_iris5 <- reactive({
    rank_data[rank_data$Kinase_Type %in% input$kinaseType,]
  })
  
  user_iris6 <- reactive({
    rank_data[rank_data$Kinase_Group %in% input$kinaseGroup,]
  })
  
  user_iris7 <- reactive({
    rank_data[rank_data$MOA_Targets %in% input$MOA,]
  })
  
  
  user_iris8 <- reactive({
    rank_data[rank_data$Kinase_Type %in% input$kinaseType & rank_data$Kinase_Group %in% input$kinaseGroup,]
  })
  
  user_iris9 <- reactive({
    rank_data[rank_data$Kinase_Type %in% input$kinaseType & rank_data$MOA_Targets %in% input$MOA,]
  })
  
  user_iris10 <- reactive({
    rank_data[rank_data$TDL %in% input$SelectTDL & rank_data$Kinase_Group %in% input$kinaseGroup,]
  })
  
  
  sd <- SharedData$new(user_iris)
  sd1 <- SharedData$new(user_iris1)
  sd2 <- SharedData$new(user_iris2)
  sd3 <- SharedData$new(user_iris3)
  sd4 <- SharedData$new(user_iris4)
  sd5 <- SharedData$new(user_iris5)
  sd6 <- SharedData$new(user_iris6)
  sd7 <- SharedData$new(user_iris7)
  sd8 <- SharedData$new(user_iris8)
  sd9 <- SharedData$new(user_iris9)
  sd10 <- SharedData$new(user_iris10)
 
   output$scatter <- renderPlotly({
    
    if(input$Gene == "All"){
    p <- ggplot(rank_data, aes(x=Kinase_Score, y=Cancer,  color  = TDL,  text = paste("gene:",Gene))) +
    facet_grid(. ~ TDL) +
    geom_point(alpha = 1 ) +
    scale_colour_manual(values = c( Tdark = "black" , Tbio = "red", Tchem = "green", Tclin = "blue")) +
    labs(x='') + theme_minimal()
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                      size=3))
    
    if (!is.null(input$SelectTDL)) {
      p <- ggplot(sd1, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tdark = "black", Tbio = "red", Tchem = "green", Tclin = "blue" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
    if (!is.null(input$kinaseType)) {
      p <- ggplot(sd5, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
    if (!is.null(input$kinaseGroup)) {
      p <- ggplot(sd6, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
    if (!is.null(input$MOA)) {
      p <- ggplot(sd7, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
    if (!is.null(input$kinaseType) & !is.null(input$kinaseGroup)) {
      p <- ggplot(sd8, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
    if (!is.null(input$kinaseType) & !is.null(input$MOA)) {
      p <- ggplot(sd9, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
    if (!is.null(input$kinaseType) & !is.null(input$SelectTDL)) {
      p <- ggplot(sd2, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
    if (!is.null(input$kinaseType) & !is.null(input$SelectTDL) &  !is.null(input$kinaseGroup))  {
      p <- ggplot(sd3, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
    if (!is.null(input$kinaseType) & !is.null(input$SelectTDL) &  !is.null(input$kinaseGroup) & !is.null(input$MOA))  {
      p <- ggplot(sd4, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
    if (!is.null(input$SelectTDL) &  !is.null(input$kinaseGroup))  {
      p <- ggplot(sd10, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_grid(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      
    }
    
    ggplotly(p, tooltip=c("x", "y"))
    
     }else {
      p <- ggplot(sd, aes(x=Kinase_Score, y=Cancer, color = TDL, text = paste("gene:",Gene))) +
        geom_point(alpha = 1) +
        facet_wrap(. ~ TDL, scales = "free_x") +
        scale_colour_manual(values = c( Tbio = "red", Tchem = "green", Tclin = "blue", Tdark = "black" )) +
        labs(x='') + theme_minimal()
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       size=3))
      ggplotly(hide_legend(p), tooltip=c("x", "y")) 
  }
    
    
    
    
    #   
 #black, red, green, blue for dark, bio, chem, clin 
  })
  #################
  #tab 3 results
  ################
  
  observeEvent(input$reset_input1, {
    reset("PlotsPage")
    
  })
  
  
  dataFrame <- reactive({
    filename <- DEAnlysis
    filename$color <- as.factor(abs(filename$logFC) > 2 & filename$FDR < 0.05)
    filename$negLogFDR <- -log10(filename$FDR)
    Kin <- unique(rank_data$Gene)
    Kin_FC <- filename[filename$Genes %in% Kin,]
    Kin_FC$color <- ifelse(Kin_FC$color == "TRUE", "Kinases (Significant differentially expressed)", "Kinases (Not Significant differentially expressed)")
    filename <- filename[!filename$Genes %in% Kin,]
    filename1 <- rbind(filename,  Kin_FC)
    levels(filename1$color)[levels(filename1$color) == "FALSE"] <- "Non-kinases (Not Significant differentially expressed)"
    levels(filename1$color)[levels(filename1$color) == "TRUE"] <- "Non-kinases (Significant differentially expressed)"
    select <- filename1 %>% filter(Cancer == input$Cancer)
  })
  
  dataFilter <- reactive({
    dataFrame()[dataFrame()$logCPM > input$cpmCut,]
  })
  
  output$sd <-  renderUI({
    sliderInput('cpmCut', label="log(CPM) cutoff",-2,10,0, width="200px")})
  
  
  output$volcanoPlot <- renderPlot({ 
    ggplot(dataFilter(),aes(x=logFC, y=negLogFDR,  color=color)) +
      geom_point() +
      coord_cartesian() +
      ylab("log2 FC") +
      xlab("log2 CPM") + scale_colour_manual(values=c("grey", "grey50", "grey", "red")) +
      theme_bw()+
    theme(legend.title = element_text(color = "black", size = 20), 
          legend.text = element_text(color = "black", size = 12), legend.spacing.x = unit(1.0, 'cm'), legend.key.size = unit(1.5, "cm")) +
      guides(colour = guide_legend(override.aes = list(size=5)))
      
  })
  
  
  #get the clicked points!
  clicked <- reactive({
    # We need to tell it what the x and y variables are:
    nearPoints(dataFilter(), input$plot_click, xvar = "logFC", yvar = "negLogFDR")
  })
  
  #output those points into a table
  
  output$clickedPoints <- renderTable({
    clicked()
  }, rownames = T)
  
  
  ################# 
  #tab3 and subplot for tab 2  study plots
  
  ####################
  
  dataFrame_study <- reactive({
    req(input$Cancer)
    file <- data.frame(read.csv(paste0("data/stage_expression/", input$Cancer, "_stage.csv" ,sep = "")))
  })
  
  gen <- reactive({ sort(unique(as.character(colnames(dataFrame_study()[, -(1:6)]))))})
  
  
  output$gg <- renderUI({
    selectInput("Gene1", "Choose the gene", gen(), selected = NULL)})
  
  
  output$T_stage <- renderPlotly({
    if(!is.null(input$Gene1)){
      t <- dataFrame_study() %>% select(pathology_T_stage, input$Gene1)
      t1 <- t %>% 
        dplyr::filter(pathology_T_stage != "tx" &  !is.na(pathology_T_stage)) %>% 
        droplevels()
      plot_ly(t1, y = ~log2(t1[,2]), color = ~pathology_T_stage, type = "box") %>% layout(yaxis = list(title = "log2"))
    } else {
      "gene does not have pathology_T_stage"
    }
  })
  
  
  
  
  output$M_stage <- renderPlotly({
    if(!is.null(input$Gene1)){
      m <- dataFrame_study() %>% select(pathology_M_stage, input$Gene1)
      m1 <- m %>% dplyr::filter(pathology_M_stage != "mx" &  !is.na(pathology_M_stage)) %>% droplevels()
      
      plot_ly(m1, y = ~log2(m1[,2]), color = ~m1[,1], type = "box") %>% layout(yaxis = list(title = "log2"))
      #boxplot( m1[,2] ~ m1[,1] , data=m1)
      
    } else {
      "gene does not have pathology_M_stage"
    }
  })
  
  
  
  output$N_stage <- renderPlotly({
    if(!is.null(input$Gene1)){
      n <- dataFrame_study() %>% select(pathology_N_stage, input$Gene1)
      n1 <- n %>% dplyr::filter(pathology_N_stage != "nx" &  !is.na(pathology_N_stage)) %>% droplevels()
      plot_ly(n1, y = ~log2(n1[,2]), color = ~n1[,1], type = "box") %>% layout(yaxis = list(title = "log2"))
      
      #boxplot( n1[,2] ~ n1[,1] , data=n1)
      
    } else {
      "gene does not have pathology_N_stage"
    }
  })
  
  
  
  
  ################# 
  #tab3 and subplot for tab 3  study plots
  
  ####################
  
  ###survival plot 
  
  datasetInput3 <- reactive({ #tab 1 option 1 select the kinases
    
    survival_data <- Survival_Kinases
    #dat <- rank_data[rank_data$IDG_Status == input$dataset1 ]
  })
  
  #genes for selection 
  
  Genes_sur <- reactive({ sort(unique(as.character(datasetInput3()[,5])))})
  
  
  output$gg_s <- renderUI({
    selectInput("Gene2", "Choose the gene",Genes_sur(), selected = NULL)})
  
  
  output$Survival1 <- renderHighchart({
    
    sur <- datasetInput3()
    dat <- sur %>% filter(gene == input$Gene2 & cohort == input$Cancer) 
    
    surv_obj <- Surv(dat$times,dat$patient.vital_status)
    fit = do.call(survfit, list(surv_obj ~ expr + cohort, data = dat))
    pval <- surv_pvalue(fit)
    
    hchart(fit, rangesOpacity = 0.2, ranges = TRUE)
    # ggsurvplot(
    #   fit, 
    #   #data= data,
    #   break.time.by = 500, 
    #   ggtheme = theme_bw(), 
    #   #risk.table = TRUE,
    #   xlim = c(0,3000),
    #   xlab = "Time in days",
    #   pval = TRUE,
    #   font.x = 16, 
    #   font.y=16,
    #   #risk.table.height = 0.25,
    #   fontsize=3, 
    #   #risk.table.col= "strata",
    #   size=1, palette = c("#2E9FDF", "red"),
    #   main = input$gene
    #   #risk.table.fontsize = 5, 
    #   #risk.table.y.text = FALSE
    # ) 
    # 
  })
  
  
  # output$selected_var <- renderText({ 
  #   print(paste0("the Pvalue is", pval()))
  
 # })
  
  #get the clicked points!
  clicked1 <- reactive({
    # We need to tell it what the x and y variables are:
    nearPoints(datasetInput3(), input$plot_click1, xvar = "times", yvar = "expr")
  })
  
  
  #output those points into a table
  
  output$clickedPoints1 <- renderTable({
    clicked1()
  }, rownames = T)
  
  
  ################
  # tab 4 download data 
  ###############
  output$scoring_table <- downloadHandler(
    filename = function() {
      paste("output_scoring", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rank_data , file, row.names = TRUE,sep="\t")
    }
  )
  
  
  output$DE_table <- downloadHandler(
    filename = function() {
      paste("output_DE", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(DEAnlysis , file, row.names = TRUE,sep="\t")
    }
  )
  
  output$Stage_table <- downloadHandler(
    filename = function() {
      paste("output_Stage", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataFrame_study, file, row.names = TRUE,sep="\t")
    }
  )
  
  
  # output$mytable3 <- renderTable({
  #   t <- dataFrame_study() %>% select(input$Gene1)
  # })
  
  
  
})

shinyApp(ui, server)
