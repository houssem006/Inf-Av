#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(readxl)
library(knitr)
library(dplyr)
library(tidyr)
library(sunburstR)
library(fastDummies)
library(caret)
library(verification)
library(plotly)
library(dplyr)
library(questionr)
library(ROCR)
library(plotROC)
library(rpart)
library(rpart.plot)
library(stringr)
library(shinydashboard)
#library(devtools)
library(shiny)
library(gmodels)
library(readr)



library(dashboardthemes)
library(plotly)
library(plyr)
library(sunburstR)
library(DataExplorer)
library(corrr)
library(tidyverse)
library(networkD3)
library(corrplot)
library(ggforce) # for 'geom_arc_bar'
library(ggplot2)
library(yarrr)
library(ggpirate)
library(DescTools)
library(dashboardthemes)
library(ResourceSelection)



library(shiny)

shinyUI(dashboardPage(
  
  dashboardHeader(title="Arvea ",
                  dropdownMenuOutput("messageMenu"),
                  tags$li(class = "dropdown",
                         tags$img(src="logo.jpg",width="100px",height = "50px", alt="SNAP Logo" ))),
  
  
  dashboardSidebar(sidebarMenu(
    id= "tabs" ,
    
    
    menuItem("Import dataset",tabName = "Import",icon=icon("database")
    ),
    
    
    
    menuItem("Tableaux des statistiques",tabName = "an" ,icon=icon("area-chart "),
             menuSubItem("Parcours 2",tabName = "analyse1",icon=icon("bar-chart")),
             
             menuSubItem("Parcours 1",tabName = "analyse2",icon=icon("circle")),
             menuSubItem("Totalite",tabName = "Totalite",icon=icon("circle")),
             menuSubItem("Evolution Niveau",tabName = "Niveau",icon=icon("circle")),
             menuSubItem("Evolution Effort",tabName = "Effort",icon=icon("circle"))
             
             
    ),
    menuItem("Graphs",tabName = "an" ,icon=icon("area-chart "),
             menuSubItem("Niveau",tabName = "analyse4",icon=icon("bar-chart")),
             
             menuSubItem("Effort",tabName = "analyse3",icon=icon("circle"))
             
    ),
    menuItem("logistic",tabName = "logistic1",icon=icon("area-chart")
    )
    ,
    menuItem("Export",tabName = "logist",icon=icon("area-chart")
    ),
    
    
    sliderInput("tr","train",min = 30,max = 100,value = 75),
    checkboxInput("dummy","transformer les variables en dummy (veuillez patienter)")
    
    
    
    
  )),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                              .skin-blue .main-header .logo {
                              font-family: "Calibri";
                              font-weight: bold;
                              font-size: 28px;
                              background-color: #003D76;
                              }
                              .skin-blue .main-header .navbar {
                              background-color: #0082D1;
                              }'))),
    tags$style(type="text/css",
               
               "
               
               .box {
               
               background: transparent;
               
               
               }
               .nav-tabs-custom>.nav-tabs>li.header {
               font-family: Georgia, serif;
               }
               .box-header .box-title {
               font-family: Georgia, serif;
               }
               .main-sidebar { font-family: Georgia, serif;
               
               }.content-wrapper, .right-side {
               
               background-image: url(oo.jpg);
               
               
               }
               "),
    
    shinyDashboardThemes(theme = "blue_gradient"),
    style = "max-height: 90vh; overflow-y: auto;",
    tags$style(type="text/css",
               ".shiny-output-error {visibility: hidden;}",
               ".shiny-output-error:before {visibility: hidden;}"),
    
    tabItems(
      
      
      
      tabItem(tabName = "Import",fillPage(),
              tabsetPanel(tabPanel("Donnees brut",tabName="sens",
                                   
                                   fluidRow(
                                     box(style="color:white",
                                         width = 3, status = "primary",solidHeader = TRUE,
                                         title = "Download manager",
                                         helpText(tags$b("Please uplaod .xlsx")),
                                         tags$hr(),
                                         fileInput('csv_file1', 'Choose file to upload',
                                                   accept = c(
                                                     'text/csv',
                                                     'text/comma-separated-values',
                                                     'text/tab-separated-values',
                                                     'text/plain',
                                                     '.xlsx',
                                                     '.csv'
                                                   )
                                         )
                                         
                                         
                                         
                                         
                                         
                                     ),
                                     box(width = 9,status = "primary",solidHeader = TRUE,
                                         title = "Data",
                                         DT::dataTableOutput("filetable")
                                     ))
              ),
              tabPanel("Summary",tabName="sensum",
                       box(uiOutput("variable3")),
                       
                       box(
                         status = "primary",solidHeader = TRUE,
                         width = 9,title = "Data summary",
                         verbatimTextOutput("summary1")))
              ,
              tabPanel("structure",tabName="sensum",
                       box(
                         status = "primary",solidHeader = TRUE,
                         width = 13,title = "Data structure",
                         diagonalNetworkOutput("structure111")))
              
              )),
      
      
      
      tabItem("analyse1" ,
              tabsetPanel(
              tabPanel(
                        
                       
                         status = "primary",solidHeader = TRUE,
                         width = 9,title = "Tableau",
                         DT::dataTableOutput("pcrr2")
                         ),
              tabPanel(
                        
                          status = "primary",solidHeader = TRUE,
                          width = 12,title = "Densite",
                          plotOutput("plothist1")),
              tabPanel(
                       
                         status = "primary",solidHeader = TRUE,
                         width = 12,title = "Histogramme",
                         plotOutput("plotpie"))
                        )
              
      
    ),
    tabItem("analyse2" ,
            tabsetPanel(
              tabPanel(
                status = "primary",solidHeader = TRUE,
                width = 9,title = "Tableau",
                DT::dataTableOutput("pcrr1")
              ),
              tabPanel(
                
                status = "primary",solidHeader = TRUE,
                width = 9,title = "Densite",
                plotOutput("boxp1")),
              tabPanel(
                
                status = "primary",solidHeader = TRUE,
                width = 9,title = "Histogramme",
                plotOutput("histdensity"))
            
              )
    ),
    tabItem("Totalite" ,
            tabsetPanel(
              tabPanel( 
                status = "primary",solidHeader = TRUE,
                width = 9,title = "Tableau",
                DT::dataTableOutput("statii1")),
              tabPanel(                
                status = "primary",solidHeader = TRUE,
                width = 9,title = "Histogramme niveau",
                #verbatimTextOutput("crsstab"))
                plotOutput("crsstab")),
              tabPanel(
                
                status = "primary",solidHeader = TRUE,
                width = 9,title = "repartition niveau",
                plotOutput("repartition")),
              tabPanel(
                
                status = "primary",solidHeader = TRUE,
                width = 9,title = "repartition temps entrainement",
                plotOutput("ggbarx"),
                plotOutput("ggbarx1")
                ),
              tabPanel(
                
                status = "primary",solidHeader = TRUE,
                width = 9,title = "relation Niveau atteint /Temps d'entrainement ",
                plotOutput("ggbarx2"),
                plotOutput("ggbarx3"))
            )
            
            
    ),
    
    tabItem("Niveau" ,
            fluidRow(
              box(style="color:white",
                  width = 3, status = "primary",solidHeader = TRUE,
                  title = "Download manager",
                  helpText(tags$b("Please uplaod .xlsx")),
                  tags$hr(),
                  fileInput('csv_file2', 'Choose file to upload',
                            accept = c(
                              'text/csv',
                              'text/comma-separated-values',
                              'text/tab-separated-values',
                              'text/plain',
                              '.xlsx',
                              '.csv'
                            )
                  )
                  
                  
                  
                  
                  
              ),
              box(width = 9,status = "primary",solidHeader = TRUE,
                  title = "Data",
                  DT::dataTableOutput("nivf")
              ))
            
            
    ),
    
    tabItem("Effort" ,
            fluidRow(
              box(style="color:white",
                  width = 3, status = "primary",solidHeader = TRUE,
                  title = "Download manager",
                  helpText(tags$b("Please uplaod .xlsx")),
                  tags$hr(),
                  fileInput('csv_file3', 'Choose file to upload',
                            accept = c(
                              'text/csv',
                              'text/comma-separated-values',
                              'text/tab-separated-values',
                              'text/plain',
                              '.xlsx',
                              '.csv'
                            )
                  )
                  
                  
                  
                  
                  
              ),
              box(width = 9,status = "primary",solidHeader = TRUE,
                  title = "Data",
                  DT::dataTableOutput("effortf")
              ))
            
            
    ),
    
    tabItem("analyse4",
            box(height = 850,width=12, plotlyOutput("graphniv"))
    
    
   ),
   tabItem("analyse3",
           box(height = 850,width=12, plotlyOutput("grapheff"))
           
           
    ),
   
   tabItem("logistic1",tabsetPanel(
     tabPanel( 
       
       status = "primary",solidHeader = TRUE,
       width = 9,title = "datalogistic",
       box(style="color:white",
                        width = 3, status = "primary",solidHeader = TRUE,
                        title = "Download manager",
                        helpText(tags$b("Please uplaod .xlsx")),
                        tags$hr(),
                        fileInput('csv_fileD', 'Choose file to upload',
                                  accept = c(
                                    'text/csv',
                                    'text/comma-separated-values',
                                    'text/tab-separated-values',
                                    'text/plain',
                                    '.xlsx',
                                    '.csv'
                                  )
                        )





                    ),
       box(width = 9,status = "primary",solidHeader = TRUE,
           title = "Data",
           DT::dataTableOutput("filedataDd")
       )),
     tabPanel( 
       
       status = "primary",solidHeader = TRUE,
       width = 9,title = "nuII",
       verbatimTextOutput("logisticn")),
     tabPanel( 
       
       status = "primary",solidHeader = TRUE,
       width = 9,title = "fuII",
       verbatimTextOutput("logisticf"),
       h5("pseudo R2"),
       verbatimTextOutput("R2Fu"),
       h5("Cstat : AUC"),
       verbatimTextOutput("cstatf"))
     ,
     tabPanel( 
       
       status = "primary",solidHeader = TRUE,
       width = 9,title = "best",
       verbatimTextOutput("logisticb"),
       h5("pseudo R2"),
       verbatimTextOutput("R2Be"),
       h5("Cstat : AUC"),
       verbatimTextOutput("cstatb")),
     tabPanel( 
       
       status = "primary",solidHeader = TRUE,
       width = 9,title = "tests",
       h5("Hosmer_Lemeshow"),
       verbatimTextOutput("Hosmer_Lemeshow"),
       plotOutput("pHosmer_Lemeshow"),
       h5("anova"),
       verbatimTextOutput("avv")),
     tabPanel("Summary",
              textInput('model', 'Enter your model' ),
              helpText(tags$b("Please choose your method")),
              uiOutput("var"),
              selected = "Summary",
              
              verbatimTextOutput("logisticsummary")),
     tabPanel("ROC",plotOutput("roc"))
     #,tabPanel("Lift",plotOutput("lift"),plotOutput("logisticlift") ) 
     
   
   )
   
   
   )

    
    
    
    
   
    )
  
    ))                     )