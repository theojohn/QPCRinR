library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(shinyjs)
library(shinyjqui)
library(sortable)
library(spsComps)
library(shinycssloaders)
library(shinycustomloader)
library(shinyBS)
library(openxlsx)
library(lme4)
library(lmerTest)
library(pbkrtest)
library(emmeans)
library(shinyWidgets)
library(magrittr)
library(limma)
library(ggplot2)
library(plotly)
library(RColorBrewer)

ui <- fluidPage(theme = shinytheme("spacelab"),
                useShinyjs(),
                tags$head(
                  tags$style(HTML(".help-block a {color: #b82318 !important;}"))
                ),
                tabsetPanel(id = "qPCR",
                            tabPanel("Welcome",
                                     fixedPanel(
                                       actionButton("launch", label = "Launch!!", icon("rocket"), style = "background-color: #afcbe0"),
                                       right = 10,
                                       bottom = 10
                                     ),
                                     fixedPanel(
                                       actionButton("Steppdf", "QPCRinR User's Guide", onclick = "window.open('QPCRinRV1_Supplementary.pdf')"),
                                       left = 10,
                                       bottom = 10
                                     ),
                                     fluidPage(
                                       fluidRow(
                                         column(4, offset = 4,
                                                br(),
                                                br(),
                                                tags$img(src='QPCRinR.png', heigth=300, width=500)
                                         )
                                       ),
                                       fluidRow(
                                         column(1, offset = 7,
                                                h5("v.1.0")
                                         ),
                                       ),
                                       fluidRow(
                                         column(4, offset = 4,
                                                br(),
                                                br(),
                                                br(),
                                                tags$img(src='Powerful.png', heigth=300, width=450),
                                         )
                                       ),
                                       fluidRow(
                                         column(4, offset = 4,
                                                br(),
                                                br(),
                                                br(),
                                                br(),
                                                br()
                                         )
                                       ),
                                       fluidRow(
                                         column(4, offset = 5,
                                                tags$img(src='benaki.jpg', heigth=300, width=250)
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Load Data",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    h4("Select Data Input parameters below"),
                                                    checkboxInput("header", "Header", TRUE),
                                                    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", TRUE),
                                                    radioButtons("sep", "Separator",
                                                                 choices = c(
                                                                   Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t"
                                                                 ),
                                                                 selected = ","
                                                    ),
                                                    checkboxInput("readXLSX", "Open Excel File (.xlsx)", FALSE),
                                                    column(12,
                                                           splitLayout(cellWidths = c("70%", "5%", "60%"),
                                                                       fileInput("main", "Input Data File",
                                                                                 multiple = FALSE,
                                                                                 accept = c(
                                                                                   "text/csv",
                                                                                   "text/comma-separated-values",
                                                                                   "text/plain",
                                                                                   ".csv")
                                                                       ),
                                                                       h3(),
                                                                       actionButton(inputId = "demo",
                                                                                    label = "Demo Set",
                                                                                    class = "btn-primary",
                                                                                    style='padding:8px 2px; font-size:95%; margin-top:25px'),
                                                           )
                                                    ),
                                                    helpText("Default max. file size is 100MB"),
                                                    br(),
                                                    uiOutput("advancedclassChange"),
                                       ), 
                                       
                                       mainPanel(
                                         DT::dataTableOutput("data"),
                                         tags$hr(),
                                         verbatimTextOutput("structure"),
                                         fixedPanel(
                                           actionButton("toWelcome", label = "", icon("angle-left"), style = "background-color: #e0afaf"),
                                           center = 10,
                                           bottom = 10
                                         ),
                                         fixedPanel(
                                           actionButton("toModelSpec", label = "", icon("angle-right"), style = "background-color: #afe0bb"),
                                           right = 10,
                                           bottom = 10
                                         )
                                       ) 
                                     ) 
                            ),
                            
                            tabPanel("Model Specification",
                                     sidebarLayout(
                                       sidebarPanel(width = 2,
                                                    uiOutput("callibrators"),
                                                    uiOutput("specCalib")
                                       ), # sidebarPAnel
                                       mainPanel(
                                         uiOutput("SpecifyModel"),
                                         br(),
                                         withSpinner(tableOutput("modelTable"), type = 1),
                                         withSpinner(verbatimTextOutput("ModelSummary"), type = 1),
                                         withSpinner(verbatimTextOutput("emmeans"), type = 1),
                                         fixedPanel(
                                           actionButton("toLoadData", label = "", icon("angle-left"), style = "background-color: #e0afaf"),
                                           center = 10,
                                           bottom = 10
                                         ),
                                         fixedPanel(
                                           actionButton("toContrastAssignment", label = "", icon("angle-right"), style = "background-color: #afe0bb"),
                                           right = 10,
                                           bottom = 10
                                         )
                                       )
                                     )
                            ),
                            
                            tabPanel("Contrast Assignment",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    uiOutput("ChooseRefGene"),
                                                    hr(),
                                                    uiOutput("CntrYesNo"),
                                                    uiOutput("conditionalInput"),
                                                    uiOutput("ContrFact"),
                                                    uiOutput("ContrConfirm")
                                       ),
                                       mainPanel(
                                         withSpinner(verbatimTextOutput("Contrasts"), type = 1),
                                         fixedPanel(
                                           actionButton("toModelSpecB", label = "", icon("angle-left"), style = "background-color: #e0afaf"),
                                           center = 10,
                                           bottom = 10
                                         ),
                                         fixedPanel(
                                           actionButton("toResult", label = "", icon("angle-right"), style = "background-color: #afe0bb"),
                                           right = 10,
                                           bottom = 10
                                         )
                                       )
                                     )
                            ),
                            
                            tabPanel("Results",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    uiOutput("forPlot")
                                       ),
                                       mainPanel(
                                         DT::dataTableOutput("result"),
                                         uiOutput("DownloadTable"),
                                         tags$hr(),
                                         br(),
                                         
                                         plotlyOutput("plot"),
                                         fixedPanel(
                                           actionButton("toContrastAssignmentB", label = "", icon("angle-left"), style = "background-color: #e0afaf"),
                                           center = 10,
                                           bottom = 10
                                         )
                                       )
                                     )
                            )
                )
)

