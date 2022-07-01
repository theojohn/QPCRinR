#http://shiny.rstudio.com/
#https://s3.amazonaws.com/assets.datacamp.com/production/course_3805/slides/chapter3.pdf--------------------- explore data with shiny
#https://yang-tang.github.io/shinyjqui/
#https://github.com/dreamRs/esquisse----------------------------------------------------------for Data Visualisation
# https://emitanaka.org/shinycustomloader/----------------------------------------------------spinner
# https://goodekat.github.io/redres/articles/redres-vignette.html#shiny-app-----------------evaluate LMMs with shiny
# https://rdrr.io/cran/lme4/man/findbars.html ----------------------------------findbars: Determine random-effects expressions from a formula
# https://www.stat.uga.edu/sites/default/files/FactorsInR.pdf
# https://hosting.analythium.io/run-shiny-apps-locally/ ----------------------------------- hosting Shiny apps

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


#######################################
#######################################
# UI
#######################################

ui <- fluidPage(theme = shinytheme("spacelab"),
                useShinyjs(),
                tags$head(#-------------------------------------------------------------- global style options
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
                                       actionButton("pdf", "QPCRinR User's Guide", onclick = "window.open('QPCRinR_Manual.pdf')"),
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
                                                h5("v.0.9.7.3")
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
                            ), # tabPanel "Welcome"
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
                                       ), # sidebarPAnel
                                       
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
                                       ) # mainPanel
                                     ) # sidebarLayout
                            ), # tabPanel "Load Data"
                            
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
                                     ) # sidebarLayout  
                            ), # tabPanel "ModelSpecification"
                            
                            tabPanel("Contrast Assignment",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    uiOutput("ChooseRefGene"),
                                                    hr(),
                                                    uiOutput("CntrYesNo"),
                                                    uiOutput("conditionalInput"),
                                                    uiOutput("ContrFact"),
                                                    uiOutput("ContrConfirm")
                                       ), # sidebarPAnel
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
                                     ) # sidebarLayout
                            ), # tabPanel "Contrast Assignment"
                            
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
                                         #  uiOutput("DownloadPlotBttn"),
                                         plotlyOutput("plot"),
                                         fixedPanel(
                                           actionButton("toContrastAssignmentB", label = "", icon("angle-left"), style = "background-color: #e0afaf"),
                                           center = 10,
                                           bottom = 10
                                         )
                                       ) # mainPanel
                                     ) # sidebarLayout
                            ) # tabPanel "Results"
                ) # tabsetPanel
) # fluidPage


#######################################
#######################################
# SERVER
#######################################


server <- function(input, output, session) {
  options(shiny.maxRequestSize = 200*1024^2) # 200MB file max for upload
  
  # Function for closing the App when closing browser
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # observeEvent(input$pdf, {
  #   # Absolute path to a pdf
  #   onclick({window.open('rQPCR_Manual.pdf')})
  # })
  
  #__________________________________________________________________ Back and Forward Buttons
  observeEvent(input$launch, {
    updateTabsetPanel(inputId = "qPCR", selected = "Load Data")
  })
  observeEvent(input$toWelcome, {
    updateTabsetPanel(inputId = "qPCR", selected = "Welcome")
  })
  observeEvent(input$toModelSpec, {
    updateTabsetPanel(inputId = "qPCR", selected = "Model Specification")
  })
  observeEvent(input$toLoadData, {
    updateTabsetPanel(inputId = "qPCR", selected = "Load Data")
  })
  observeEvent(input$toContrastAssignment, {
    updateTabsetPanel(inputId = "qPCR", selected = "Contrast Assignment")
  })
  observeEvent(input$toModelSpecB, {
    updateTabsetPanel(inputId = "qPCR", selected = "Model Specification")
  })
  observeEvent(input$toResult, {
    updateTabsetPanel(inputId = "qPCR", selected = "Results")
  })
  observeEvent(input$toContrastAssignmentB, {
    updateTabsetPanel(inputId = "qPCR", selected = "Contrast Assignment")
  })
  
  output$structure <- c()
  output$modelTable <- c()
  output$ModelSummary <- c()
  output$emmeans <- c()
  output$Contrasts <- c()
  
  #___________________________________________________________________Load Data
  rv <- reactiveValues() # create reactive val
  
  observe({
    req(input$demo)
    rv$loadedData <- read.delim("DemoSet.txt", # initialize reactiveVal
                                header = input$header,
                                sep = input$sep,
                                stringsAsFactors = input$stringAsFactors,
                                quote = "")
  })
  
  observe({
    req(input$main)
    shinyCatch({
      if(input$readXLSX == TRUE){
        rv$loadedDat <- read.xlsx(req(input$main)$datapath,
                                  sep.names = "_",
                                  colNames = input$header)
        
      } else {
        rv$loadedDat <- read.delim(req(input$main)$datapath, # initialize reactiveVal
                                   header = input$header,
                                   sep = input$sep,
                                   stringsAsFactors = input$stringAsFactors,
                                   quote = "")
      }
      prefix = "QPCRinR"})
    
  })  # observe input main
  
  observeEvent(rv$loadedDat, {
    datt <- rv$loadedDat
    if(length(colnames(datt))>1 & !isTruthy(grep("^[Gg][Ee][Nn][Ee][Ss]?$", colnames(datt)))){
      showModal(modalDialog(
        tags$h2('Define Gene Name Column'),
        radioButtons('defineGene',
                     'Select Column that contains gene or locus names',
                     choices = colnames(datt)),
        helpText(a("Notification: Column name will change to \"Gene\" after selection")),
        footer=tagList(
          actionButton('submitGene', 'Submit'),
          modalButton('Cancel')),
        easyClose = TRUE
      ))
    } else {
      rv$loadedData <- rv$loadedDat
    }
  })
  
  observe({
    req(input$submitGene)
    if(isTruthy(input$submitGene)){
      print(input$defineGene)
      colnames(rv$loadedDat)
      daa <- rv$loadedDat
      colnames(daa)
      colnames(daa) <- gsub(as.character(input$defineGene), "Gene", colnames(daa))
      head(daa)
      rv$loadedData <- daa
      removeModal()
    }
  })
  
  observeEvent(rv$loadedData, {
    rv$LMModel <- c()
    rv$tableInfo <- c()
    rv$LMMEmmeans <- c()
    rv$Contrasts <- c()
    rv$OutTable <- c()
    rv$DummyOut <- c()
    rv$plot <- c()
    output$data <- DT::renderDataTable({DT::datatable(rv$loadedData,
                                                      options = list(initComplete = JS(
                                                        "function(settings, json) {",
                                                        "$(this.api().table().header()).css({'background-color': '#940b01', 'color': '#d6d4d4'});",
                                                        "}")
                                                      ))
    })
    output$structure <- renderPrint(str(rv$loadedData))
    
    output$advancedclassChange <- renderUI({
      tagList(
        h4("Variable Definition"),
        helpText("Choose class of each variable"),
        helpText(a("Important: Set all variables to \"factor\", EXCEPT CT which should be set as \"numeric\"")),
        selectInput("VariableSelection", "Variable Name", choices = names(rv$loadedData)),
        selectInput("classChangecolumn", "Variable Class Type", choices = c(factor='factor', integer='integer',numeric='numeric')),
        actionButton("classChange", "Submit Class Change")
      )
    })  # renderUI advancedclassChange
  })
  
  observeEvent(input$classChange, {
    if( input$classChangecolumn == "numeric"){
      rv$loadedData[, input$VariableSelection] <- as.numeric(as.character(rv$loadedData[, input$VariableSelection]))
    } else if( input$classChangecolumn == "factor"){
      rv$loadedData[, input$VariableSelection] <- as.factor(rv$loadedData[, input$VariableSelection])
    } else if( input$classChangecolumn == "integer"){
      rv$loadedData[, input$VariableSelection] <- as.integer(rv$loadedData[, input$VariableSelection])
    }
  })  # observeEvent input$classChange
  
  #__________________________________________________________________Model Specification
  
  observeEvent(rv$loadedData, {
    output$callibrators <- renderUI({
      checkboxInput("calibr", label = "Inter-Run Calibration", value = FALSE)
      
    })
    
    output$SpecifyModel <- renderUI({
      tagList(
        bucket_list(
          header = "Drag items in the desired bucket",
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(text = strong("Model Components"), input_id = 'A', labels = names(rv$loadedData)),
          add_rank_list(text = strong("Response - Drag CT value here"), input_id = 'Response', labels = NULL),
          add_rank_list(text = strong("Fixed Effects"), input_id = 'FixedEff', labels = NULL),
          add_rank_list(text = strong("Random effects"), input_id = 'RandomEff', labels = NULL)
        ),
        br(),
        uiOutput("ManFormYesNo"),
        uiOutput("manualFormula"),
        actionButton("runModel", "Run Model!"),
        br()
      )
    })  # renderUI SpecifyModel
  })  # observeEvent input$main
  
  #______________________________________________________________Calibrator Model
  observe({
    req(input$calibr)
    conditionalPanel(condition = input$calibr == TRUE,
                     output$specCalib <- renderUI(
                       tagList(
                         textInput("calibID", "Please insert Calibrators' Tag", value = ""),
                         textInput("plateID", "Please insert Plate Column Name", value = "")
                       )  
                     )
    )
  })
  
  #______________________________________________________________For Modal; Manual Formula
  observe({
    req(rv$loadedData)
    output$ManFormYesNo <- renderUI({
      checkboxInput("show", "Or Input model formula manually", value = FALSE)
    })
    
    output$manualFormula <- renderUI({
      if(input$show){
        textInput("Formul", "Please enter model formula",
                  placeholder = "i.e., ct ~ gene * treatment1 * treatment2 + (1|sample)",
                  width = "700px",
                  value = "")
        
      }
    })
  })
  
  #______________________________________________________________Build Model and Emmeans
  observeEvent(input$runModel, {
    shinyjs::reset("confirmContrasts")
    output$Contrasts <- c()
    output$result <- c()
    output$plot <- c()
    
    shinyCatch({
      datt <- rv$loadedData
      
      if(isTruthy(input$show)){
        modelFormula <- as.formula(input$Formul)
        cal.Response <- as.character(modelFormula)[2]
        Response <- as.character(modelFormula)[2]
        
        if(isTruthy(input$calibr)){
          FindGene <- grep("^[Gg][Ee][Nn][Ee][Ss]?$", colnames(datt))
          
          dat.cal <- datt[grep(input$calibID, datt[,grep(input$calibID, sapply(datt, levels))]), ]
          dat.cal <- droplevels(dat.cal)
          nPlates <- length(levels(datt[ , grep(input$plateID, colnames(datt))]))
          
          cal.Fixed <- paste(input$plateID, colnames(datt[FindGene]), sep = "+")
          cal.Formula <- as.formula(paste(cal.Response, cal.Fixed, sep = "~"))
          
          mod.cal <- lm(cal.Formula, data = dat.cal)
          emm.cal <- emmeans(mod.cal, input$plateID)
          
          
          cf <- summary(emm.cal)$emmean
          cf <- median(cf)/cf
          
          indRes <- grep(Response, colnames(datt))
          indPlate <- grep(input$plateID, colnames(datt))
          
          for(i in 1 : nPlates){
            datt$nCt[datt[, indPlate] == levels(datt[, indPlate])[i]] <-  datt[, indRes][datt[, indPlate] == levels(datt[, indPlate])[i]] * cf[i]
          }
          
          datnewR <- datt[ ,-indRes]
          indNewCt <- grep("nCt", colnames(datnewR))
          colnames(datnewR)[indNewCt] <- Response
          
          datnewRnoCalib <- datnewR[-grep(input$calibID, rv$loadedData[,grep(input$calibID, sapply(rv$loadedData, levels))]), ]
          
          if(!isTruthy(findbars(modelFormula))){
            rv$LMModel <- lm(modelFormula, data = datnewRnoCalib)
          } 
          else {
            rv$LMModel <- lmer(modelFormula, data = datnewRnoCalib)
          }
          
          #____________condition for Calib ends here
        } else {
          
          if(!isTruthy(findbars(modelFormula))){
            rv$LMModel <- lm(modelFormula, data = datt)
          } 
          else {
            rv$LMModel <- lmer(modelFormula, data = datt)
          }
        }
        FixedEff <- strsplit(as.character(modelFormula)[3], split = " \\+ \\(") %>% sapply(extract2, 1)
        FixedEff <- paste("~ ", FixedEff, sep = "")
        rv$LMMEmmeans <- emmeans(rv$LMModel, as.formula(FixedEff))
        
        #------------------------------------------------------------------Define Reference Gene
        FindGene <- grep("^[Gg][Ee][Nn][Ee][Ss]?$", colnames(datt))
        
        output$ChooseRefGene <- renderUI({
          checkboxGroupInput(
            inputId = "ChooseRef",
            label = h4("Choose REFERENCE Gene"),
            choices = levels(datt[,FindGene])
          )
        }) # renderUI ChooseRefGene
        
        rv$tableInfo <- matrix(c(logLik(rv$LMModel), AIC(rv$LMModel), BIC(rv$LMModel)), 
                               nrow = 1, ncol = 3, byrow = FALSE,
                               dimnames = list(c(), c("logLik", "AIC", "BIC")))
        
        output$modelTable <- renderTable(rv$tableInfo)
        output$ModelSummary <- renderPrint(summary(rv$LMModel))
        output$emmeans <- renderPrint(summary(rv$LMMEmmeans))
        
      } #-------------------------------if isTruthy rv$formula, condition for manual formula
      
      else { #----------------------------------------------condition for dragged formula
        if(isTruthy(input$calibr)){
          FindGene <- grep("^[Gg][Ee][Nn][Ee][Ss]?$", colnames(datt))
          
          dat.cal <- datt[grep(input$calibID, datt[,grep(input$calibID, sapply(datt, levels))]), ]
          dat.cal <- droplevels(dat.cal)
          nPlates <- length(levels(datt[ , grep(input$plateID, colnames(datt))]))
          cal.Fixed <- paste(input$plateID, colnames(datt[FindGene]), sep = "+")
          cal.Response <- as.character(input$Response)
          cal.Formula <- as.formula(paste(cal.Response, cal.Fixed, sep = "~"))
          mod.cal <- lm(cal.Formula, data = dat.cal)
          emm.cal <- emmeans(mod.cal, input$plateID)
          
          cf <- summary(emm.cal)$emmean
          cf <- median(cf)/cf
          
          indRes <- grep(input$Response, colnames(datt))
          indPlate <- grep(input$plateID, colnames(datt))
          
          for(i in 1 : nPlates){
            datt$nCt[datt[, indPlate] == levels(datt[, indPlate])[i]] <-  datt[, indRes][datt[, indPlate] == levels(datt[, indPlate])[i]] * cf[i]
          }
          
          datnewR <- datt[ ,-indRes]
          indNewCt <- grep("nCt", colnames(datnewR))
          colnames(datnewR)[indNewCt] <- input$Response
          
          datnewRnoCalib <- datnewR[-grep(input$calibID, rv$loadedData[,grep(input$calibID, sapply(rv$loadedData, levels))]), ]
          
          if(!isTruthy(input$RandomEff)){
            Response <- as.character(input$Response)
            FixedEff <- as.character(input$FixedEff)
            FirstPart <- paste(Response, paste(FixedEff, collapse = " * "), sep = "~")
            modelFormula <- as.formula(FirstPart)
            rv$LMModel <- lm(modelFormula, data = datnewRnoCalib)
            
          } 
          
          else {
            
            Response <- as.character(input$Response)
            FixedEff <- as.character(input$FixedEff)
            RandomEff <- as.character(input$RandomEff)
            
            FirstPart <- paste(Response, paste(FixedEff, collapse = " * "), sep = "~")
            RandPart <- paste("(1|", paste(RandomEff, collapse = ")+(1|"), sep = "", paste(")"))
            modelFormula <- as.formula(paste(FirstPart, RandPart, sep = "+"))
            rv$LMModel <- lmer(modelFormula, data = datnewRnoCalib)
          }
          
          #____________condition for Calib ends here
        } else {
          
          if(!isTruthy(input$RandomEff)){
            Response <- as.character(input$Response)
            FixedEff <- as.character(input$FixedEff)
            FirstPart <- paste(Response, paste(FixedEff, collapse = " * "), sep = "~")
            modelFormula <- as.formula(FirstPart)
            rv$LMModel <- lm(modelFormula, data = datt)
          } 
          
          else {
            
            Response <- as.character(input$Response)
            FixedEff <- as.character(input$FixedEff)
            RandomEff <- as.character(input$RandomEff)
            
            FirstPart <- paste(Response, paste(FixedEff, collapse = " * "), sep = "~")
            RandPart <- paste("(1|", paste(RandomEff, collapse = ")+(1|"), sep = "", paste(")"))
            modelFormula <- as.formula(paste(FirstPart, RandPart, sep = "+"))
            rv$LMModel <- lmer(modelFormula, data = datt)
          }
        }
        rv$LMMEmmeans <- emmeans(rv$LMModel, input$FixedEff)
        
        #------------------------------------------------------------------Define Reference Gene
        FindGene <- grep("^[Gg][Ee][Nn][Ee][Ss]?$", colnames(datt))
        
        output$ChooseRefGene <- renderUI({
          checkboxGroupInput(
            inputId = "ChooseRef",
            label = h4("Choose REFERENCE Gene(s)"),
            choices = levels(datt[,FindGene])
          )
        }) # renderUI ChooseRefGene
        
        rv$tableInfo <- matrix(c(logLik(rv$LMModel), AIC(rv$LMModel), BIC(rv$LMModel)), 
                               nrow = 1, ncol = 3, byrow = FALSE,
                               dimnames = list(c(), c("logLik", "AIC", "BIC")))
        
        output$modelTable <- renderTable(rv$tableInfo)
        output$ModelSummary <- renderPrint(summary(rv$LMModel))
        output$emmeans <- renderPrint(summary(rv$LMMEmmeans))
      }
    }, prefix = "iQPCR")
    
  }) # observeEvent input$runModel
  
  #-----------------------------------------------------------------Define Control Treatments
  
  observe({
    req(input$ChooseRef)
    shinyCatch({
      output$CntrYesNo <- renderUI({
        tagList(
          actionButton("continueWithoutContr", "Proceed without Control"),
          checkboxInput("CntrTreatment", label = "Include Control", value = TRUE)
        )
      })
      
      output$conditionalInput <- renderUI({
        if(input$CntrTreatment){
          aa <- as.character(formula(rv$LMModel, fixed.only = TRUE))[3]    # ---------- get fixed part of model formula, [3] means only right part  
          FixedEfMod <- unlist(strsplit(aa, split = " "))[which(nchar(unlist(strsplit(aa, split = " "))) > 1)]
          MinusGene <- FixedEfMod[-grep("^[Gg][Ee][Nn][Ee][Ss]?$", FixedEfMod)]
          tagList(
            selectInput(inputId = "chooseCtrFact",
                        label = h4("Control in..."), 
                        choices = MinusGene, 
                        selected = NULL),
            actionButton("continueToContr", "... continue...")
          )
        }
      })
      output$ContrFact <- renderUI({
        if(input$continueToContr){
          tagList(
            selectInput("ChooseCntrls", label = h4("Choose Control"), 
                        choices = levels(rv$loadedData[,input$chooseCtrFact]), 
                        selected = NULL),
            actionButton("confirmContrasts", "Confirm Selection")
          )
        }
      })
    })
  })
  
  observeEvent(input$confirmContrasts, {
    shinyCatch({
      output$result <- c()
      output$plot <- c()
      #emGrid <- paste(rv$LMMEmmeans@grid[,1], rv$LMMEmmeans@grid[,2], rv$LMMEmmeans@grid[,3], sep = "_")
      levs <- rv$LMMEmmeans@levels
      newcols <- make.names(apply(expand.grid(levs),1,paste,collapse="_"))
      
      RefForGrep <- paste(input$ChooseRef, collapse = "|")
      
      tempList3 <- list()
      tempList4 <- list()
      refGposition <- grep(RefForGrep, levs)
      contTposition <- grep(input$ChooseCntrls, levs)
      
      ContForCrep <- input$ChooseCntrls
      
      if(contTposition == 1){
        ContForCrep <- make.names(ContForCrep)
      }else{
        ContForCrep <- ContForCrep
      }
      
      bin1 <- intersect(newcols[-grep(RefForGrep, newcols)],newcols[-grep(ContForCrep, newcols)])
      toBin2 <- strsplit(bin1, split = "_")
      toBin2 <- data.table::transpose(toBin2)
      toBin2[[contTposition]] <- rep(ContForCrep, length(toBin2[[contTposition]]))
      bin2 <- do.call("paste", c(toBin2, sep = "_"))
      
      toBin3 <- strsplit(bin1, split = "_")
      toBin3 <- data.table::transpose(toBin3)
      toBin3[[refGposition]] <- sort(rep(input$ChooseRef, length(toBin3[[refGposition]])))
      bin3 <- do.call("paste", c(toBin3, sep = "_"))
      
      toBin4 <- strsplit(bin1, split = "_")
      toBin4 <- data.table::transpose(toBin4)
      toBin4[[contTposition]] <- rep(ContForCrep, length(toBin4[[contTposition]]))
      toBin4[[refGposition]] <- sort(rep(input$ChooseRef, length(toBin4[[refGposition]])))
      bin4 <- do.call("paste", c(toBin4, sep = "_"))
      
      for(i in seq_along(bin1)){
        tempList3[[i]] <- list()
        tempList4[[i]] <- list()
        for(k in 1:length(input$ChooseRef)){
          j = k - 1
          tempList3[[i]][[k]] <- unlist(c(if (!is.na(bin3[i+j*length(bin1)])) list(bin3[i+j*length(bin1)]))
          )
          tempList4[[i]][[k]] <- unlist(c(if (!is.na(bin4[i+j*length(bin1)])) list(bin4[i+j*length(bin1)]))
          )
        }
        tempList3[[i]] <- unlist(tempList3[[i]])
        tempList4[[i]] <- unlist(tempList4[[i]])
      }
      
      # for(i in seq_along(bin1)){
      #   tempList3[[i]] <- unlist(c(if (!is.na(bin3[i])) list(bin3[i]),
      #                              if (!is.na(bin3[i+length(bin1)])) list(bin3[i+length(bin1)]),
      #                              if (!is.na(bin3[i+2*length(bin1)])) list(bin3[i+2*length(bin1)]),
      #                              if (!is.na(bin3[i+3*length(bin1)])) list(bin3[i+3*length(bin1)])
      #   ))
      # }
      
      bin3 <- paste("(", unlist(lapply(tempList3, paste, collapse = " + ")), sep = "")
      bin3 <- paste(1/length(input$ChooseRef), bin3, sep = " * ")
      bin3 <- paste(bin3, ")", sep = '')
      
      bin4 <- paste("(", unlist(lapply(tempList4, paste, collapse = " + ")), sep = "")
      bin4 <- paste(1/length(input$ChooseRef), bin4, sep = " * ")
      bin4 <- paste(bin4, ")", sep = '')
      
      makContrasts <- c()
      MContr <- list()
      inContr <- list()
      
      for(k in 1:length(bin1)){
        temp <- paste(bin1[k], bin2[k], bin3[k], sep = " - ")
        temp1 <- paste(temp, bin4[k], sep = " + ")
        temp1 <- paste("-(", temp1, sep = "")
        makContrasts[k] <- paste(temp1, ")", sep = "")
      }
      print(makContrasts)
      MContr <- list()
      for (m in 1:length(bin1)){
        MContr[[bin1[m]]] <- do.call("makeContrasts", list(makContrasts[m], levels = newcols))
      }
      
      inContr <- list()
      for (p in 1:length(bin1)){
        inContr[[bin1[p]]] <- MContr[[p]][1:length(MContr[[p]])]
      }
      
      Contr <- contrast(rv$LMMEmmeans, inContr, by=NULL)
      rv$Contrasts <- Contr
      
      output$Contrasts <- renderPrint(rv$Contrasts)
    })
  }) # observeEvent input$confirmContrasts
  
  
  observeEvent(input$continueWithoutContr, {
    shinyCatch({
      output$result <- c()
      output$plot <- c()
      #emGrid <- paste(rv$LMMEmmeans@grid[,1], rv$LMMEmmeans@grid[,2], rv$LMMEmmeans@grid[,3], sep = "_")
      levs <- rv$LMMEmmeans@levels
      newcols <- make.names(apply(expand.grid(levs),1,paste,collapse="_"))
      RefForGrep <- paste(input$ChooseRef, collapse = "|")
      
      tempList3 <- list()
      tempList4 <- list()
      refGposition <- grep(RefForGrep, levs)
      
      bin1 <- newcols[-grep(RefForGrep, newcols)]
      
      toBin3 <- strsplit(bin1, split = "_")
      toBin3 <- data.table::transpose(toBin3)
      toBin3[[refGposition]] <- sort(rep(input$ChooseRef, length(toBin3[[refGposition]])))
      bin3 <- do.call("paste", c(toBin3, sep = "_"))
      
      for(i in seq_along(bin1)){
        tempList3[[i]] <- list()
        for(k in 1:length(input$ChooseRef)){
          j = k - 1
          tempList3[[i]][[k]] <- unlist(c(if (!is.na(bin3[i+j*length(bin1)])) list(bin3[i+j*length(bin1)]))
          )
        }
        tempList3[[i]] <- unlist(tempList3[[i]])
      }
      
      bin3 <- paste("(", unlist(lapply(tempList3, paste, collapse = " + ")), sep = "")
      bin3 <- paste(1/length(input$ChooseRef), bin3, sep = " * ")
      bin3 <- paste(bin3, ")", sep = '')
      
      makContrasts <- c()
      MContr <- list()
      inContr <- list()
      
      for(k in 1:length(bin1)){
        temp <- paste(bin1[k], bin3[k], sep = " - ")
        temp1 <- paste("-(", temp, sep = "")
        makContrasts[k] <- paste(temp1, ")", sep = "")
      }
      
      MContr <- list()
      for (m in 1:length(bin1)){
        MContr[[bin1[m]]] <- do.call("makeContrasts", list(makContrasts[m], levels = newcols))
      }
      
      inContr <- list()
      for (p in 1:length(bin1)){
        inContr[[bin1[p]]] <- MContr[[p]][1:length(MContr[[p]])]
      }
      
      Contr <- contrast(rv$LMMEmmeans, inContr, by=NULL)
      rv$Contrasts <- Contr
      
      output$Contrasts <- renderPrint(rv$Contrasts)
    })
  }) # observeEvent input$continueWithoutContr
  
  
  #___________________________________________________________________________________Results
  
  observe({
    req(rv$Contrasts)
    shinyCatch({
      rv$plot <- c()
      
      dfToPlot <- data.frame(rv$Contrasts)
      dfToPlot <- na.omit(dfToPlot)
      
      contNameLen <- length(names(rv$Contrasts@misc$orig.grid))
      newDF <- list()
      for(i in 1:contNameLen){
        newDF[[names(rv$Contrasts@misc$orig.grid)[i]]] <- strsplit(as.character(dfToPlot$contrast), "_") %>% sapply(extract2, i)
      }
      
      rv$OutTable <- cbind(newDF, dfToPlot [,-1])
      
      output$result <- DT::renderDataTable({DT::datatable(rv$OutTable,
                                                          options = list(initComplete = JS(
                                                            "function(settings, json) {",
                                                            "$(this.api().table().header()).css({'background-color': '#940b01', 'color': '#d6d4d4'});",
                                                            "}")
                                                          ))
      })
      output$DownloadTable <- renderUI({
        downloadButton("DownLoadResult", label = "Download Result Table")
      })
      
      if(isTruthy(input$submitMerge)){
        ToPlot2 <- cbind(newDF, dfToPlot [,-1])
        output$plot <- c()
        # Merged <- input$inputMerge
        rv$DummyOut$Merged <- apply(ToPlot2[ ,input$inputMerge] , 1 , paste , collapse = "_" )
        removeModal()
      } else {
        rv$DummyOut <- rv$OutTable
      }
      print(rv$DummyOut)
      output$forPlot <- renderUI({
        tagList(
          selectInput(inputId = "chooseX",
                      label = h5("Define X-axis"), 
                      choices = names(rv$DummyOut)[!names(rv$DummyOut) %in% c("estimate", "SE", "df", "t.ratio", "p.value")], 
                      selected = NULL),
          selectInput(inputId = "chooseY",
                      label = h5("Define Y-axis"), 
                      choices = "estimate", 
                      selected = "estimate"),
          selectInput(inputId = "fill",
                      label = h5("Fill by"), 
                      choices = names(rv$DummyOut)[!names(rv$DummyOut) %in% c("estimate", "SE", "df", "t.ratio", "p.value")], 
                      selected = NULL),
          selectInput(inputId = "FacetBy",
                      label = h5("Split by"), 
                      choices = names(rv$DummyOut)[!names(rv$DummyOut) %in% c("estimate", "SE", "df", "t.ratio", "p.value")], 
                      selected = NULL),
          column(12,
                 splitLayout(cellWidths = c("60%", "60%"),
                             actionButton("merge", "Merge Factors",
                                          class = "btn-info"),
                             actionButton("createPlot", "Create Plot")
                 )
          ),
          br(),
          hr(),
          h4("Select properties of plot"),
          sliderInput("height", "Height", min = 100, max = 3000, value = 500),
          sliderInput("width", "Width", min = 100, max = 3000, value = 1000),
          sliderInput("Xrotation", "X Label Rotation", min = 0, max = 90, value = 30),
          selectInput(inputId = "theme",
                      label = "Plot theme", 
                      choices = c("theme_grey", "theme_bw", "theme_linedraw", "theme_light", "theme_dark",
                                  "theme_minimal", "theme_classic", "theme_void", "theme_test"),
                      selected = "theme_bw"),
          selectInput(inputId = "palette",
                      label = "Color Palette", 
                      choices = c("ColSet_16","ColPast_16","ColSet_21","Set1","Set2","Set3","Pastel1",
                                  "Pastel2","Dark2","Accent","Paired","Spectral"),
                      selected = "ColSet_21")
        )
      })
    })
  })  # observeEvent input$confirmContrasts
  
  observeEvent(input$merge, {
    shinyCatch({
      showModal(modalDialog(
        tags$h2('Merge Factors'),
        checkboxGroupInput('inputMerge',
                           'Choose Factors to Merge',
                           choices = names(rv$Contrasts@misc$orig.grid)),
        footer=tagList(
          actionButton('submitMerge', 'Submit'),
          modalButton('Cancel')),
        easyClose = TRUE
      ))
    })
  })
  
  observeEvent(input$createPlot, {
    pd = position_dodge(0.91)    ### How much to jitter the points on the plot
    req(rv$DummyOut)
    shinyCatch({
      toPlot <- rv$DummyOut
      toPlot$plus2se <- toPlot$estimate + (2 * toPlot$SE)
      toPlot$minus2se <- toPlot$estimate - (2 * toPlot$SE)
      th <- input$theme
      ColSet_16 <- c("#B3E2CD","#FDCDAC","#CBD5E8","#F4CAE4","#E6F5C9","#FFF2AE"
                    ,"#F1E2CC","#CCCCCC","#66C2A5","#FC8D62","#8DA0CB","#E78AC3"
                    ,"#A6D854","#FFD92F","#E5C494","#B3B3B3")
      ColPast_16 <- c("#FBB4AE","#B3CDE3","#CCEBC5","#DECBE4","#FED9A6","#FFFFCC"
                    ,"#E5D8BD","#FDDAEC","#E41A1C","#377EB8","#4DAF4A","#984EA3"
                    ,"#FF7F00","#FFFF33","#A65628","#F781BF")
      ColSet_21 <- c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462"
                    ,"#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F"
                    ,"#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33"
                    ,"#A65628","#F781BF","#999999")
      Set1 <- brewer.pal(n = 9, name = "Set1")
      Set2 <- brewer.pal(n = 8, name = "Set2")
      Set3 <- brewer.pal(n = 12, name = "Set3")
      Pastel1 <- brewer.pal(n = 9, name = "Pastel1")
      Pastel2 <- brewer.pal(n = 8, name = "Pastel2")
      Dark2 <- brewer.pal(n = 8, name = "Dark2")
      Accent <- brewer.pal(n = 8, name = "Accent")
      Paired <- brewer.pal(n = 12, name = "Paired")
      Spectral <- brewer.pal(n = 11, name = "Spectral")
      
      output$plot <- renderPlotly(
        {
          rv$plot <- ggplot(toPlot, aes_string(x = input$chooseX, y = input$chooseY, fill = input$fill)) +
            geom_bar(position = pd, stat = "identity") + 
            geom_errorbar(aes(ymin = minus2se, ymax = plus2se,  color = p.value), width = 0.03, size = 0.5, position = pd) +
            scale_color_gradientn(colours = c("springgreen1", "palegreen4", "black"), values = c(0, 0.05, 1)) +
            geom_hline(yintercept = 0) +
            facet_grid(. ~ get(input$FacetBy), scales = "free", space = "free") +
            scale_y_continuous(name = "log2FC",
                               breaks = c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                               label = c(14,13,12,11,10,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                               sec.axis = sec_axis(~. *1, name="FC",
                                                   breaks = c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14), 
                                                   label = c(16384,8192,4096,2048,1024,512,256,128,64,32,16,8,4,2,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384))) +
            ggtitle(input$main$name, subtitle = "") +
            labs(caption  = paste0("Error bars indicate the 95% CI of the EMmean.\nBaseline at log2FC=0 (FC=1) represents Control Level.
                                 Estimated means are significantly different than those of Controls when\nerror bars do not cross the baseline."), hjust=0.5) +
            scale_fill_manual(values = get(input$palette)) +
            match.fun(stringr::str_sub(input$theme, 1))() +
            theme(axis.text.x = element_text(size = 11, vjust = 1, angle = input$Xrotation))
          
          #       ggsave("plot.pdf", rv$plot)
          ggplotly(rv$plot,
                   width = input$width,
                   height = input$height)
        }
      )
      
      # output$DownloadPlotBttn <- renderUI({
      #   downloadButton("DownLoad_Plot", label = "Download Plot")
      # })
    })
  })  # observeEvent input$drawPlot
  
  # data_react <- reactive({
  #   data_table <- data.frame(rv$OutTable)
  # })
  output$DownLoadResult <- downloadHandler(
    filename = function() {
      paste(input$main$name, '.QPCRinR_Output_', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(rv$OutTable, file)
    }
  )
  
  # output$Download_Plot <- downloadHandler(
  #   filename = function() {
  #     "plot.pdf"
  #   },
  #   content = function(file) {
  #     file.copy("plot.pdf", file, overwrite=TRUE)
  #   }
  # )
  
  
}  # server function


shinyApp(ui, server)








