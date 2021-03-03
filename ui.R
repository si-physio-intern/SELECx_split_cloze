# Split Cloze Responses

library(shiny)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(rlang)
library(shiny)

# UI ----------------------------------------------------------------------


ui <- fluidPage(
    
    shinyFeedback::useShinyFeedback(),
    
    titlePanel("SELECx - Split Embedded Answer (Cloze)"),
    hr(),
    
    fluidRow(
        column(8,
               
               helpText("1",tags$sup("st"),": Download file from SELECx as .csv or .xlsx, Rename that file to English (Short names)"),
               helpText("2",tags$sup("nd"),": Prepare student's ID file that has student's id, student's name
           as column names:\'ID',\'Name' respectively"),
           
           helpText("3",tags$sup("rd"),": Upload Embedded Answer (Cloze) file from SELECx that has column \'Response...' "),
           
           fileInput("file", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload files",
                     placeholder = "choose file .csv or .xlsx",multiple = FALSE)
           
        ),
        
        column(4,
               downloadButton("download", "Download Splited Data .xlsx"),
               downloadButton("download_2", "Download Raw Data .xlsx")
        )
        
    ),
    
    fluidRow(
        column(6,
               helpText("4",tags$sup("th"),": Upload student's ID file and choose more column if you want"),
               fileInput("file_id", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload ID",
                         placeholder = "choose file .csv or .xlsx"),
               
               helpText("5",tags$sup("th"),": Select Column(s) that has Embedded Answer (Cloze) responses."),
               selectInput("cloze_cols","Select Cloze Responses", choices = NULL, multiple = TRUE),
               checkboxInput("pivot", "Pivot the output column to long format ?", value = F)
               
        ),
        
        column(4,offset = 2,
               checkboxInput("add_cols","Add more column from ID file ?",value = FALSE),
               uiOutput("select")
               
               
        )
        
    ),
    
    verbatimTextOutput("raw"),
    
    hr(),
    
    h2("Splited Data"),
    helpText("Each part of Embedded Answer (Cloze) will be split into multiple columns."),
    br(),
    dataTableOutput("table_split"),
    
    hr(),
    
    h2("Raw data"),
    helpText("Not yet split"),
    br(),
    dataTableOutput("table_cleaned")
    
    
)

