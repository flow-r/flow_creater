
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
# devtools::install_github("sahilseth/params", ref = "devel")
# devtools::install_github("sahilseth/flowr", ref = "devel")


library(shiny)
library(dplyr)
as.c=as.character

## jobname
## cmd
## previous_job
## submission_type
## dependency_type
#### ----------- some styling
#     HTML('<style type="text/css">
#         .span4 .well { background-color: #00FFFF; }
#         </style>'), 
#     HTML('<style type="text/css">
#      .row-fluid .span4{width: 26%;}
#      </style>'),

hashProxy <- function(inputoutputID) {
  div(id=inputoutputID, class = inputoutputID, tag("div", ""));
}

shinyUI(
  fluidPage(
    includeHTML("URL.js"),
    hashProxy("hash"),
    titlePanel("Creating flows"),
    
    ## Sidebar with a slider input for number of bins
    fluidRow(column(12, mainPanel(
      tabsetPanel(
        
        tabPanel("Plot", plotOutput("flow_diagram"), 
                 downloadButton('downloadPlot', 'Download Plot')),
        
        tabPanel("Summary",
                 h3("Flow Definition"), 
                 "This is a table, describing various steps in the flow",
                 hr(),
                 tableOutput("flow_def"),
                 downloadButton('download_flowdef', 'Download flowdef')),
        
        tabPanel("Code", uiOutput("code"),
                 downloadButton('rscript', 'Download Rscript'))
        
      )))),
    
    hr(),
    
    fluidRow(
      column(4,
             ########### flow form
             
             wellPanel(
               h3("Flow details"), hr(),
               hr(),
               textInput(inputId = "flow_name", label="Flow Name", value= "myflow"),
               selectInput(inputId = "flow_q_type", label="type of computing cluster",width = '70%',
                           choices = list("lsf" = 'lsf', 
                                          "torque" = 'torque', 
                                          "moab" = "moab", 
                                          "sge" = "sge"), 
                           selected = 'lsf'),
               #actionButton("add_job","Add"),
               
               #numericInput(inputId = "num_jobs", label="Number of jobs", value = 1, min = 1),
               sliderInput(inputId = "num_jobs", label="Number of jobs", value = 1, min = 1, 
                           max = 10),
               
               #helpText("Minimum of 2 jobs are requied to make a flow chart"),
               actionButton("refresh", "Refresh"),
               #,submitButton(text = "Apply Changes", icon = NULL)
               hr(),
               h4("Purpose:"),
               "Purpose of this app is to aid the users in creating a skeleton flow definition.",
               hr(), 
               h4("Limitations:"), 
               "This UI is for convience purposes; and lacks several options available in the flowr package.", 
               br(), 
               "One such example is where we have a flow with multiple jobs running in parallel.",
               "Although flowr supports it, we are not able to expose it using this UI.", br(),
               "For example say we have three jobs A, B and C; and C depends on both A and B.", br(),
               "In the flow-definition, one may specify the previou_job for C as: `A,B`",
               br(), br()
               
               
             )),
      #htmlOutput("flow_table"),
      #column(8,wellPanel(verbatimTextOutput("summary"))),
      column(8, uiOutput("jobs"))
    ) ## fluid row
  )## FLUID PAGE
) ## SHINY UI






#htmlOutput("jobs"))

#   sidebarLayout(
#     sidebarPanel(
#       helpText("Start creating your flow"),
#       #       textInput(inputId = "job1", label="Job Name", value= "firstjob"),
#       #       textInput(inputId = "cpu", label="cpu", value= "firstjob"),
#       htmlOutput("selectInputs")     
#     ),    
#     mainPanel(  
#       helpText("Alternatives list"),
#       verbatimTextOutput("summary")
#     ),
