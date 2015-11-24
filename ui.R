
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
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
        tabPanel("Summary", tableOutput("flow_table")),
        tabPanel("Code", uiOutput("code"),
                 downloadButton('rscript', 'Download Rscript'))
      )))),
  hr(),
    fluidRow(
      column(4,
      ########### flow form
             wellPanel(
               h3("Flow details"), hr(),
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
               actionButton("refresh", "Refresh")
               #,submitButton(text = "Apply Changes", icon = NULL)
             )),
      #htmlOutput("flow_table"),
      #column(8,wellPanel(verbatimTextOutput("summary"))),
      column(8, uiOutput("jobs"))
    ) ## fluid row
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
  )## FLUID PAGE
) ## SHINY UI
