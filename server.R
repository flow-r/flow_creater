
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(gplots)
require(reshape2)
library(flowr)

source("funcs.R")
as.c=as.character
#source("~/Dropbox/public/github_flow/R/plot-funcs.R")

## http://shiny.rstudio.com/gallery/widgets-gallery.html
## http://stackoverflow.com/questions/22160372/r-shiny-how-to-build-dynamic-ui-text-input

## for each job we have:
## jobname
## cmd
## previous_job
## submission_type
## dependency_type

shinyServer(func = function(input, output, session){
  ##print(url_fields_to_sync)
  firstTime <- TRUE
  output$hash <- renderText({
    #url_fields_to_sync <- as.c(names(input))
    #if(length(url_fields_to_sync) == 0) 
    url_fields_to_sync = c("num_jobs", "refresh")
    newHash = paste(collapse=",",
                    Map(function(field) {
                      paste(sep="=",
                            field,
                            input[[field]])
                    },
                    url_fields_to_sync))
    # the VERY FIRST time we pass the input hash up.
    return(
      if (!firstTime) {
        newHash
      } else {
        if (is.null(input$hash)) {
          NULL
        } else {
          firstTime<<-F;
          isolate(input$hash)
        }
      }
    )
  })
  
  output$jobs = renderUI({
    ## make a new UI, when ever either of these change their value
    input$num_jobs;input$refresh
    isolate({get_jobforms(input)})
  })
  #outputOptions(output, 'jobs', suspendWhenHidden=FALSE)
  
  ##----------- get the dat to be used in the later steps
  #debug(get_flow_table)
  current_dat <- reactive({
    if (input$num_jobs < 1) return("") ##get this only if we have more than 0 jobs
    get_flow_table(input) 
  })
  
  ######## --------- flow summary table
  output$flow_table <- renderTable({
    ## need atleast one job
    #browser() 
    if (input$num_jobs < 1) return()
    dat <- current_dat()
    return(dat)
  }, include.rownames=FALSE)
  
  ######## --------- flow summary chart
  #debug(.plot_flow_dat)
  output$flow_diagram <- renderPlot({  
    #browser() 
    ## ------- proceed only when we have more than 2 jobs
    if (input$num_jobs < 2) return(textplot(c('Add some jobs using the slider below\n', 'Say more than 3 ...')))
    dat <- current_dat();  #browser() ;#print(dat)
    flow:::.plot_flow_dat(dat)
  })
  
  ######## ----------- download the plot made earlier
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$flow_name, '.pdf', sep='') },
    content = function(file) {
      if (input$num_jobs < 2) return("") ##get this only if we have more than 2 jobs
      pdf(file)
      dat <- current_dat()
      flow:::.plot_flow_dat(dat)
      dev.off()
    })
  
  current_flow_code <- reactive({
    get_dat_flowcode(x=current_dat(), input=input)
  })
  
  output$code <- renderUI({
    #debug(get_dat_flowcode)
    flow_code <- current_flow_code()
    #print(flow_code)
    ret <- pre(class="shiny-code",
               # we need to prevent the indentation of <code> ... </code>
               HTML(format(tags$code(
                 class="language-r",
                 #paste(readLines(file.path.ci(getwd(), rFile), warn=FALSE),collapse="\n")
                 paste(flow_code, collapse = "\n")
               ), indent = FALSE)))
    return(ret)
  })
  
  output$rscript <- downloadHandler(
    filename = function () { paste(input$flow_name, '.R', sep='')},
    content = function(file) {
      flow_code <- current_flow_code()
      write(flow_code, file)
    })
  
  
  
}) ## server


