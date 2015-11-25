
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(gplots)
require(reshape2)
library(flowr)
source("funcs2.R")

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
  source("funcs2.R")
  
  
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

    input$num_jobs;
    input$refresh
    
    #print(get_jobnames(input))
    
    isolate({
      get_jobforms(input)
      })

    
  })
  
  #outputOptions(output, 'jobs', suspendWhenHidden=FALSE)
  
  #----------- get the dat to be used in the later steps
  #debug(get_flow_table)
  current_def <- reactive({

    if (input$num_jobs < 1) 
      return("") 
    
    # get this only if we have more than 0 jobs
    message("class of input is: ")
    #print(class(input))
    
    to_flowdef(input) 
    
  })
  
  #  --------- flow summary table, this is really a flowdef
  output$flow_def <- renderTable({
    
    ## need atleast one job
    #browser() 
    if (input$num_jobs < 1) 
      return()
    
    def <- current_def()

    return(as.data.frame(def))
  }, include.rownames=FALSE)
  
  # ------------------------------------- output flowdef -------------------------------
  
  output$download_flowdef <- downloadHandler(
    
    filename = function() { 
      paste(input$flow_name, '.def', sep='') 
    },
    
    content = function(file) {
      # get this only if we have more than 2 jobs
      
      def <- current_def()
      params::write_sheet(def, file)
      
    })
  
  # -------------------------- flow summary plot ----------------------------------------------------
  #debug(.plot_flow_dat)
  output$flow_diagram <- renderPlot({  
    #browser() 
    
    # ------- proceed only when we have more than 2 jobs
    if (input$num_jobs < 2) 
      return(textplot(c('Add some jobs using the slider below\n', 'Say more than 3 ...')))
    
    def <- try(current_def())

    if(class(def)[1] == "try-error"){
      textplot("please refresh ...")
    }
    else{
      plot_flow(def)
    }
    
  })
  
  
  # ------------------------------------- download the plot made earlier -------------------------------
  output$downloadPlot <- downloadHandler(
    
    filename = function() { paste(input$flow_name, '.pdf', sep='') },
    content = function(file) {
      # get this only if we have more than 2 jobs
      
      if (input$num_jobs < 2) 
        return("") 
      
      pdf(file)
      def <- current_def()
      plot_flow(def)
      dev.off()
      
    })
  
  current_flow_code <- reactive({
    
    to_flowcode(input=input)
    
  })
  
  # ------------------------------------- output code -------------------------------
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
    filename = function () { 
      paste(input$flow_name, '.R', sep='')
      },
    
    content = function(file) {
      flow_code <- current_flow_code()
      write(flow_code, file)
    })
  

  
  
}) ## server


