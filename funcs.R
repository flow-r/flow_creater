as.c=as.character

get_jobnames <- function(x, drop){ ## x is the input; drop numeric index to drop
  nms <- names(x);
  job_attrs <- grep("job-[0-9]*_name", nms, value=TRUE)
  job_attrs <- job_attrs[-drop] ## remove itself
  nms  <- lapply(job_attrs, function(y) x[[y]])
  names(nms) <- nms
  nms <- c(nms, "none" = NA)
  return(nms)
}

get_flow_table <- function(input){
  nms <- names(input)
  #cat(nms,"\n\n");
  job_attrs <- grep("job-", nms, value=TRUE)
  if(length(job_attrs) < 1) return() ## need a few jobs
  job_values  <- sapply(job_attrs, function(x) input[[x]])
  tmp <- do.call(rbind, strsplit(job_attrs, "_"))
  tmp <- data.frame(tmp, job_attrs, job_values)
  #print(tmp)
  names(tmp) <- c("jobid", "attr", "inputid", "value")
  dat <- dcast(tmp, formula = jobid ~ attr, value.var = 'value' )
  ## prev_jobs dep_type sub_type cpu nodes jobid prev_jobid
  jobnames <- dat$name
  jobid <- 1:length(jobnames);names(jobid)=jobnames
  prev_jobid <- jobid[as.c(dat$prevjob)]
  #print(prev_jobid); print(jobid)
  flow_dat <- data.frame(jobnames=dat$name, prev_jobs=dat$prevjob, 
                         dep_type=dat$deptype,sub_type=dat$subtype, cpu=dat$cpu,
                         jobid=jobid, prev_jobid=prev_jobid, cmd = dat$cmd,
                         stringsAsFactors=FALSE)
  return(flow_dat)
}


newjobform <- function(input, index){
  i=index
  jobname <- sprintf("job-%d_name",i)
  cmd <- sprintf("job-%d_cmd",i)
  cpu <- sprintf("job-%d_cpu",i)
  prevjob <- sprintf("job-%d_prevjob",i)
  subtype <- sprintf("job-%d_subtype",i)
  deptype <- sprintf("job-%d_deptype",i)
  ## ------------- get values; if dont exist use default
  jobname_v <- ifelse(length(input[[jobname]]) > 0, input[[jobname]], sprintf("myjob%s",i) )
  cmd_v <- ifelse(length(input[[cmd]]) > 0, input[[cmd]], "sleep 1" )
  cpu_v <- ifelse(length(input[[cpu]]) > 0, input[[cpu]], 1 )
  #print(head(jobname_v, 2))
  jobnames <- get_jobnames(input, i)
  prevjob_v <- ifelse(length(input[[prevjob]]) > 0, input[[prevjob]], tail(jobnames, 2 ))
  subtype_v <- ifelse(length(input[[subtype]]) > 0, input[[subtype]], 'serial' )
  deptype_v <- ifelse(length(input[[deptype]]) > 0, input[[deptype]], 'serial' )
  ret <- list( 
    #column(3, wellPanel(
    #h3(sprintf("%s", jobname_v)), hr(),
    textInput(inputId = jobname, label="Job Name", value = jobname_v),
    textInput(inputId = cmd, label="Command", value= cmd_v),
    #numericInput(inputId = cpu, label="cpu", value = cpu_v),
    selectInput(inputId = cpu, label="cpu", selected = cpu_v, choices = seq(1,64)),
    selectInput(inputId = subtype, label="Submission Type",
                choices = list("Scatter" = 'scatter', "Serial" = 'serial'), selected = subtype_v),
    selectInput(inputId = deptype, label="dependency_type",
                choices = list("gather" = 'gather', "Serial" = 'serial'), selected = deptype_v),
    if(length(jobnames) > 0){#print(jobnames);print(prevjob_v)
      selectInput(inputId = prevjob, label="previous_job", choices = jobnames, 
                  selected = prevjob_v)
    }
  )
  return(list(elements=ret, name=jobname_v))
}


get_jobforms <- function(input){
    w <- lapply(1:input$num_jobs, function(i) {
      tmp <- newjobform(input, index=i)
      tabPanel(title = tmp$name, wellPanel(tmp$elements))
      #w <- c(w, tmp)
    })
    ret <- do.call(tabsetPanel, w)
  return(ret)
}

# generate code from dat
get_dat_flowcode <- function(x, input,...){
  ## this would take in a flowdef and produce a code to generate it
  jobnames <- x$jobnames;#sapply(fobj@jobs, slot, "name")
  code_initial <- c("# ----- install flowr","install.packages('flowr')", 
                    "# ----- load library","library(flowr)\n",
                    "# ----- Thus defines the cluster and job submission","qobj <- queue(q_type = 'lsf', q_queue = 'normal')\n",
                    "# ----- Define a few job commands"
                    )
  code_jobs <- sapply(1:nrow(x), function(i){
    #prev_jobs=fobj@jobs[[j]]@previous_job;prev_jobs <- ifelse(length(prev_jobs) > 1, prev_jobs, "none")
    #cpu=fobj@jobs[[j]]@cpu;cmds=fobj@jobs[[j]]@cmds
    j <- x$jobnames[i]
    prev_jobs <- x$prev_jobs[i];
    cpu <- x$cpu[i];
    cmd <- x$cmd[i]
    dep_type <- x$dep_type[i]
    sub_type <- x$sub_type[i]
    code_cmd <- sprintf("cmd_%s <- '%s'", j, cmd)
    code_job <- sprintf("jobj_%s <- job(name = '%s', q_obj = qobj, previous_job = '%s', 
                         cpu = '%s', cmd = cmd_%s, 
                         submission_type = '%s', dependency_type = '%s')",
                        j, j, prev_jobs, cpu, j, dep_type, sub_type)
    return(c(code_cmd, code_job))
  })
  code_flow <- sprintf("fobj <- flow(name = '%s', jobs = list(%s), desc='flow_instance_description')",
                        input$flow_name, paste('jobj',jobnames,sep="_", collapse=", "))
  code_final <- c("\n\n#####----- Define the flow", code_flow, 
                  "#####----- plot the flow", "plot_flow(fobj)", 
                  "#####----- submit the flow and the files getting created", "submit(fobj, execute = FALSE)",
                  "#####----- submit the flow and the files getting created", "submit(fobj, execute = TRUE)")
  return(c(code_initial, code_jobs, code_final))
}



