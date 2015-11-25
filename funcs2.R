as.c = as.character
options(stringsAsFactors = FALSE)

# ---------------------------------------- get job names ------------------------------------------------------------

#' get job names
#' 
#' by default, one may remove a specific index. So that this works well for previous jobs
#' 
#' @params x is the input
#' @params drop numeric index to drop
get_jobnames <- function(input, drop){ 
  
  nms <- names(input);
  #print(nms)
  
  # get a list of job ids
  job_attrs <- sprintf("job-%s_name",  1:input$num_jobs)
  #print(job_attrs)

  
  if(!missing(drop))
    job_attrs <- job_attrs[-drop] ## remove itself
  
  nms  <- lapply(job_attrs, function(y) input[[y]])
  names(nms) <- nms

  
  nms <- c("none" = "none", nms)

  return(nms)
}


# ---------------------------------------- job forms ------------------------------------------------------------

get_jobforms <- function(input){
  w <- lapply(1:input$num_jobs, function(i) {
    
    tmp <- newjobform(input, index=i)
    
    tabPanel(title = tmp$name, wellPanel(tmp$elements))
    #w <- c(w, tmp)
  })
  ret <- do.call(tabsetPanel, w)

  # only return th  
  return(ret)
}


newjobform <- function(input, index){
  
  i = index
  
  #print(input)
  
  # input is a list with various values, stored as a 
  # get the variables to be used
  jobname <- sprintf("job-%d_name",i)
  cmd <- sprintf("job-%d_cmd",i)
  cpu <- sprintf("job-%d_cpu",i)
  prevjob <- sprintf("job-%d_prevjob",i)
  subtype <- sprintf("job-%d_subtype",i)
  deptype <- sprintf("job-%d_deptype",i)
  
  #print(head(jobid_v, 2))
  jobnames <- names(get_jobnames(input, i))
  
  # get values; if dont exist use default
  # decide on default values
  #dat = try(to_flowdat.reactivevalues(input))

  jobid_v <- ifelse(length(input[[jobname]]) > 0, input[[jobname]], sprintf("myjob%s",i) )
  cmd_v <- ifelse(length(input[[cmd]]) > 0, input[[cmd]], "sleep 1" )
  cpu_v <- ifelse(length(input[[cpu]]) > 0, input[[cpu]], 1 )
  
  # previous jobs is the second last in jobnames
  subtype_v <- ifelse(length(input[[subtype]]) > 0, input[[subtype]], 'serial' )

  default.prevjob = tail(jobnames, 1)
  #print(default.prevjob)
  prevjob_v <- ifelse(length(input[[prevjob]]) > 0, input[[prevjob]], default.prevjob)
  print(prevjob_v)
  
  
  # switch deptype, if previous jobs is selected
  default.deptype = ifelse(prevjob_v == "none", "none", "serial")
  deptype_v <- ifelse(length(input[[deptype]]) > 0, input[[deptype]], default.deptype )
  
  # switch previous job, if dep_type is selected
  prevjob_v = ifelse(prevjob_v == "none" &  deptype_v != "none", tail(jobnames, 1), prevjob_v)
  # if there are rows, where dep is none and prev job is NOT null
  deptype_v = ifelse(deptype_v == "none" &  prevjob_v != "none", "serial", deptype_v)

  ret <- list( 
    #column(3, wellPanel(
    #h3(sprintf("%s", jobid_v)), hr(),
    
    textInput(inputId = jobname, label="Job Name", value = jobid_v),
    textInput(inputId = cmd, label="Command", value= cmd_v),
    
    #numericInput(inputId = cpu, label="cpu", value = cpu_v),
    
    selectInput(inputId = cpu, label="cpu", selected = cpu_v, choices = seq(1,64)),
    selectInput(inputId = subtype, label="Submission Type",
                choices = list(scatter = 'scatter',
                               serial = 'serial'), selected = subtype_v),
    
    selectInput(inputId = deptype, label="dependency_type",
                choices = list(none = "none", 
                               gather = 'gather', 
                               serial = 'serial',
                               burst = "burst"), 
                selected = deptype_v)
    
  )
  
  if(length(jobnames) > 0){#print(jobnames);print(prevjob_v)
    ret2 =
      list(
        selectInput(inputId = prevjob,
                    label = "previous_job", 
                    choices = jobnames, 
                    selected = prevjob_v)
        
        # checkboxGroupInput(inputId = paste0(prevjob, "2"), 
        #                    label = 'previous_job2',
        #                    choices = jobnames, 
        #                    selected = prevjob_v)
      )
    ret = c(ret, ret2)
  }
  
  
  
  
  return(list(elements=ret, name=jobid_v))
}

# ---------------------------------------- get flowdef ------------------------------------------------------------

# create values from the job form, into a data.frame
to_flowdat.reactivevalues <- function(input){

  nms <- names(input)
  #cat(nms,"\n\n");
  
  job_attrs <- grep("job-", nms, value=TRUE)
  if(length(job_attrs) < 1) 
    return() ## need a few jobs
  
  job_values  <- sapply(job_attrs, function(x){
    val = input[[x]]
    ifelse(length(val) == 0, NA, paste(val, collapse = ","))
  })
  print(job_values)
    
  # table of job ID attrs
  tmp <- do.call(rbind, strsplit(job_attrs, "_"))
  
  tmp <- data.frame(tmp, job_attrs, job_values)
  
  #print(tmp)
  names(tmp) <- c("jobid", "attr", "inputid", "value")
  
  # subset only the ones we need
  message("number of jobs: ", input$num_jobs)
  
  # filter tmp
  tmp <- subset(tmp, tmp$jobid %in% paste0("job-", 1:input$num_jobs))
  message("dim of tmp: ", nrow(tmp))
  #print(tmp)
  
  dat <- dcast(tmp, formula = jobid ~ attr, value.var = 'value')

  
  
  # flowdat = data.frame(
  #   jobid = dat$jobid,
  #   attr = dat$attr,
  #   inputid = dat$inputid,
  #   value = dat$value)
  # 
  # 
  # flowdat[1:input$num_jobs, ]
  dat
  
}

to_flowmat.reactivevalues <- function(input){
  flowdat <- to_flowdat.reactivevalues(input)
  print(flowdat)
  
  # create a flow mat
  flowmat <- data.frame(samplename = "samp1", 
                        jobname = flowdat$name, 
                        cmd = flowdat$cmd)
  class(flowmat) = c("flowmat", "data.frame")
  print(flowmat)
  
  flowmat
  
}

#' @param input reactive values
to_flowdef.reactivevalues <- function(input){
  
  if (input$num_jobs < 1) 
    return("") 
  
  flowdat <- to_flowdat.reactivevalues(input)
  #print(flowdat)
  
  # create a flow mat
  flowmat <- to_flowmat.reactivevalues(input)
  # from this, get a flowdat
  #undebug(flowr:::to_flowdef.flowmat)
  flowdef = try(to_flowdef(flowmat, 
                       prev_jobs = flowdat$prevjob,
                       cpu_reserved = flowdat$cpu,
                       dep_type = flowdat$deptype,
                       sub_type = flowdat$subtype))
  

  #print(flowdef)
  
  flowdef
  
}


# no used
.get_flow_table <- function(input){
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




# ---------------------------------------- get code ------------------------------------------------------------

# generate code from dat
to_flowcode <- function(input,...){
  ## this would take in a flowdef and produce a code to generate it
  #sapply(fobj@jobs, slot, "name")
  mat = to_flowmat(input)
  def = to_flowdef(input)
  
  dat = dplyr::left_join(def, mat)
  
  jobnames <- def$jobnames;
  
  code_flowdef = code_final = ""
  
  
  code_initial <- c("# --------------- install flowr ---------------", 
                    'install.packages("flowr", repos = c(CRAN="http://cran.rstudio.com", DRAT="http://sahilseth.github.io/drat"))', 
                    "\n# --------------- load library ---------------",
                    "library(flowr)\n",
                    "\n# --------------- Create a list of commands ---------------"
                    )
  
  code_cmds <- lapply(1:nrow(def), function(i){
    j <- def$jobname[i]
    cmd <- mat$cmd[i]
    code_cmd <- sprintf("%s = '%s'", j, cmd)
  })
  
  code_cmds <- sprintf("cmds <- list(%s)\n\t", 
                       paste(sprintf("%s = '%s'", dat$jobname, dat$cmd), collapse = ",\n\t"))

  code_flowmat <- c(
    "\n# --------------- create a flowmat ---------------",
    "flowmat <- to_flowmat(cmds, samplename = 'samp1')")

  code_flowdef <- c(
    "\n# --------------- create a flow definition ---------------",
    "flowdef = to_flowdef(flowmat,",
    sprintf("\tprev_jobs = c( '%s' ),", paste(dat$prev_jobs, collapse = "', '")),
    sprintf("\ttcpu_reserved = c( '%s' ),", paste(dat$tcpu_reserved, collapse = "', '")),
    sprintf("\tdep_type = c( '%s' ),", paste(dat$dep_type, collapse = "', '")),
    sprintf("\tsub_type = c( '%s' )", paste(dat$sub_type, collapse = "', '")), ")")

  code_final <- c(
    "\n# --------------- submit the flow to HPCC---------------",
    "fobj = to_flow(flowmat, flowdef, execute = TRUE, platform = 'local')", 

    "\n# --- plot, using flow object OR flowdef",
    "plot_flow(flowdef)", 
    "plot_flow(fobj)",
    
    
    "\n# --- other functions",
    "status(fobj)",
    "# kill(fobj)"
    
    )

  # code_jobs <- sapply(1:nrow(x), function(i){
  #   #prev_jobs=fobj@jobs[[j]]@previous_job;prev_jobs <- ifelse(length(prev_jobs) > 1, prev_jobs, "none")
  #   #cpu=fobj@jobs[[j]]@cpu;cmds=fobj@jobs[[j]]@cmds
  #   j <- x$jobnames[i]
  #   prev_jobs <- x$prev_jobs[i];
  #   cpu <- x$cpu[i];
  #   cmd <- x$cmd[i]
  #   dep_type <- x$dep_type[i]
  #   sub_type <- x$sub_type[i]
  #   code_cmd <- sprintf("cmd_%s <- '%s'", j, cmd)
  #   code_job <- sprintf("jobj_%s <- job(name = '%s', q_obj = qobj, previous_job = '%s', 
  #                        cpu = '%s', cmd = cmd_%s, 
  #                        submission_type = '%s', dependency_type = '%s')",
  #                       j, j, prev_jobs, cpu, j, dep_type, sub_type)
  #   return(c(code_cmd, code_job))
  # })
  # code_flow <- sprintf("fobj <- flow(name = '%s', jobs = list(%s), desc='flow_instance_description')",
  #                       input$flow_name, paste('jobj',jobnames,sep="_", collapse=", "))
  # code_final <- c("\n\n#####----- Define the flow", code_flow, 
  #                 "#####----- plot the flow", "plot_flow(fobj)", 
  #                 "#####----- submit the flow and the files getting created", "submit(fobj, execute = FALSE)",
  #                 "#####----- submit the flow and the files getting created", "submit(fobj, execute = TRUE)")
  return(c(code_initial, code_cmds, code_flowmat, code_flowdef, code_final))
}



