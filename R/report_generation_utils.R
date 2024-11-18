### Report generating functions ####

# Dependency tree for report generating functions
# generate_report
# generate_report_powerpoint
# generate_slide_ppt_report
# generate_report_word
# generate_report_pdf
# generate_report_html

# Dependency tree for plot generating functions
# observeEvent(variable summary)
# observeEvent(correlation)
# observeEvent(trend)
# generate_collection_single_variable
# generate_object_grid_screen


# top level function
generate_report <- function(dataset, variable_of_interest, variable_of_interest_role, correlate_flag, stratify_flag, stratify_var, labels_flag, legend_flag, report_format){

  report_output <- list()

  # powerpoint
  if(report_format == "Powerpoint") {

    tryCatch({
      showNotification("Generating report...!")
      report_output[["object_for_output"]] <- generate_report_powerpoint(
        dataset = dataset,
        variable_of_interest = variable_of_interest,
        variable_of_interest_role = variable_of_interest_role,
        correlate_flag = correlate_flag,
        stratify_flag = stratify_flag,
        stratify_var = stratify_var,
        labels_flag = labels_flag,
        legend_flag = legend_flag
      )
      showNotification("Creating report...!")
    }, error = function(e) {
      showNotification(paste0("Error in report generation"))
    })
  }
  # render Rmarkdown for rendering to other formats
  else if (report_format %in% c("Word document", "PDF", "HTML Web Page")){
    tryCatch({
      showNotification("Testing generate_generate_report_rmarkdown...")
      report_output[["object_for_output"]] <- generate_report_rmarkdown(dataset = dataset,
                                                                        variable_of_interest = variable_of_interest,
                                                                        variable_of_interest_role = variable_of_interest_role,
                                                                        correlate_flag = correlate_flag,
                                                                        stratify_flag = stratify_flag,
                                                                        stratify_var = stratify_var,
                                                                        labels_flag = labels_flag,
                                                                        legend_flag = legend_flag,
                                                                        report_format = report_format
      )
      # return(NULL)
    }, error = function(e) {
      showNotification(paste0("Error: ",e))
    })
  }

  report_output[["report_format"]] <- report_format

  return(report_output)
}

generate_report_powerpoint <- function(dataset, variable_of_interest=NA, variable_of_interest_role = NA,
                                       correlate_flag = F, stratify_flag = F, stratify_var = NA, labels_flag = F, legend_flag = F, palette = "Set1"){

  # load template and add introductory slide
  print("gen report ppt")

  pptx <- read_pptx("./report_templates/template_16x9_blank.pptx")
  pptx <- remove_slide(pptx, index=1)
  pptx <- add_slide(pptx, layout = "Title Slide", master="Office Theme")
  pptx <- ph_with(pptx, "Exploratory Data Analysis", location = loc_ctrtitle)

  # List of variable names
  # this is either the list of all variables we will plot, or the list of variables we will correlate our variable of interest with
  variable_names <- colnames(dataset[6:10])

  if(correlate_flag == F){
    withProgress(message = 'Report generation in progress', value = 0, detail="0%", {
      value <- reactiveVal(0)
      total_variables <- length(variable_names)
      for (variable_of_interest in variable_names) {
        print(variable_of_interest)

        # this contains plot and (qq+histo for numerics, barplot for factors) and a summary table
        collection_object_output <- generate_collection_single_variable(dataset = dataset,
                                                                        variable_of_interest = variable_of_interest,
                                                                        stratify_flag = stratify_flag,
                                                                        stratify_var = stratify_var,
                                                                        labels_flag = labels_flag,
                                                                        legend_flag = legend_flag,
                                                                        destination = "slide",
                                                                        palette = palette)

        pptx <- generate_slide_ppt_report(pptx = pptx,
                                          collection_object_output = collection_object_output)

        newValue <- value() + 1
        value(newValue)
        incProgress(1/total_variables,detail = paste0(value(),"/",total_variables," variables"))
      }
    })
  } else if (correlate_flag == T){
    withProgress(message = 'Report generation in progress', value = 0, detail="0%", {
      value <- reactiveVal(0)
      total_variables <- length(variable_names)
      for (correlation_variable in variable_names) {
        print(correlation_variable)
        # skip plotting variable against itself
        if(correlation_variable != variable_of_interest) {
          collection_object_output <- NA

          # this contains correlation plot only, for now
          if(variable_of_interest_role == "exposure") {
            collection_object_output <- generate_collection_single_variable(dataset = dataset,
                                                                            variable_of_interest = correlation_variable,
                                                                            correlation_var = variable_of_interest,
                                                                            stratify_flag = stratify_flag,
                                                                            stratify_var = stratify_var,
                                                                            labels_flag = labels_flag,
                                                                            legend_flag = legend_flag,
                                                                            destination = "slide",
                                                                            palette = palette)
          }
          else if (variable_of_interest_role == "outcome"){
            collection_object_output <- generate_collection_single_variable(dataset = dataset,
                                                                            variable_of_interest = variable_of_interest,
                                                                            correlation_var = correlation_variable,
                                                                            stratify_flag = stratify_flag,
                                                                            stratify_var = stratify_var,
                                                                            labels_flag = labels_flag,
                                                                            legend_flag = legend_flag,
                                                                            destination = "slide",
                                                                            palette = palette)
          }

          pptx <- generate_slide_ppt_report(pptx = pptx,
                                            collection_object_output = collection_object_output)

        }
        newValue <- value() + 1
        value(newValue)
        incProgress(1/total_variables,detail = paste0(value(),"/",total_variables," variables"))
      }

    })
  }

  # cmat <- dataset %>% select_if(check_numeric) %>% cor
  # png("corrplot1.png")
  # cmatp <- corrplot(cmat, method="color")
  # dev.off()
  #
  # pptx <- add_slide(pptx, layout = "Title and Content", master="Office Theme")
  # pptx <- ph_with(pptx, str_to_title("Correlation Matrix"), location=loc_title)
  # pptx <- ph_with(pptx,external_img(src = "corrplot1.png"), location=loc_body)

  # Save the PowerPoint presentation

  return(pptx)

}

generate_slide_ppt_report <- function(pptx, collection_object_output) {

  # this function only generates slides for a single variable
  # for a report creating descriptive summary for variables, we will have multiple plots possible (qq and histogram for numerics)
  # will also have a summary table
  # for a report comparing variables (as exposure or outcome), plots will contain a single plot, and we will not have a summary table

  # context variables
  variable_of_interest <- collection_object_output$variable_of_interest
  correlate_var <- collection_object_output$correlation_var
  stratify_flag <- collection_object_output$stratify_flag
  stratify_var <- collection_object_output$stratify_var

  # output objects
  slide_plots <- collection_object_output$plots
  summary_table <- collection_object_output$summary_table

  # generate Grob for title of slide
  slide_title <- variable_of_interest
  if(!is.na(correlate_var)) slide_title <- paste0(variable_of_interest," vs ",correlate_var)
  if(stratify_flag) slide_title <-paste0(slide_title," stratified by ", stratify_var)
  slide_title <- textGrob(str_to_title(slide_title), x=0.5, y=0.5, just="center",gp=gpar(fontsize = 20))

  # create slide for each plot present, in order they were created and added to the object
  for(x in 1:length(slide_plots)){
    plotgrid <- grid.arrange(
      ggplotGrob(slide_plots[[x]]),
      ncol = 1
    )

    slidegrid <- dml(
      grid.arrange(
        arrangeGrob(
          slide_title,
          plotgrid,
          heights = c(1,9)
        )
      )
    )

    # add fullsize plotgrid to presentation
    pptx <- add_slide(pptx, layout = "Blank", master="Office Theme")
    pptx <- ph_with(pptx, slidegrid, location = loc_fullsize)
  }

  # reports containing VoI as an outcome or exposure will not have a summary table, as of now

  if(!is.null(summary_table)){
    # add stats table to new slide
    pptx <- add_slide(pptx, layout = "Title and Content", master="Office Theme")
    pptx <- ph_with(pptx, str_to_title(variable_of_interest), location=loc_title)
    pptx <- ph_with(pptx, summary_table, location=loc_body)
  }

  # Return the modified pptx object
  return(pptx)
}

generate_report_rmarkdown <- function(dataset, variable_of_interest=NA, variable_of_interest_role = NA, correlate_flag = F,
                                      stratify_flag = F, stratify_var = NA, labels_flag = F, legend_flag = F, report_format,
                                      palette = "Set1"){
  # my goal here is to create all the objects that I'll be printing out, based on correlate_flag (summary vs correlation) and variable_type
  # save it into an RDS, create the RMD to load to RDS, then iterate through it and print the relevant plots

  # first, create all objects

  variable_names <- colnames(dataset[6:10])
  variable_type <- class(dataset[[variable_of_interest]])
  report_objects_for_export <- list(variable_objects = list())

  if(correlate_flag == F){
    # cycle through all variables, except stratify
    withProgress(message = 'Report generation in progress', value = 0, detail="0%", {
      value <- reactiveVal(0)
      total_variables <- length(variable_names)
      for (variable_name in variable_names) {
        print(variable_name)
        if(!(stratify_flag == T & variable_name == stratify_var)){
          report_objects_for_export[["variable_objects"]][[variable_name]] <-
            generate_collection_single_variable(dataset = dataset,
                                                variable_of_interest = variable_name,
                                                stratify_flag = stratify_flag,
                                                stratify_var = stratify_var,
                                                labels_flag = labels_flag,
                                                legend_flag = legend_flag,
                                                destination = "slide",
                                                palette = palette)
        }
        newValue <- value() + 1
        value(newValue)
        incProgress(1/total_variables,detail = paste0(value(),"/",total_variables," variables"))
      }
    })
  } else if (correlate_flag == T){
    withProgress(message = 'Report generation in progress', value = 0, detail="0%", {
      value <- reactiveVal(0)
      total_variables <- length(variable_names)
      for (correlation_variable in variable_names) {
        print(correlation_variable)
        # skip plotting variable against itself
        if(correlation_variable != variable_of_interest) {
          report_objects_for_export[["variable_objects"]][[variable_name]] <-
            generate_collection_single_variable(dataset = dataset,
                                                variable_of_interest = variable_of_interest,
                                                correlation_var = correlate_var,
                                                stratify_flag = stratify_flag,
                                                stratify_var = stratify_var,
                                                labels_flag = labels_flag,
                                                legend_flag = legend_flag,
                                                destination = "slide",
                                                palette = palette)
        }
        newValue <- value() + 1
        value(newValue)
        incProgress(1/total_variables,detail = paste0(value(),"/",total_variables," variables"))
      }
    })
  }

  # save the report_objects_for_export object to a file that will be read by the rmarkdown file

  saveRDS(report_objects_for_export, file = "report_objects.rds")

  # begin creation of string that will be responsible for reading rds file above and iterating and printing all
  # objects to file

  newline_char <- case_when(report_format == "PDF" ~ "\newpage", report_format == "Microsoft Word" ~ "***", T ~ "")
  rmd_content <- paste0(
    "---\n",
    "title: Dynamic Plots in R Markdown\n",
    "output: html_document\n",
    "---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = TRUE)\n",
    "plot_list <- readRDS('report_objects.rds')\n",
    "```\n\n",
    "## Plots\n\n"
  )

  print(rmd_content)

  return(report_objects_for_export)
}

generate_collection_single_variable <- function(dataset, variable_of_interest, correlation_var=NA,
                                                stratify_var, stratify_flag, labels_flag=F, legend_flag=F,
                                                legend_position = "right", destination="slide", palette = "Set1"){

  variable_of_interest_type <- class(dataset[[variable_of_interest]])
  return_list <- list(plots=list(), summary_table=NULL)


  if(is.na(correlation_var)){
    ## single variable plots and table

    # single variable summary table
    summary_table <- generate_summary_table_variable(dataset = dataset,
                                                     variable_of_interest = variable_of_interest,
                                                     # stratify_flag = stratify_flag,
                                                     # stratify_var = stratify_var,
                                                     destination = destination)

    if (variable_of_interest_type %in% c("numeric","integer")) {
      ## numeric summary objects

      qq_temp <- generate_qq_plot(dataset = dataset,
                                  variable_of_interest = variable_of_interest,
                                  stratify_flag = stratify_flag,
                                  stratify_var = stratify_var,
                                  destination = destination)

      histogram_temp <- generate_histogram_plot(dataset = dataset,
                                                variable_of_interest = variable_of_interest,
                                                stratify_flag = stratify_flag,
                                                stratify_var = stratify_var,
                                                destination = destination)

      return_list$plots <- list(qq_plot = qq_temp, histogram_plot = histogram_temp)
      return_list$summary_table <- summary_table

    } else if (variable_of_interest_type %in% c("factor","logical")) {
      ## factor summary objects
      # generate_plot will create a bar plot based on logic
      barplot_temp <- generate_plot(dataset = dataset,
                                    indep_var = variable_of_interest,
                                    stratify_var = stratify_var,
                                    stratify_flag = stratify_flag,
                                    labels_flag = labels_flag,
                                    legend_flag = legend_flag,
                                    destination = destination,
                                    palette = palette)

      return_list$plots <- list(bar_plot = barplot_temp)

      # figure out how to create a 3 column grid with 2 cols for the plot and 1 col for the table
      return_list$summary_table <- summary_table

    }
  } else if(!is.na(correlation_var)){
    correlation_plot_temp <- generate_plot(dataset = dataset,
                                           indep_var = correlation_var,
                                           dep_var = variable_of_interest,
                                           stratify_var = stratify_var,
                                           stratify_flag = stratify_flag,
                                           labels_flag = labels_flag,
                                           legend_flag = legend_flag,
                                           legend_position = legend_position,
                                           destination = destination,
                                           palette = palette)

    return_list$plots <- list(correlation_plot = correlation_plot_temp)

  }

  return_list[["variable_of_interest"]] <- variable_of_interest
  return_list[["variable_of_interest_type"]] <- variable_of_interest_type
  return_list[["correlation_var"]] <- correlation_var
  return_list[["stratify_flag"]] <- stratify_flag
  return_list[["stratify_var"]] <- stratify_var

  return(return_list)

}

generate_object_grid_screen <- function(collection_objects_output, variable_of_interest, variable_of_interest_type, result_type){
  returngrid <- NA

  if(result_type == "variable summary"){
    if(variable_of_interest_type %in% c("numeric","integer")){

      # retrieve summary objects
      qq_plot <- collection_objects_output$plots[["qq_plot"]]
      histogram_plot <- collection_objects_output$plots[["histogram_plot"]]
      summary_table <- collection_objects_output$summary_table %>% gen_grob()

      left_column <- grid.arrange(qq_plot, histogram_plot, ncol=1)
      returngrid <- grid.arrange(left_column, summary_table, ncol=2)

      #talk about gtable here to put a table in a plot object
    }
    else if (variable_of_interest_type %in% c("factor","logical")){

      bar_plot <- collection_objects_output$plots[["bar_plot"]]
      # returnval <- objects_output$plots[["bar_plot"]]
      returngrid <- grid.arrange(bar_plot, ncol = 1)
      # do i want to add the same variable summary for factors as i do in the slide summary? i think that kind of makes sense...

    }
  }
  else if(result_type == "correlation"){
    correlation_plot <- collection_objects_output$plots[["correlation_plot"]]
    returngrid <- grid.arrange(correlation_plot, ncol = 1)
  }

  return(returngrid)
}

#### END Report generating functions
