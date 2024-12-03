#### Server definition ####

server_edark <- function(input, output, session){

  #### Header section ####

  #### General code setup ####

  #### Initialize variables ####
  dataset_original <- edark_dataset_input
  dataset_loaded <- reactiveVal(dataset_original)
  dataset_filtered <- reactiveVal(dataset_loaded)

  #to store summary table for main table
  summary_table_all_variables <- reactiveVal(NA)

  # cohort filtering parameters
  feature_filter <- reactiveVal(vector(mode="list", length=0))
  feature_filter_modified_flag <- reactiveVal(F)

  # from server
  #TOAD db connection
  db_connection <- NULL

  # marking initial run for initialize_dataset in observe()
  first_run <- reactiveVal(TRUE)

  # vectors to hold column names by data type for use in UI updating
  dataset_columns_factor <- NA
  dataset_columns_numeric <- NA
  dataset_columns_posix <- NA
  dataset_columns_factor_numeric <- NA

  #### END header section

  #plotting reactives that get values from UI elements
  ylim_scale <- reactive({input$slider_ylim_scale})
  xlim_scale <- reactive({input$slider_xlim_scale})

  #output objects
  data_explorer_plot <- reactiveVal(NULL)
  data_explorer_table <- reactiveVal(NULL)
  data_explorer_reactable <- reactiveVal(NULL)

  # on initialization
  observe({
    if(first_run() == T){
      initialize_dataset(dataset_loaded())
      # shinyjs::runjs("document.documentElement.requestFullscreen();")
      # shinyjs::runjs("document.documentElement.msRequestFullscreen();")
      # shinyjs::runjs("document.documentElement.mozRequestFullScreen();")
      # shinyjs::runjs("document.documentElement.webkitRequestFullscreen();")
      first_run(FALSE)
    }
  })

  #### Data processing functions ####

  # after query or loading new dataset, make numeric columns numbers, and remaining non-numeric columns factors
  # this is the dataset before cohort filtering etc, so that we can read a "clean" interpretable dataset

  initialize_dataset <- function(df){

    # set reactive vars to dataset
    # first one autocasts columns based on type
    dataset_loaded(df_column_datatype_autocast(df))
    dataset_filtered(dataset_loaded())
    print("data loaded into dataset_loaded")

    # clear current dynamic feature filtering/modification data structure
    feature_filter(vector(mode="list", length=0))

    # update cohort selection tab with new data features
    updateSelectInput(session,inputId="select_feature_filter_modify", choices=NULL)
    updateSelectInput(session,inputId="select_feature_filter_add", choices=names(dataset_loaded()))

    # udpate UI with column information based on type
    update_eda_variables_ui()

    # create and store summary table, display summary table
    update_dataset_descriptive_tables()
  }

  update_eda_variables_ui <- function(){
    # update correlation feature lists
    dataset_columns_factor <- dataset_filtered() %>% select_if(~ is.factor(.) || is.logical(.)) %>% names
    dataset_columns_numeric <- dataset_filtered() %>% select_if(~ is.numeric(.) && !is.POSIXct(.)) %>% names
    dataset_columns_posix <- dataset_filtered() %>% select_if(~ is.POSIXct(.)) %>% names
    dataset_columns_factor_numeric <- dataset_filtered() %>% select_if(~ is.factor(.) || is.logical(.) || (is.numeric(.) && !is.POSIXct(.))) %>% names

    dataset_columns_factor <- dataset_columns_factor[!dataset_columns_factor %in% c("stratify_by","trend_by")]
    dataset_columns_numeric <- dataset_columns_numeric[!dataset_columns_numeric %in% c("stratify_by","trend_by")]
    dataset_columns_posix <- dataset_columns_posix[!dataset_columns_posix %in% c("stratify_by","trend_by")]
    dataset_columns_factor_numeric <- dataset_columns_factor_numeric[!dataset_columns_factor_numeric %in% c("stratify_by","trend_by")]

    updateSelectInput(session, inputId="select_feature_a",
                      choices=dataset_columns_factor_numeric,
                      selected=dataset_columns_factor_numeric[1])
    updateSelectInput(session, inputId="select_feature_b",
                      choices=dataset_columns_factor_numeric,
                      selected=dataset_columns_factor_numeric[2])

    updateSelectInput(session, inputId="select_stratify",
                      choices=dataset_columns_factor,
                      selected=dataset_columns_factor[1])

    updateSelectInput(session, inputId="select_feature_trend",
                      choices=dataset_columns_posix,
                      selected=dataset_columns_posix[1])
  }

  update_dataset_descriptive_tables <- function(){
    summary_table_all_variables(
      generate_summary_table_dataset(
        dataset_filtered()
        )
      )

    tbl <- reactable(data = summary_table_all_variables(),
                     filterable = TRUE,
                     searchable = TRUE,
                     pagination = FALSE,
                     striped = TRUE,
                     highlight = TRUE,
                     showSortable = TRUE,
                     selection = "single"
    )

    data_explorer_reactable(tbl)
  }

  # update feature filter data structure with UI elements
  update_feature_filter <- function(){
    feature_type <- class(dataset_loaded()[[input$select_feature_filter_modify]])
    if(feature_type %in% c("numeric","integer")){
      feature_filter[[input$select_feature_filter_modify]]$feature_range <- input$slider_feature_filter_range
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <- unlist(strsplit(input$text_feature_filter_factor_cut_points,","))
      feature_filter[[input$select_feature_filter_modify]]$feature_factor <- input$radio_feature_filter_factor_type
      feature_filter[[input$select_feature_filter_modify]]$feature_factor <- input$radio_feature_filter_factor_type
    }
    if(feature_type %in% c("factor","logical"))
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <- input$select_feature_filter_factor_levels
  }

  # load feature filter data structure into UI
  update_feature_filter_ui <- function(){
  }

  # Filter cohort based on UI selections
  # this includes feature modification and cohort filtering
  # as well as dynamic outlier factoring for correlations tab

  filter_dataset <- function(){

    feature_b_type <- class(dataset_loaded()[[input$select_feature_b]])
    feature_a_type <- class(dataset_loaded()[[input$select_feature_a]])

    returnval <- dataset_loaded()

    # if(feature_a_type %in% c("numeric","integer") && input$checkbox_outliers_feature_a==TRUE){
    #   outlier_limit <- quantile(dataset_loaded()[[input$select_feature_a]],input$numinput_outliers_feature_a, na.rm=TRUE)
    #   returnval %<>% filter(!!as.symbol(input$select_feature_a) <= outlier_limit)
    # }
    # if(feature_b_type %in% c("numeric","integer") && input$checkbox_outliers_feature_b==TRUE){
    #   outlier_limit <- quantile(dataset_loaded()[[input$select_feature_b]],input$numinput_outliers_feature_b, na.rm=TRUE)
    #   returnval %<>% filter(!!as.symbol(input$select_feature_b) <= outlier_limit)
    # }

    if(length(feature_filter)>0){
      for(feature in seq(1:length(feature_filter))){
        feature_name <- names(feature_filter[feature])
        feature_type <- class(dataset_loaded()[[feature_name]])

        if(feature_type %in% c("numeric","integer")){
          if(!is.null(feature_filter[[feature]]$feature_range))
            returnval %<>% filter(!!as.symbol(feature_name) >= feature_filter[[feature]]$feature_range[1],
                                  !!as.symbol(feature_name) <= feature_filter[[feature]]$feature_range[2])

          # as is factoring
          if(feature_filter[[feature]]$feature_factor==2){
            returnval %<>% mutate(!!as.symbol(feature_name) := as.factor(!!as.symbol(feature_name))) %>% filter(!!as.symbol(feature_name) %in% feature_filter[[feature]]$feature_factor_selected_levels)
          }
          #cut point factoring
          else if(feature_filter[[feature]]$feature_factor==3 && check_numeric(feature_filter[[feature]]$feature_factor_cut_points)){
            returnval %<>% mutate(!!as.symbol(feature_name) := cut(!!as.symbol(feature_name),c(-Inf,feature_filter[[feature]]$feature_factor_cut_points,Inf)))
          }
        }
        else if(feature_type %in% c("factor","logical")){
          returnval %<>% filter(!!as.symbol(feature_name) %in% feature_filter[[feature]]$feature_factor_selected_levels)
        }
      }
    }

    ## consider moving this below the dynamic filtering code block below. need to think about this

    dataset_filtered(returnval)

    update_eda_variables_ui()
  }

  #### Dataset import/export events ####

  # query button clicked, rerun query fetch
  observeEvent(input$button_query,ignoreInit = T, {
    query_result <- NA
    con_valid_flag <- F

    tryCatch({
      if(DBI::dbIsValid(db_connection)){
        con_valid_flag <- T
      }
    }, error = function(e){
      showNotification(paste0("Error: ",e,"\nDB connection is invalid. Please enter credentials."))
    })

    if(con_valid_flag == F){
      tryCatch({
        db_connection <- create_db_connection_toad()
      }, error = function(e){
        showNotification(paste0("Error: ",e,"\nDB connection is invalid. Please try again."))
        return()
      })
    }

    if(is.null(db_connection)){
      showNotification(paste0("DB connection is invalid. Please try again."))
      return()
    }
    else {
      con_valid_flag <- T
      showNotification("DB connection successful.")
    }

    if(con_valid_flag == T){
      tryCatch({
        showNotification("Fetching query.")
        query_result <- dbGetQuery(db_connection, input$textarea_query)
      }, error = function(e) {
        showNotification(paste0("Error: ",e,"\nQuery fetch failure."))
        return()
      })

      tryCatch({
        dataset_loaded(initialize_dataset(query_result))
        showNotification("Dataset loaded successfully.")
      }, error = function(e){
        showNotification(paste0("Error: ",e,"\nDataset load failure."))
        return()
      })
    }

  })

  # observeEvent(input$button_load_dataset,{
  #   # filters <- list(
  #   #   "R data files" = list("rds"),
  #   #   "CSV" = list("csv"),
  #   #   "Excel" = list("xls, xlsx"),
  #   #   "All files" = list("*")
  #   # )
  #
  #   tryCatch({
  #     filename <- rstudioapi::selectFile(caption = "Load dataset from file",
  #                                        filter = "Data files (*.rds | *.csv | *.xls | *.xlsx)",
  #                                        existing = TRUE)
  #     if(is.null(filename)){
  #       showNotification("Loading dataset canceled by user.")
  #       return()
  #     }
  #     if(!file.exists(filename)){
  #       showNotification("File does not exist.")
  #       return()
  #     }
  #     if(length(filename)>0 && file.exists(filename)){
  #       switch(tools::file_ext(filename),
  #              "rds" = {df_from_file <- readRDS(filename)},
  #              "csv" = {
  #                date_formats <- col_datetime(format = c("ymd", "mdy", "dmy"))
  #                df_from_file <- read_csv(filename, col_types=date_formats)
  #              },
  #              "xls" = ,
  #              "xlsx" = {df_from_file <- read_excel(filename)}
  #       )
  #       initialize_dataset(df_from_file)
  #       showNotification("Loaded file successfully!")
  #     } else {
  #       showNotification("Unknown error loading dataset.")
  #     }
  #   }, error = function(e) {
  #
  #     showNotification("Loading dataset failed.")
  #     showNotification(paste0("Error Message: ", conditionMessage(e)))
  #   }, finally =  {
  #
  #   })
  # })

  observeEvent(input$button_save_dataset,ignoreInit = T, {
    datafile <- NA
    try(filename <- rstudioapi::selectFile(caption = "Save dataset to file",
                                           filter = "RDS Files (*.rds)",
                                           existing = FALSE)
    )
    if(!is.na(filename)){
      saveRDS(dataset_loaded(), file=filename)
      showNotification("Saved file successfully!")
    }
    else(showNotification("Saving dataset canceled"))

  })

  #### Cohort filtering/feature modification UI events ####

  observeEvent(input$button_feature_filter_add, ignoreInit = T, {
    feature_filter[[input$select_feature_filter_add]] <- list(initialized=FALSE, feature_factor=NA,
                                                               feature_range=NA, feature_factor_all_levels=NA, feature_factor_selected_levels=NA, feature_factor_cut_points=NA)
    shinyjs::enable("select_feature_filter_modify")
    updateSelectInput(session,inputId="select_feature_filter_modify", choices=names(feature_filter), selected=input$select_feature_filter_add)
    updateSelectInput(session,inputId="select_feature_filter_add", choices=names(dataset_loaded())[!(names(dataset_loaded()) %in% names(feature_filter))])

  })

  observeEvent(input$button_feature_filter_remove, ignoreInit = T, {
    # print(paste0("feature filter length ",length(feature_filter)))
    # print(feature_filter)
    feature_filter[[input$select_feature_filter_modify]] <- NULL
    updateSelectInput(session,inputId="select_feature_filter_add", choices=names(dataset_loaded())[!(names(dataset_loaded()) %in% names(feature_filter))])
    if(length(feature_filter)>0)
      updateSelectInput(session,inputId="select_feature_filter_modify", choices=names(feature_filter), selected=names(feature_filter)[1])
    else
      updateSelectInput(session,inputId="select_feature_filter_modify", choices=NA)

  })

  observeEvent(input$select_feature_filter_modify, ignoreInit=TRUE, {

    feature_type <- class(dataset_loaded()[[input$select_feature_filter_modify]])
    showNotification("select modify running")
    if(length(feature_filter)==0){
      # set UI values to empty/nulls
      updateSliderInput(session, inputId="slider_feature_filter_range", value=NA, min=NA, max=NA)
      updateRadioButtons(session, inputId="radio_feature_filter_factor_type", selected=NA)
      updateTextInput(session, inputId="text_feature_filter_factor_cutpoints", value="")
      updateSelectInput(session, inputId="select_feature_filter_factor_levels", choices=NA, selected=NA)
      # disable all UI elements, including modify select box
      shinyjs::disable("select_feature_filter_modify")
      shinyjs::disable("slider_feature_filter_range")
      shinyjs::disable("radio_feature_filter_factor_type")
      shinyjs::disable("text_feature_filter_factor_cut_points")
      shinyjs::disable("select_feature_filter_factor_levels")
    }
    else #length(feature_filter) > 0
    {
      if(feature_filter[[input$select_feature_filter_modify]]$initialized==FALSE){
        feature_filter[[input$select_feature_filter_modify]]$initialized <- TRUE
        if(feature_type %in% c("integer","numeric")){
          #set slider range and range element in list
          feature_filter[[input$select_feature_filter_modify]]$feature_range <- c(min(dataset_loaded()[[input$select_feature_filter_modify]], na.rm=TRUE),
                                                                                   max(dataset_loaded()[[input$select_feature_filter_modify]], na.rm=TRUE))
          # init factoring to no
          feature_filter[[input$select_feature_filter_modify]]$feature_factor <- 1
          feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <- NA
          feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <- NA
          shinyjs::enable("select_feature_filter_modify")
          shinyjs::enable("slider_feature_filter_range")
          shinyjs::enable("radio_feature_filter_factor_type")
          shinyjs::enable("text_feature_filter_factor_cut_points")
          shinyjs::enable("select_feature_filter_factor_levels")
          showNotification("setting select factor levels")
          updateSelectInput(session, inputId="select_feature_filter_factor_levels", choices=NA,
                            selected=NA)
          updateTextInput(session, inputId="text_feature_filter_factor_cut_points", value="")
          updateRadioButtons(session, inputId="radio_feature_filter_factor_type", selected=1)
          updateSliderInput(session, inputId="slider_feature_filter_range", value=feature_filter[[input$select_feature_filter_modify]]$feature_range,
                            min=feature_filter[[input$select_feature_filter_modify]]$feature_range[1],
                            max=feature_filter[[input$select_feature_filter_modify]]$feature_range[2])
          shinyjs::disable("text_feature_filter_factor_cut_points")
          shinyjs::disable("select_feature_filter_factor_levels")
        }
        else if(feature_type %in% c("factor","logical")){
          # set select factor levels box multiple with all levels, all selected
          feature_filter[[input$select_feature_filter_modify]]$feature_range <- c(NA,NA)
          feature_filter[[input$select_feature_filter_modify]]$feature_factor <- 1
          feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <- levels(dataset_loaded()[[input$select_feature_filter_modify]])
          feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <- levels(dataset_loaded()[[input$select_feature_filter_modify]])
          shinyjs::enable("select_feature_filter_modify")
          shinyjs::disable("slider_feature_filter_range")
          shinyjs::disable("radio_feature_filter_factor_type")
          shinyjs::disable("text_feature_filter_factor_cut_points")
          shinyjs::enable("select_feature_filter_factor_levels")
          updateSelectInput(session, inputId="select_feature_filter_factor_levels", choices=feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels,
                            selected=feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels)
        }
      }
      # else, we are already initialized and can just read and set based on variable type
      else {
        if(feature_type %in% c("numeric","integer")){

          # set range slider visiblity and values
          shinyjs::enable("slider_feature_filter_range")
          updateSliderInput(session, inputId="slider_feature_filter_range", value=feature_filter[[input$select_feature_filter_modify]]$feature_range,
                            min=min(dataset_loaded()[[input$select_feature_filter_modify]], na.rm=TRUE),
                            max=max(dataset_loaded()[[input$select_feature_filter_modify]], na.rm=TRUE))

          # set visibility and value of factor radio buttons, cutoff textbox, and factor level select
          shinyjs::enable("radio_feature_filter_factor_type")
          updateRadioButtons(session, inputId="radio_feature_filter_factor_type",
                             selected=feature_filter[[input$select_feature_filter_modify]]$feature_factor)

          shinyjs::toggleState("text_feature_filter_factor_cut_points", condition=(feature_filter[[input$select_feature_filter_modify]]$feature_factor==3))
          shinyjs::toggleState("select_feature_filter_factor_levels", condition=(feature_filter[[input$select_feature_filter_modify]]$feature_factor>1))
          # populate the textbox with the properly formatted string
          if(length(feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points)==0)
            updateTextInput(session, inputId="text_feature_filter_factor_cut_points", value="")
          else if(length(feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points)==1)
            updateTextInput(session, inputId="text_feature_filter_factor_cut_points",
                            value=feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points)
          else if(length(feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points)>1)
            updateTextInput(session, inputId="text_feature_filter_factor_cut_points",
                            value=paste0(feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points,sep=","))

          # either as-is factoring or cut point factoring -- get values from structure as they are populated only if validated
          if(feature_filter[[input$select_feature_filter_modify]]$feature_factor>1){
            updateSelectInput(session, inputId="select_feature_filter_factor_levels",
                              choices=feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels,
                              selected=feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels)
          } else{
            updateSelectInput(session, inputId="select_feature_filter_factor_levels",
                              choices=NA)
          }

        }

        else if(feature_type %in% c("factor","logical")){
          # set visibility and value of multiple select for factors
          shinyjs::disable("slider_feature_filter_range")
          updateSliderInput(session, inputId="slider_feature_filter_range", min=NA, max=NA, value=NA)
          updateRadioButtons(session, inputId="radio_feature_filter_factor_type",
                             selected=2)
          shinyjs::disable("radio_feature_filter_factor_type")
          shinyjs::disable("text_feature_filter_factor_cut_points")
          shinyjs::enable("select_feature_filter_factor_levels")
          updateSelectInput(session, inputId="select_feature_filter_factor_levels",
                            choices=feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels,
                            selected=feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels)
        }
      }
    }
  })

  observeEvent(input$slider_feature_filter_range, ignoreInit=TRUE,{
    feature_filter[[input$select_feature_filter_modify]]$feature_range <- input$slider_feature_filter_range
  })

  observeEvent(input$radio_feature_filter_factor_type, ignoreInit=TRUE, {
    feature_filter[[input$select_feature_filter_modify]]$feature_factor <- input$radio_feature_filter_factor_type
    if(input$radio_feature_filter_factor_type==1){
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <- NA
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <- NA
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <- NA
      shinyjs::enable("text_feature_filter_cut_points")
      shinyjs::enable("select_feature_filter_factor_levels")
      updateTextInput(session, inputId="text_feature_filter_factor_cutpoints", value="")
      updateSelectInput(session, inputId="select_feature_filter_factor_levels", choices=NA, selected=NA)
      shinyjs::disable("text_feature_filter_factor_cut_points")
      shinyjs::disable("select_feature_filter_factor_levels")
    }
    else if(input$radio_feature_filter_factor_type==2){
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <- {dataset_loaded()[[input$select_feature_filter_modify]] %>% as.factor %>% levels}
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <- {dataset_loaded()[[input$select_feature_filter_modify]] %>% as.factor %>% levels}
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <- NA
      shinyjs::disable("text_feature_filter_cut_points")
      shinyjs::enable("select_feature_filter_factor_levels")
      updateTextInput(session, inputId="text_feature_filter_factor_cut_points", value="")
      updateSelectInput(session, inputId="select_feature_filter_factor_levels", choices=feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels,
                        selected=feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels)
    }
    else if(input$radio_feature_filter_factor_type==3){
      shinyjs::enable("text_feature_filter_factor_cut_points")
      shinyjs::enable("select_feature_filter_factor_levels")
      updateTextInput(session, inputId="text_feature_filter_factor_cut_points", value=feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points)
    }
  })

  observeEvent(input$text_feature_filter_factor_cut_points, ignoreInit=TRUE,{
    # if cut string is all numbers separated by commas, save it; otherwise, dont
    if(feature_filter[[input$select_feature_filter_modify]]$feature_factor==3){
      valid_cut_point_string <- str_detect(input$text_feature_filter_factor_cut_points, "^(\\d*\\.?\\d*)(,(\\d*\\.?\\d*))*$")

      if(valid_cut_point_string)
        feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <- unlist(strsplit(input$text_feature_filter_factor_cut_points,","))
      else
        feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <- NA

      print("saving cut point vector and default factor and selected factor levels")
      feature_cut_levels <- dataset_loaded()[[input$select_feature_filter_modify]] %>% cut(breaks=c(-Inf,feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points,Inf)) %>% levels()
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <- feature_cut_levels
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <- feature_cut_levels
      updateSelectInput(session, inputId="select_feature_filter_factor_levels", choices=feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels,
                        selected=feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels)
    }
  })

  observeEvent(input$select_feature_filter_factor_levels, ignoreInit=TRUE, {
    print("saving selected feature levels")
    feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <- input$select_feature_filter_factor_levels
  })

  # mark dataset for filtering moving between tabs if anything is changed
  # this will allow for updating of the selectInputs for variable selection based on new variable types
  observeEvent({input$button_feature_filter_add
    input$button_feature_filter_remove
    input$slider_feature_filter_range
    input$radio_feature_filter_factor_type
    input$text_feature_filter_factor_cut_points
    input$select_feature_filter_factor_levels}, ignoreInit = TRUE, {
      feature_filter_modified_flag(T)
    })

  # if dataset has been modified, filter dataset again, which also updates UI and variable selectInputs
  observeEvent(input$tabsetpanel_data_explorer_sidebar, {

    if(feature_filter_modified_flag() == T && input$tabsetpanel_data_explorer_sidebar == "Explore data"){
      filter_dataset()
      feature_filter_modified_flag(F)
    } else if (input$tabsetpanel_data_explorer_sidebar == "Cohort selection"){
      updateTabsetPanel(session, inputId="tabsetpanel_data_explorer_mainpanel", selected="Dataset")

    }
  })

  observeEvent(input$button_update_dataset, {
    if(feature_filter_modified_flag() == T){
      filter_dataset()
      update_dataset_descriptive_tables()
      feature_filter_modified_flag(F)
    }
  })


  # also update reactable for dataset info

  #### Data visualization UI Events ####

  observeEvent(input$checkbox_correlate, {
    if(input$checkbox_correlate){
      shinyjs::enable("select_feature_b")
      shinyjs::enable("button_plot_correlation")
    }
    else{
      shinyjs::disable("select_feature_b")
      shinyjs::disable("button_plot_correlation")
    }
  })

  observeEvent(input$checkbox_stratify, {

    if(input$checkbox_stratify)
    {shinyjs::enable("select_stratify")}
    else{
      shinyjs::disable("select_stratify")
    }
  })

  observeEvent(input$checkbox_outliers_feature_a, {
    shinyjs::disable("numinput_outliers_feature_a")
    if(input$checkbox_outliers_feature_a==TRUE) shinyjs::enable("numinput_outliers_feature_a")
  })

  observeEvent(input$checkbox_outliers_feature_b, {
    shinyjs::disable("numinput_outliers_feature_b")
    if(input$checkbox_outliers_feature_b==TRUE) shinyjs::enable("numinput_outliers_feature_b")
  })

  observeEvent(input$select_feature_a, {
    updateSliderInput(session, inputId="slider_xlim_scale", value=100)
    updateSliderInput(session, inputId="slider_ylim_scale", value=100)
    # Clear and disable factoring inputs so their states and values are not maintained and disabled, but used for the next plot click
    updateCheckboxInput(session,inputId="checkbox_factor_feature_a",value=FALSE)
    shinyjs::disable("checkbox_factor_feature_a")
    updateTextInput(session, inputId="textinput_factor_feature_a",value="")
    shinyjs::disable("textinput_factor_feature_a")
    # If the variable is of a proper type, enable these features again
    if(class(dataset_loaded()[[input$select_feature_a]]) %in% c("numeric","integer")) {
      shinyjs::enable("checkbox_factor_feature_a")
      # shinyjs::enable("textinput_factor_feature_a")
    }
  })

  observeEvent(input$select_feature_b, {
    updateSliderInput(session, inputId="slider_xlim_scale", value=100)
    updateSliderInput(session, inputId="slider_ylim_scale", value=100)
    # Clear and disable factoring inputs so their states and values are not maintained and disabled, but used for the next plot click
    updateCheckboxInput(session,inputId="checkbox_factor_feature_b",value=FALSE)
    shinyjs::disable("checkbox_factor_feature_b")
    updateTextInput(session, inputId="textinput_factor_feature_b",value="")
    shinyjs::disable("textinput_factor_feature_b")
    # If the variable is of a proper type, enable these features again
    if(class(dataset_loaded()[[input$select_feature_b]]) %in% c("numeric","integer")) {
      shinyjs::enable("checkbox_factor_feature_b")
      # shinyjs::enable("textinput_factor_feature_b")
    }
  })

  observeEvent(input$checkbox_factor_feature_a, {
    shinyjs::disable("textinput_factor_feature_a")
    if(input$checkbox_factor_feature_a == T)
      shinyjs::enable("textinput_factor_feature_a")
  })

  observeEvent(input$checkbox_factor_feature_b, {
    shinyjs::disable("textinput_factor_feature_b")
    if(input$checkbox_factor_feature_b == T)
      shinyjs::enable("textinput_factor_feature_b")
  })

  # Data visualization events ####

  # Button event: Summarize Feature ####

  observeEvent(input$button_summarize_feature_a, ignoreInit = T, {

    updateTabsetPanel(session, inputId="tabsetpanel_data_explorer_mainpanel", selected="Plot")
    shinyjs::show("loading-overlay") # Show the loading overlay

    tryCatch({

      # establish stratify variables
      stratify_flag <- input$checkbox_stratify
      stratify_var <- input$select_stratify

      # use stratify variables to stratify dataset with parameters
      dataset_filtered(stratify_dataset(dataset = dataset_filtered(), stratify_flag, stratify_var))
      dataset_loaded_local <- dataset_filtered()

      # use loaded dataset for other things
      variable_of_interest <- input$select_feature_a
      variable_of_interest_type <- class(dataset_loaded_local[[variable_of_interest]])
      stratify_type <- class(dataset_loaded_local[[input$select_stratify]])
      labels_flag <- input$checkbox_show_labels
      legend_flag <- input$checkbox_show_legend

      # the below is present in the correlation button code, but NOT necessary for the variable summary, because we want this to show up for categoricals
      #   filter out any records that have NA in the variables of interest, and droplevels() to make sure if any factor levels are not present, they dont get graphed
      # dataset_loaded_local <- dataset_filtered %>% filter(!is.na(!!as.symbol(input$select_feature_a)),!is.na(!!as.symbol(input$select_feature_b))) %>%
      #   droplevels()


      if (stratify_flag == TRUE && variable_of_interest == stratify_var) {
        showModal(modalDialog(
          title = "Error",
          icon = shiny::icon("remove"),
          "Error: variable of interest and stratify variable must be distinct.",
          easyClose = TRUE,
          footer = NULL
        ))

        shinyjs::show("loading-overlay") # Show the loading overlay
        return()
      }

      updateTabsetPanel(session, inputId="tabsetpanel_data_explorer_mainpanel", selected="Plot")
      showNotification(paste0("Summarize: feature type: ", variable_of_interest_type))

      variable_summary_objects <- generate_collection_single_variable(dataset = dataset_loaded_local,
                                                                      variable_of_interest = variable_of_interest,
                                                                      stratify_var = stratify_var,
                                                                      stratify_flag = stratify_flag,
                                                                      labels_flag = labels_flag,
                                                                      legend_flag = legend_flag,
                                                                      destination = "screen",
                                                                      palette = input$select_fill_palette)

      returnval <- generate_object_grid_screen(variable_summary_objects, variable_of_interest, variable_of_interest_type, result_type = "variable summary")

      data_explorer_plot(returnval)
    }, error = function(e) {
      showNotification(paste0("Error in summarize feature: ",e))
    })

    shinyjs::hide("loading-overlay") # Show the loading overlay
  }) ## END Summarize Feature Button

  # Button event: Plot Correlation ####
  observeEvent(input$button_plot_correlation, ignoreInit = T, {

    # Method:
    # - filter dataset per global criteria, modifications, etc
    # - filter dataset locally for NAs and dropping levels
    # - create temp dataset of variables of interest? this would be for speed of processing
    # - modify dataset locally for categorical split (this also doesnt seem to be functioning as well as it could)
    # - choose pathway based on variable types and stratification (faceting) -- each 2 x 2 x 2 should be its own if/else condition, for ease of reading simplicity
    #   - create facet column (let's make this a string based on the cutoffs that we have if we are creating a categorical from a numeric)
    #   - create graph x and y axis maximums
    #   - generate plot label data
    #   - generate plot
    # - for all cases, generate graph titles/axis titles (this is already done in a neat block below)

    updateTabsetPanel(session, inputId="tabsetpanel_data_explorer_mainpanel", selected="Plot")
    shinyjs::show("loading-overlay") # Show the loading overlay

    tryCatch({
      updateTabsetPanel(session, inputId="tabsetpanel_data_explorer_mainpanel", selected="Plot")

      # updateSelectInput(session, inputId="select_feature_b", selected="case_year")

      # setup stratify variables
      stratify_var <- input$select_stratify
      stratify_flag <- input$checkbox_stratify

      # stratify datset
      dataset_filtered(stratify_dataset(dataset = dataset_filtered(), stratify_flag, stratify_var))

      # filter out any records that have NA in the variables of interest, and droplevels() to make sure if any factor levels are not present, they dont get graphed
      # not present in variable summary because we WANT NAs there
      dataset_loaded_local <- dataset_filtered() %>% filter(!is.na(!!as.symbol(input$select_feature_a)),!is.na(!!as.symbol(input$select_feature_b))) %>%
        droplevels()

      variable_of_interest <- input$select_feature_a
      correlate_flag <- T
      correlate_var <- input$select_feature_b
      stratify_type <- class(dataset_loaded_local[[input$select_stratify]])
      labels_flag <- input$checkbox_show_labels
      legend_flag <- input$checkbox_show_legend
      legend_position <- input$select_legend_position


      # error check for distinct variables in variable of interest, correlation, and stratification variables if selected
      if (
        (correlate_flag == TRUE && variable_of_interest == correlate_var) ||
        (stratify_flag == TRUE && variable_of_interest == stratify_var) ||
        (correlate_flag == TRUE && stratify_flag == TRUE & correlate_var == stratify_var))
      {
        showModal(modalDialog(
          title = "Error",
          icon = shiny::icon("remove"),
          "Error: variable of interest, correlation variable, and stratify variable must be distinct.",
          easyClose = TRUE,
          footer = NULL
        ))

        return(NULL)
      }

      return_collection <- generate_collection_single_variable(dataset = dataset_loaded_local,
                                                               variable_of_interest = variable_of_interest,
                                                               correlation_var = correlate_var,
                                                               stratify_var = stratify_var,
                                                               stratify_flag = stratify_flag,
                                                               labels_flag = labels_flag,
                                                               legend_flag = legend_flag,
                                                               legend_position = legend_position,
                                                               destination = "screen",
                                                               palette = input$select_fill_palette)

      return_grid <- generate_object_grid_screen(return_collection, variable_of_interest, variable_of_interest_type, result_type = "correlation")

      # code I had used or scaling the plots
      # ylim_max <- {
      #   a <- 1.1
      #   if(class(dataset_loaded_local[[input$select_feature_a]]) %in% c("numeric","integer")){ #b is independent, a is dependent
      #     a <- max(dataset_loaded_local[[input$select_feature_a]], na.rm=TRUE) * ylim_scale() / 100
      #   }
      #
      #   a
      # }
      #
      # xlim_max <- {
      #   a <- NA
      #   if(class(dataset_loaded_local[[input$select_feature_b]]) %in% c("numeric","integer")){ #b is independent, a is dependent
      #     a <- max(dataset_loaded_local[[input$select_feature_b]], na.rm=TRUE) * xlim_scale() / 100
      #   }
      #   a
      # }

      ########### END REDONE CODE BLOCK
      # return_grid <- grid.arrange(return_plot, ncol=1)
      data_explorer_plot(return_grid)
    }, error = function(e) {
      showNotification(paste0("Error in summarize feature: ",e))
    })

    shinyjs::hide("loading-overlay") # Show the loading overlay
  }) # END observeEvent button_plot_correlation


  # Button event: Trend ####

  observeEvent(input$button_plot_trend,{

    updateTabsetPanel(session, inputId="tabsetpanel_data_explorer_mainpanel", selected="Plot")

    shinyjs::show("loading-overlay") # Show the loading overlay

    tryCatch({

      # set stratify variables
      stratify_var <- input$select_stratify
      stratify_flag <- input$checkbox_stratify

      # stratify dataset
      dataset_filtered(stratify_dataset(dataset = dataset_filtered(), stratify_flag, stratify_var))

      # filter out any records that have NA in the variables of interest, and droplevels() to make sure if any factor levels are not present, they dont get graphed
      dataset_loaded_local <- dataset_filtered() %>% filter(!is.na(!!as.symbol(input$select_feature_a)),!is.na(!!as.symbol(input$select_feature_trend))) %>%
        droplevels()

      variable_of_interest <- input$select_feature_a
      # unused after removing correlate checkbox, and unused in this function anyway
      # correlate_flag <- input$checkbox_correlate
      correlate_var <- input$select_feature_b
      stratify_type <- class(dataset_loaded_local[[input$select_stratify]])
      labels_flag <- input$checkbox_show_labels
      legend_flag <- input$checkbox_show_legend
      legend_position <- input$select_legend_position

      if (stratify_flag == TRUE && variable_of_interest == stratify_var) {
        showModal(modalDialog(
          title = "Error",
          icon = shiny::icon("remove"),
          "Error: variable of interest and stratify variable must be distinct.",
          easyClose = TRUE,
          footer = NULL
        ))

        return(NULL)
      }

      trend_var <- input$select_feature_trend
      trend_resolution <- input$select_feature_trend_resolution

      dataset_loaded_local %<>%
        mutate(trend_by = case_when(
          trend_resolution == "Minute" ~ format(get(trend_var), "%Y-%m-%d %H:%M") %>% as.POSIXct(),
          trend_resolution == "Hour" ~ format(get(trend_var), "%Y-%m-%d %H:00") %>% as.POSIXct(),
          trend_resolution == "Day" ~ format(get(trend_var), "%Y-%m-%d") %>% as.POSIXct(),
          trend_resolution == "Month" ~ format(get(trend_var), "%Y-%m-01") %>% as.POSIXct(),
          trend_resolution == "Quarter" ~ paste0(year(get(trend_var)),"-",sprintf("%02d",(month(get(trend_var))+2) %/% 3 * 3 - 2),"-01") %>% as.POSIXct(),
          trend_resolution == "Year" ~ format(get(trend_var), "%Y-01-01") %>% as.POSIXct(),
          TRUE ~ NA
        )
        )

      return_plot <- generate_plot(dataset = dataset_loaded_local,
                                   indep_var = "trend_by",
                                   dep_var = input$select_feature_a,
                                   stratify_var = input$select_stratify,
                                   stratify_flag = input$checkbox_stratify,
                                   labels_flag = input$checkbox_show_labels,
                                   legend_flag = legend_flag,
                                   legend_position = legend_position,
                                   destination = "screen",
                                   palette = input$select_fill_palette,
                                   trend_resolution = trend_resolution)

      data_explorer_plot(return_plot)
    }, error = function(e) {
      showNotification(paste0("Error in summarize feature: ",e))
    })

    shinyjs::hide("loading-overlay") # Show the loading overlay
  })

  #### Report generation events ####

  # Button event: Table One ####

  observeEvent(input$button_generate_table_one, ignoreInit = T, {

    # set stratify variables
    stratify_var <- input$select_stratify
    stratify_flag <- input$checkbox_stratify

    # stratify dataset
    dataset_filtered(stratify_dataset(dataset = dataset_filtered(), stratify_flag, stratify_var))

    updateTabsetPanel(session, inputId="tabsetpanel_data_explorer_mainpanel", selected="gt Table")

    returnval <- NA

    if(stratify_flag == TRUE){
      returnval <- dataset_filtered() %>% tbl_summary(by = !!as.symbol(stratify_var),
                                                    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} / {N} ({p}%)")) %>%
        modify_header(label = paste0("**",stratify_var,"**")) %>% add_p()
    }
    else {
      returnval <- dataset_filtered() %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                     all_categorical() ~ "{n} / {N} ({p}%)"))
    }

    print(returnval)
    returnval %<>% as_gt()

    data_explorer_table(returnval)
  })

  # Button event: Generate report ####

  observeEvent(input$button_generate_report,{

    # set stratify variables
    stratify_var <- input$select_stratify
    stratify_flag <- input$checkbox_stratify

    # stratify dataset
    dataset_filtered(stratify_dataset(dataset = dataset_filtered(), stratify_flag, stratify_var))

    # remove posixct and non-numeric and non-factor and non-logical variables from dataset
    dataset_report <- dataset_filtered() %>% select_if(~ (is.numeric(.) || is.factor(.) || is.logical(.)) && !is.POSIXct(.))
    variable_of_interest <- input$select_feature_a
    variable_of_interest_role <- NA
    labels_flag <- input$checkbox_show_labels
    legend_flag <- input$checkbox_show_legend
    legend_position <- input$select_legend_position
    report_format <- input$select_report_format

    correlate_flag = FALSE
    if(input$select_report_analysis_type == "VoI as exposure") {
      correlate_flag = TRUE
      variable_of_interest_role <- "exposure"
    }
    if(input$select_report_analysis_type == "VoI as outcome") {
      correlate_flag = TRUE
      variable_of_interest_role <- "outcome"
    }

    report_object_to_write <- list(object_for_output = NULL, report_format = NULL)

    report_object_to_write <- generate_report(dataset = dataset_report,
                                               variable_of_interest = variable_of_interest,
                                               variable_of_interest_role = variable_of_interest_role,
                                               correlate_flag = correlate_flag,
                                               stratify_flag = stratify_flag,
                                               stratify_var = stratify_var,
                                               labels_flag = labels_flag,
                                               legend_flag = legend_flag,
                                               report_format = report_format)

    file_ext <- case_when(
      report_format == "Powerpoint" ~ "pptx",
      report_format == "Word document" ~ "docx",
      report_format == "PDF" ~ "pdf",
      report_format == "HTML Web Page" ~ "html"
    )

    report_filename <- paste0("report-", Sys.Date(), ".", file_ext)

    write_report_to_file(report_object_to_write, report_filename)
    showNotification("Saved report successfully!")

  })


  write_report_to_file <- function(report_object_to_write, report_filename){

    report_format <- report_object_to_write[["report_format"]]
    object_for_output <- report_object_to_write[["object_for_output"]]

    if(report_format == "Powerpoint") {
      print(object_for_output, target=report_filename)
      showNotification("Saved report successfully!")
    }
    # Word document
    else if (report_format == "Word document"){
      showNotification("Microsoft Word report not implemented yet!")
    }
    # PDF
    else if (report_format == "PDF"){
      showNotification("PDF report not implemented yet!")
    }
    # HTML web page
    else if (report_format == "HTML Web Page"){
      showNotification("HTML report not implemented yet!")
    }
  }

  # output$button_download_report <- shiny::downloadHandler(
  #   filename = function() {report_filename},
  #   content = function(report_filename_temp)
  #   {
  #     write_report_to_file(report_object_to_write, report_filename_temp)
  #   }
  # )

  #### END UI Events

  #### Output rendering ####

  output$data_explorer_output_table <- renderPlot({
    data_explorer_plot()
  })

  output$data_explorer_output_plot <- renderPlot({
    grid.draw(data_explorer_plot())
  })

  output$data_explorer_output_gt <- render_gt({
    data_explorer_table()
  })

  output$data_explorer_output_reactable <- renderReactable({
    data_explorer_reactable()
  })

  output$table_debug <- renderTable({
    data_explorer_table()
  })
  #
  # output$text_debug <- renderText({names(dataset_loaded())
  # }

} #### END server main

#### END Server code ###
