# header comments

# To-do list

# Level 
  # modify generate_report_rmarkdown to output each individual plot from the "plots" list, and the summary table, to all different slides
  # create function generate_report_object_collection that calls generate_collection_single_variable to create all components for the report and store in object, including 
    # dataset_summary_table, missingness_plot/table, variable_output_objects, correlation_plot
    # ideally, this should then be passed to any report generating function
    # add missingness graph with naniar package
    # will need to modify generate_report_powerpoint to output the odd things that arent plots, unless we just want to 
      # omit them, because of the slide limitations
    # will also need to modify generate_report_rmarkdown to include the new things
  # figure out precision for numbers when displaying labels
  # convert histogram to "distribution" plot with these
          # ggplot(iris, aes(Sepal.Length, stat(density))) +
          #   geom_histogram(bins = 20, fill = "white", color = "black")  +
          #   geom_density() +
          #   geom_vline(aes(xintercept = mean(Sepal.Length)), linetype = 2)  
# Level 2
  # complete download logic - hidden download button, use action button to simulate download button click
    # https://community.rstudio.com/t/simulating-a-click-when-using-shinys-downloadhandler/15413/3
  # integrate RMD script, add some page breaks, titles, formatting, etc; modify for the new report_data structure
    # use this code to try and pass the rmd_content string as the input, and modify it to take a parameter, which will
      # be the report_objects variable so that I don't have to write and read it in again in the RMD text
          # rmarkdown::render(
          #   "report.Rmd", output_file = file, params = params
          # )
        # for rendering to PDF without installing latex
          # render_rmd_to_pdf <- function(input_file, output_file) {
          #   pagedown::chrome_print(input_file, output_file)
          # }
# Level 3
  # change the way the feature_filter object works; maybe it should contain one item for each variable so that I can
    # more quickly and easily select, display current situation, and remove the "add" and "modify" levels of ux interaction
# Level 5
    # consider plotly output for HTML and in shiny app?
  # different plot types https://www.datanovia.com/en/blog/ggplot-examples-best-reference/ 
    #  factor-factor ggballoonplot(housetasks, fill = "value")
  # when dynamically filtering, only offer factor "as is" if number of levels and factor only if < 10?
    # can potentially use the dataset variable summary stats on this one!!!
  # consider hrbrthemes for plots
  # add "copy this plot as slide" button, or at the least, 
    # output it to a bmp or png or jpg using the output functions (e.g. png() and dev.off() and render it again)
  # standardize table presentation on slides; fix scaling on the single variable summary

########### generate_report_rmarkdown



# 2024-04-17
#   - updated correlation and variable summary buttons to rely on same function generate_collection_single_variable
#   - fixed legend not showing on factor x factor bar plot, and scatter line plot
#   - fixed legend issue for trend plots
#   - changed trend line aesthetics
# 2024-04-16 - added ability in reporting function to specify variable of interest as exposure or outcome variable, (x or y axis respectively on output graphs)
# 2023-11-05 - started adding rmarkdown
# 2023-11-04 - fixed downloading report with downloadHandler; started dynamic RMD implementation
# 2023-10-18 to 23
#   - implemented trending; cleaned up code for consistency
# 2023-10-17
#   - UI improvements
#   - imported single variable summary slide creation 
#   - created single variable PPT report presentation r
#   - exported plot stylng to separate function so that I can style all plots the same and only modify code in one place
# 2023-10-15
#   - moved all plot generation to functions so that I can create presentations or markdowns quickly and easily
#   - imported but did not test powerpoint create slide function
# 2023-10-14
#   - variable lists everywhere now are linked to each other and do not show a selected variable in a related selectInput
#   - 
# 2021-06-01
#   - changed entire UI updating sequence from centralized function to trickling down between observeEvents
# 2021-05-13
#   - added radio button and removed checkbox for factoring of non-factor variables in cohort filter tab (now as is and cut point factoring available)
#   - tidied up cohort filtering
#   - fixed changing select inputs on file/query load
#   - error checking for load/save file or invalid connection to DB
#   - added descriptive stats graphs -- barplot for factors, density for continuous
# 2021-05-09
#   - added tab for dynamic cohort filtering -- continuous variable range and categorical filtering implemented
#   - started on gtsummary for dataset in descriptive stats tab
# 2021-04-03
#   - removed a LOT of redundant code
#   - confirmed year trending works - can leave variable as numeric or factor; this resulted in simplified logic for factor a/b typing
#   - reduced complexity of title and axis labelling
# 2021-02-10
#   - Labs: added time filter selectInputs, only draws lines/boxes on graph that are within time window, added reperfusion line, added some extra labs (BE, lactate, sodium, etc)
#   - integrated Kidney DGF explorer
# 2021-02-08 added query box, but doesn
# 2021-02-09
#   - dynamic query box works, updates UI variable selctions
#   - branched off to new R project - TOAD explorer
#   - changed UI to navbarPage to integrate DGF and labs explorer
#   - LQI explorer works
#   - Liver labs explorer works't work yet, need to make variable pointers internal (inside server function)
# 2021-02-07 rewrote radio_mode == 2 (correlation) code, including labels, legend, faceting, x and y scaling, tested all conditions

# END header comments

require(pacman)
# data manipulation
pacman::p_load(tidyverse, magrittr, magrittr, readxl, data.table)
# plotting and analysis
pacman::p_load(ggplot2, scales, grid, gridExtra, ggpubr, ggtext, corrplot, hrbrthemes)
# statistics
pacman::p_load(e1071) # kurtosis, skewness
# tables
pacman::p_load(gt, gtsummary, flextable)
# ui
pacman::p_load(shiny, shinyjs, shinyFiles, htmltools, shinyWidgets, shinythemes, shinyBS, reactable)
# TOAD integration
pacman::p_load(DBI, RPostgres)
# Powerpoint presentation generation
pacman::p_load(officer, rvg)

# SQL Query definitions ####

# this is here to be preloaded into texarea, but query doesn't need to run always
toad_liver_query <- "SELECT 
	lqi.lqi_id
	,lqi.case_date
	,EXTRACT(YEAR FROM lqi.case_date) AS case_year
	,meld.creatinine AS preop_cr
	,meld.inr AS preop_inr
	,meld.bilirubin AS preop_bili
	,meld.meld
	,meld.meldna
	,liver.f_lab_at_timepoint(lqi.lqi_id, 'preop','hemoglobin')::numeric AS preop_hb
	,lqi.ffp::numeric as ffp
	,lqi.prbcs::numeric as prbcs
	,lqi.platelets::numeric as platelets
	,CASE WHEN lqi.living_related_flag = 't' THEN TRUE ELSE FALSE END AS living_related_flag
	,CASE WHEN lqi.diagnosis_hcc = 't' THEN TRUE ELSE FALSE END AS diagnosis_hcc
	,star.prev_ab_surg_trr
	,star.tipss_trr 
	,aki.postop_aki_stage_composite
	,CASE WHEN aki.postop_aki_stage_composite >= 2 THEN TRUE ELSE FALSE END AS postop_aki_23_flag
FROM liver.mv_case_liverqi lqi
LEFT JOIN LATERAL liver.f_meld_by_lqi_id(lqi.lqi_id) meld ON TRUE
LEFT JOIN LATERAL liver.f_postop_aki_by_lqi_id(lqi.lqi_id, 48,96) aki ON true
INNER JOIN liver.mv_case_star star ON star.lqi_id = lqi.lqi_id
WHERE lqi.case_date > '2012-06-01'
	AND COALESCE(lqi.aborted_flag,'f') <> 't'
	AND lqi.aborted_flag <> 't'
	AND lqi.patient_age_years::int >= 18"

#### END SQL query definitions

#### Header section ####

#### General code setup ####

#### Initialize variables ####

onStart <- function() {

  # dataset_loaded_global <- readRDS(paste0(getwd(),"/Datasets/full_dataset_sample_lr.rds"))
  # dataset_loaded <<- dataset_loaded_global #readRDS(paste0(getwd(),"/Datasets/full_dataset_sample_lr.rds"))
  
  # filename <- paste0(getwd(),"/Datasets/lqi_data_clarity_2020-12-18.csv")
  # df_test <- read_csv(filename, na = "NULL")
  # df_test %<>%
  #   mutate(num_rnd1 = round(runif(nrow(df_test),1,10),1),
  #          logical_rnd1 = as.logical(round(runif(nrow(df_test), 0, 1), 0)),
  #          col_factor1 = as.factor(sample(c("red", "green", "blue"), nrow(df_test), replace = TRUE)),
  #          col_factor2 = as.factor(sample(c("dog", "cat", "elephant"), nrow(df_test), replace = TRUE)),
  #   )
  dataset_loaded <<- df_test
  dataset_original <<- dataset_loaded
  # default dataset at load
  # str(dataset_loaded)
  # dataset_loaded %<>% select(patient_age_years, meldna, meld_dialysis, vaso_used, postop_aki_stage_by_cr, postop_aki_stage_composite)

  # cohort filtering parameters
  feature_filter <<- vector(mode="list", length=0)
  feature_filter_modified <<- F
  
  # plotting constants
  theme_screen <<- ggplot2::theme_light(base_size=28)
  theme_slide <<- ggplot2::theme_light(base_size=20)
  label_size <<- 10    
  
  # powerpoint generation constants
  loc_title <<- ph_location_type(type = "title")
  loc_footer <<- ph_location_type(type = "ftr")
  loc_dt <<- ph_location_type(type = "dt")
  loc_slidenum <<- ph_location_type(type = "sldNum")
  loc_body <<- ph_location_type(type = "body")
  loc_body_left <<- ph_location_left()
  loc_body_right <<- ph_location_right()
  loc_ctrtitle <<- ph_location_type(type="ctrTitle")
  loc_subtitle <<- ph_location_type(type="subTitle")
  loc_fullsize <<- ph_location_fullsize(width = 0.8, height=0.8,)
  
  toad_liver_query <<- 
    "SELECT 
    	lqi.lqi_id
    	,lqi.case_date
    	,EXTRACT(YEAR FROM lqi.case_date) AS case_year
    	,meld.creatinine AS preop_cr
    	,meld.inr AS preop_inr
    	,meld.bilirubin AS preop_bili
    	,meld.meld
    	,meld.meldna
    	,liver.f_lab_at_timepoint(lqi.lqi_id, 'preop','hemoglobin')::numeric AS preop_hb
    	,lqi.ffp::numeric as ffp
    	,lqi.prbcs::numeric as prbcs
    	,lqi.platelets::numeric as platelets
    	,CASE WHEN lqi.living_related_flag = 't' THEN TRUE ELSE FALSE END AS living_related_flag
    	,CASE WHEN lqi.diagnosis_hcc = 't' THEN TRUE ELSE FALSE END AS diagnosis_hcc
    	,star.prev_ab_surg_trr
    	,star.tipss_trr 
    	,aki.postop_aki_stage_composite
    	,CASE WHEN aki.postop_aki_stage_composite >= 2 THEN TRUE ELSE FALSE END AS postop_aki_23_flag
    FROM liver.mv_case_liverqi lqi
    LEFT JOIN LATERAL liver.f_meld_by_lqi_id(lqi.lqi_id) meld ON TRUE
    LEFT JOIN LATERAL liver.f_postop_aki_by_lqi_id(lqi.lqi_id, 48,96) aki ON true
    INNER JOIN liver.mv_case_star star ON star.lqi_id = lqi.lqi_id
    WHERE lqi.case_date > '2012-06-01'
    	AND COALESCE(lqi.aborted_flag,'f') <> 't'
    	AND lqi.aborted_flag <> 't'
    	AND lqi.patient_age_years::int >= 18"
}

#### END header section

#### UI Definition ####

ui <- 
  navbarPage("EDARK", selected=1,
    theme = shinytheme("flatly"),
    useShinyjs(),
    # css to center the progress bar
    tags$head(
      tags$style(
        HTML("
             .shiny-notification {
               height: 100px;
               width: 800px;
               position:fixed;
               top: calc(50% - 50px);
               left: calc(50% - 400px);
               font-size: 250%;
               text-align: center;
             }
             .nav-tabs {
                border: 1px solid #ddd; /* Add border to the tabset */
              }
             .tab-content {
                border: 1px solid #ddd; /* Add border to the tab content */
                padding: 10px; /* Add some padding for better appearance */
              }
             "
        )
      )
    ),
    tabPanel(title="Exploratory Data Analysis", value=1,
      sidebarLayout(
        sidebarPanel(width=4,
           div(
             style = "height: 80vh; overflow-y: scroll;",
            fluidRow(
               column(width=12,
                  tabsetPanel(id="tabsetpanel_data_explorer_sidebar", type="pills",selected="Explore data",
                     # cohort selection
                     tabPanel(title="Import/Export Dataset",
                        br(),
                        h4("Load/save file"),
                        fluidRow(
                          column(width=5, actionButton(inputId="button_load_dataset", label="Load dataset from file", width="100%", style="height:40px")),
                          # shinyFilesButton("file", "Load dataset", title = "Please select files", multiple = FALSE, filetypes = c("R dataset" = "rds", "CSV" = "csv", "All Files" = "*")),
                          column(width=2),
                          column(width=5, actionButton(inputId="button_save_dataset", label="Save dataset to file", width="100%", style="height:40px"))
                        ),
                        br(),
                        h4("Load from TOAD using SQL query"),
                        textAreaInput(inputId="textarea_query", label="Enter query here", 
                                      value=toad_liver_query, height=400, placeholder="Enter query here", resize="both"),
                        actionButton(inputId="button_query", label="Run query")
                     ),
                     tabPanel(title="Cohort selection",
                        br(),
                        # select filter feature and add
                        fluidRow(
                          column(selectInput(inputId="select_feature_filter_add",label="Select feature to filter", choices=NULL), width=8),
                          column(actionButton(inputId="button_feature_filter_add", label="Add feature", style="height:40px"),width=3)
                        ),
                        # modify filter settings/parameters
                        fluidRow(
                          disabled(column(selectInput(inputId="select_feature_filter_modify",label="Select feature to modify",choices=NULL), width=8)),
                          column(actionButton(inputId="button_feature_filter_remove", label="Remove filter", style="height:40px"),width=3)
                        ),
                        disabled(sliderInput(inputId="slider_feature_filter_range",label="Range",min=0, max=10, step=1, value=c(1,10))),
                        disabled(radioButtons(inputId="radio_feature_filter_factor_type", label="Factoring/Categorical conversion", choiceNames=list("None","As is","Cut point list"), choiceValues=list(1,2,3), selected=1)),
                        disabled(textInput(inputId="text_feature_filter_factor_cut_points", label="Cutpoints for factoring, comma separated", value="")),
                        # disabled(pickerInput(inputId="select_feature_filter_factor_levels", label="Levels to include", choices=NA, selected=NA,
                        #             multiple=TRUE,options=list(`actions-box`=TRUE))),
                        disabled(selectInput(inputId="select_feature_filter_factor_levels", label="Levels to include", choices=NULL,
                                             multiple=TRUE)),
                        br(),
                        br(),
                        fluidRow(
                          column(actionButton(inputId="button_update_dataset", label="Update dataset", style="height:60px"),width=4)
                        )
                     ),
                     # inputs to create visualizations
                     tabPanel(title="Explore data",
                        br(),
                        fluidRow(
                          column(
                            # variable of interest ui elements
                            fluidRow(
                              column(h5("Variable"), width=4),
                              column(selectInput(inputId="select_feature_a", label=NULL, choices=NULL, selected=NULL), width=8)
                              ),
                            # stratification ui elements
                            fluidRow(
                              column(checkboxInput(inputId="checkbox_stratify", label="Stratify by", value=FALSE), width=4),
                              column(selectInput(inputId="select_stratify", label=NULL, choices=NULL, selected=NULL),width=8)
                            ),
                            width = 8),
                          column(actionButton(inputId="button_summarize_feature_a", label="Describe", width="90%"), width=4, align="center")   
                          ),
                          # column(checkboxInput(inputId="checkbox_outliers_feature_a", label="Filter outliers?", value=FALSE), width=3), 
                          # column(numericInput(inputId="numinput_outliers_feature_a", label="Quantile", min=0, max=1, step=0.01, value=0.99), width=3)
                        # h5("Correlate with"),
                          # column(checkboxInput(inputId="checkbox_outliers_feature_b", label="Filter outliers?", value=FALSE), width=3),
                          # column(numericInput(inputId="numinput_outliers_feature_b", label="Quantile", min=0, max=1, step=0.01, value=0.99), width=3)
                        # fluidRow(
                        #   column(checkboxInput(inputId="checkbox_factor_feature_b", label="Categorical", value=FALSE), width=3),
                        #   column(textInput(inputId="textinput_factor_feature_b", label="Cutoff points list, comma separated", value=""), width=9)
                        # ),
                        # h5("Stratify by"),
                        # correlation ui elements
                        fluidRow(
                          column(
                            fluidRow(
                              column(h5("Correlate with"), width=4),
                                # checkboxInput(inputId="checkbox_correlate", label="Correlate", value=FALSE), width=4),
                              column(selectInput(inputId="select_feature_b", label=NULL, choices=NULL, selected=NULL), width=8)
                              ),
                            width = 8),
                          column(
                            actionButton(inputId="button_plot_correlation",label="Plot correlation", width="90%"),
                            width = 4, align="center"
                            )
                          ),
                          # column(textInput(inputId="textinput_facet_list", label=NULL),width=1)
                        # fluidRow(
                        #   column(checkboxInput(inputId="checkbox_factor_feature_a", label="Categorical", value=FALSE), width=3),
                        #   column(textInput(inputId="textinput_factor_feature_a", label="Cutoff points list, comma separated", value=""), width=9)
                        # trend ui elements
                        fluidRow(
                          column(
                            fluidRow(
                              column(h5("Trend by"), width=4),
                              column(selectInput(inputId="select_feature_trend",label=NULL, choices = NA), width=8)
                              ),
                            fluidRow(
                              column(h5("Resolution"), width=4),
                              column(selectInput(inputId="select_feature_trend_resolution", label = NULL,
                                                        choices = c("Minute","Hour","Day","Month","Quarter","Year")), width=8)
                                     ),
                            width = 8
                          ),
                          column(
                            actionButton(inputId="button_plot_trend",label="Plot trend", width="90%"),
                            width = 4, align="center"
                          )
                        ),
                        # collapsible panels to hide settings
                        bsCollapse(id = "collapse_plot_options",
                           bsCollapsePanel("Reporting",
                                           h6("Note: report will be stratified as configured above"),
                                           column(
                                             fluidRow(
                                               column(h5("Report on"), width = 4),
                                               column(selectInput(inputId="select_report_analysis_type", label=NULL, 
                                                                  choices=c("All variables","VoI as exposure","VoI as outcome"), 
                                                                  selected=1), width=8)
                                             ),
                                             fluidRow(
                                               column(h5("Report destination"), width = 4),
                                               column(selectInput(inputId="select_report_format", label=NULL, 
                                                                  choices=c("Word document","PDF","Powerpoint", "HTML Web Page"), selected="HTML Web Page"), width=8) 
                                             ),
                                             width = 8
                                             ),
                                           column(
                                             fluidRow(actionButton(inputId="button_generate_report", label="Create report", width="90%")),
                                             fluidRow(actionButton(inputId="button_generate_table_one", label="Create table one", width="90%")
                                             ),
                                             width=4
                                             ),
                                             # column(downloadButton(outputId="button_download_report", label="Download report", width="90%", style="height:50px; visibility: hidden;")
                                             #        , width=6)
                                           style = "success"),
                           bsCollapsePanel("Aesthetics",
                                br(),
                                selectInput(inputId="select_fill_palette",label="Fill color palette", choices = 
                                              c("Blues","Reds","Oranges","Purples","Set1","Set2","Set3","Pastel1","Pastel2"), selected="Set1"),
                                checkboxInput(inputId="checkbox_show_labels", label="Show labels?", value=FALSE),
                                fluidRow(
                                  column(checkboxInput(inputId="checkbox_show_legend", label="Legend position", value=FALSE), width=4),
                                  column(selectInput(inputId="select_legend_position", label=NULL,
                                                     choices=c("top","right","bottom"), selected="right"), width=9)
                                  ),
                                sliderInput(inputId="slider_xlim_scale", label="X axis scale (%ile)", min=0, max=200, value=100),
                                sliderInput(inputId="slider_ylim_scale", label="Y axis scale (%ile)", min=0, max=200, value=100),
                                style = "info")
                           # inputs to adjust plot aesthetics
                        )
                     )
                  ) # end tabsetpanel
                ) #end column
              ) #end fluidrow
           ) # end div
        ), #end sidebarpanel
        #output
        mainPanel(width=8,
          div(
            id = "loading-overlay",
            style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(0, 0, 0, 0.7); display: none;",
            div(
              style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
              div(class = "loader")
            )
          ),
          # CSS to style the loader
          tags$style(HTML("
                      .loader {
                        border: 4px solid #f3f3f3;
                        border-top: 4px solid #3498db;
                        border-radius: 25%;
                        width: 40px;
                        height: 40px;
                        animation: spin 2s linear infinite;
                      }

                      @keyframes spin {
                        0% { transform: rotate(0deg); }
                        100% { transform: rotate(360deg); }
                      }
                      ")),
          div(
            style = "height: 83vh; overflow-y: scroll; overflow-x: scroll;",
            tabsetPanel(type = "tabs", id="tabsetpanel_data_explorer_mainpanel",
              tabPanel(title="Dataset", reactableOutput("data_explorer_output_reactable")),   
              tabPanel(title="Table", plotOutput("data_explorer_output_table")),
              tabPanel(title="Plot", plotOutput("data_explorer_output_plot", height="800px"), ),  
              tabPanel(title="gt Table", gt_output("data_explorer_output_gt"))
              # tabPanel(title="Debug", verbatimTextOutput("text_debug"), tableOutput("table_debug"))
            )
          ) # end div
        ) #end mainpanel
      ) # end sidebarlayout
    ) # end data explorer tabpanel
  ) #end  navbarlayout

#### END UI Definition

#### Server definition ####

server <- function(input, output, session){
  
  #plotting reactives that get values from UI elements
  ylim_scale <- reactive({input$slider_ylim_scale})
  xlim_scale <- reactive({input$slider_xlim_scale})
  
  #output objects
  data_explorer_plot <- reactiveVal(NULL)
  data_explorer_table <- reactiveVal(NULL) 
  data_explorer_reactable <- reactiveVal(NULL)
  
  #TOAD db connection
  db_connection <<- NULL
  
  # marking initial run for process_dataset in observe()
  first_run <<- T
  
  # on initialization
  observe({
    if(first_run == T){
      process_dataset(dataset_loaded)
      # shinyjs::runjs("document.documentElement.requestFullscreen();")
      # shinyjs::runjs("document.documentElement.msRequestFullscreen();")
      # shinyjs::runjs("document.documentElement.mozRequestFullScreen();")
      # shinyjs::runjs("document.documentElement.webkitRequestFullscreen();")
      first_run <<- F
    }
  })
  
  #### General function defintions ####
  
  create_db_connection_toad <- function(){
    user_prompt <- rstudioapi::showPrompt(title="TOAD username", message="Enter username", default="")
    if(is.null(user_prompt)) {
        showNotification("TOAD connection cancelled", type="warning")
        return(NULL)
    }
    tryCatch({
        con <- DBI::dbConnect(RPostgres::Postgres()
                          , host='QCTOADLDB001.som.ucsf.edu'
                          , port='5432'
                          , dbname='toad'
                          , user=user_prompt
                          , password=rstudioapi::askForPassword("TOAD password")
                          # , timezone = Sys.timezone()
                          )
        return(con)
    }, error = function(e){
      showNotification("TOAD connection failed", type="error")
      return(NULL)
    })
  }

  # check if a vector needs to be converted to a numeric type
  # numbers, logicals, dates all make it to numbers when casted
  # type CHAR that contains all numbers will return false
  
  check_numeric <- function(x) (
    if(class(x) %in% c("int","num")){
      T
    }
    else if (class(x) %in% c("factor", "logical")) F
    else {
      sum(is.na(as.numeric(x))) == sum(is.na(x))
    }
  )
  
  # Define a mode function as it's not built-in in R
  mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # initialize some global variables for general use
  max_levels_for_factor <<- 20
  dataset_columns_numeric <<- NA
  dataset_columns_factor <<- NA
  dataset_columns_posix <<- NA
  
  #### Data processing functions ####
  
  # after query or loading new dataset, make numeric columns numbers, and remaining non-numeric columns factors
  # this is the dataset before cohort filtering etc, so that we can read a "clean" interpretable dataset
  
  process_dataset <- function(df){
    # df %<>% mutate_if(check_numeric, as.numeric) %>% mutate_if(is.character, as.factor)
    dataset_original <<- df
    
    dataset_loaded <<- dataset_original %>%
      mutate(across(everything(), ~ {
        if (is.POSIXct(.)) {
          if (any(hour(.) != 0, minute(.) != 0, second(.) != 0)) {
            .  # Keep it as POSIXct with a timestamp
          } else {
            # Add a midnight timestamp (00:00:00) to POSIXct with only a date component
            . <- ymd_hms(format(., "%Y-%m-%d 00:00:00"))
          }
        } else if (is.Date(.)) {
          . <- ymd_hms(format(., "%Y-%m-%d 00:00:00"))  # Convert Date to POSIXct with a midnight timestamp
        } else if (is.logical(.)) {
          as.factor(.)
        } else if (is.factor(.)) {
          .
        } else if (is.numeric(.) || is.integer(.)) {
          .
        } else if (is.character(.)) {
          # Check if the entire column can be cast to a numeric
          if (all(suppressWarnings(!is.na(as.numeric(.))))) {
            . <- as.numeric(.)  # Cast to numeric
          } else if (length(unique(.)) < 20) {
            . <- as.factor(.)  # Cast to factor
          } else { . }
        }
        .  # Leave it as is if none of the conditions match
      }))
    
    
    print("data loaded into dataset_loaded")
    
    # clear current dynamic feature filtering/modification data structure
    feature_filter <<- vector(mode="list", length=0)
    
    # update cohort selection tab with new data features
    updateSelectInput(session,inputId="select_feature_filter_modify", choices=NULL)
    updateSelectInput(session,inputId="select_feature_filter_add", choices=names(dataset_loaded))
    
    dataset_filtered <<- dataset_loaded
    
    update_eda_variables_ui()
    update_dataset_descriptive_tables()
  }
  
  update_eda_variables_ui <- function(){
    # update correlation feature lists
    dataset_columns_factor <<- dataset_filtered %>% select_if(~ is.factor(.) || is.logical(.)) %>% names
    dataset_columns_numeric <<- dataset_filtered %>% select_if(~ is.numeric(.) && !is.POSIXct(.)) %>% names
    dataset_columns_posix <<- dataset_filtered %>% select_if(~ is.POSIXct(.)) %>% names
    dataset_columns_factor_numeric <<- dataset_filtered %>% select_if(~ is.factor(.) || is.logical(.) || (is.numeric(.) && !is.POSIXct(.))) %>% names
    
    dataset_columns_factor <<- dataset_columns_factor[!dataset_columns_factor %in% c("stratify_by","trend_by")]
    dataset_columns_numeric <<- dataset_columns_numeric[!dataset_columns_numeric %in% c("stratify_by","trend_by")]
    dataset_columns_posix <<- dataset_columns_posix[!dataset_columns_posix %in% c("stratify_by","trend_by")]
    dataset_columns_factor_numeric <<- dataset_columns_factor_numeric[!dataset_columns_factor_numeric %in% c("stratify_by","trend_by")]
    
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
    summary_table_all_variables <<- generate_summary_table_dataset(dataset_filtered)
    
    tbl <- reactable(data = summary_table_all_variables,
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
    feature_type <- class(dataset_loaded[[input$select_feature_filter_modify]])
    if(feature_type %in% c("numeric","integer")){
      feature_filter[[input$select_feature_filter_modify]]$feature_range <<- input$slider_feature_filter_range
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <<- unlist(strsplit(input$text_feature_filter_factor_cut_points,","))
      feature_filter[[input$select_feature_filter_modify]]$feature_factor <<- input$radio_feature_filter_factor_type
      feature_filter[[input$select_feature_filter_modify]]$feature_factor <<- input$radio_feature_filter_factor_type
    }
    if(feature_type %in% c("factor","logical"))
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <<- input$select_feature_filter_factor_levels
  }
  
  # load feature filter data structure into UI
  update_feature_filter_ui <- function(){
  }
  
  # Filter cohort based on UI selections
  # this includes feature modification and cohort filtering
  # as well as dynamic outlier factoring for correlations tab
  
  filter_dataset <- function(){
    
    feature_b_type <- class(dataset_loaded[[input$select_feature_b]])
    feature_a_type <- class(dataset_loaded[[input$select_feature_a]])
    
    returnval <- dataset_loaded
    
    # if(feature_a_type %in% c("numeric","integer") && input$checkbox_outliers_feature_a==TRUE){
    #   outlier_limit <- quantile(dataset_loaded[[input$select_feature_a]],input$numinput_outliers_feature_a, na.rm=TRUE)
    #   returnval %<>% filter(!!as.symbol(input$select_feature_a) <= outlier_limit)
    # }
    # if(feature_b_type %in% c("numeric","integer") && input$checkbox_outliers_feature_b==TRUE){
    #   outlier_limit <- quantile(dataset_loaded[[input$select_feature_b]],input$numinput_outliers_feature_b, na.rm=TRUE)
    #   returnval %<>% filter(!!as.symbol(input$select_feature_b) <= outlier_limit)
    # }
    
    if(length(feature_filter)>0){
      for(feature in seq(1:length(feature_filter))){
        feature_name <- names(feature_filter[feature])
        feature_type <- class(dataset_loaded[[feature_name]])
        
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
    
    dataset_filtered <<- returnval
    
    update_eda_variables_ui()
  }
  
  stratify_dataset <- function(){
    returnval <- dataset_filtered
    
    if(input$checkbox_stratify){
      if(class(returnval[[input$select_stratify]]) %in% c("factor","logical")){
        returnval %<>% 
          #filter(!is.na(!!as.symbol(input$select_stratify))) %>%  ################### check this line, dont throw them out so quickly
          mutate(stratify_by = paste0(str_to_title(gsub("_"," ",input$select_stratify))," = ",get(input$select_stratify)))
      }
      else{
        returnval %<>% mutate(stratify_by = "No stratification")
      }
    }
    else{
      returnval %<>% mutate(stratify_by = "No stratification")
    }
    
    returnval %<>% mutate(stratify_by = as.factor(stratify_by))
    
    dataset_filtered <<- returnval
  }

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
                                         correlate_flag = F, stratify_flag = F, stratify_var = NA, labels_flag = F, legend_flag = F){
    
    # load template and add introductory slide
    
    pptx <- read_pptx("../PPT templates/template_16x9_blank.pptx")
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
          
          collection_object_output <- generate_collection_single_variable(dataset = dataset, 
                                                                  variable_of_interest = variable_of_interest,
                                                                  stratify_flag = stratify_flag,
                                                                  stratify_var = stratify_var,
                                                                  labels_flag = labels_flag,
                                                                  legend_flag = legend_flag,
                                                                  destination = "slide")

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
            if(variable_of_interest_role == "exposure") {
              object_output <- generate_collection_single_variable(dataset = dataset, 
                                                                      variable_of_interest = correlation_variable,
                                                                      correlation_var = variable_of_interest,
                                                                      stratify_flag = stratify_flag,
                                                                      stratify_var = stratify_var,
                                                                      labels_flag = labels_flag,
                                                                      legend_flag = legend_flag,
                                                                      destination = "slide")
            }
            else if (variable_of_interest_role == "outcome"){
              object_output <- generate_collection_single_variable(dataset = dataset, 
                                                                      variable_of_interest = variable_of_interest,
                                                                      correlation_var = correlation_variable,
                                                                      stratify_flag = stratify_flag,
                                                                      stratify_var = stratify_var,
                                                                      labels_flag = labels_flag,
                                                                      legend_flag = legend_flag,
                                                                      destination = "slide")
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
    
    cmat <- dataset %>% select_if(check_numeric) %>% cor
    png("corrplot1.png")
    cmatp <- corrplot(cmat, method="color")
    dev.off()
    
    pptx <- add_slide(pptx, layout = "Title and Content", master="Office Theme")
    pptx <- ph_with(pptx, str_to_title("Correlation Matrix"), location=loc_title)
    pptx <- ph_with(pptx,external_img(src = "corrplot1.png"), location=loc_body)
    
    # Save the PowerPoint presentation
    
    return(pptx)
    
  }
  
  generate_slide_ppt_report <- function(pptx, collection_object_output) {
    
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
    
    # add stats table to new slide
    pptx <- add_slide(pptx, layout = "Title and Content", master="Office Theme")
    pptx <- ph_with(pptx, str_to_title(variable_of_interest), location=loc_title)
    pptx <- ph_with(pptx, summary_table, location=loc_body)
    
    # Return the modified pptx object
    return(pptx)
  }
  
  generate_report_rmarkdown <- function(dataset, variable_of_interest=NA, variable_of_interest_role = NA, correlate_flag = F, stratify_flag = F, stratify_var = NA, labels_flag = F, legend_flag = F, report_format){
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
                                                destination = "slide")
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
                                                destination = "slide")
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
                                      legend_position = "right", destination="slide"){
    
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
                                      destination = destination)
        
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
                    destination = destination)
      
      return_list$plots <- list(correlation_plot = correlation_plot_temp)
      
    }
    
    return_list[["variable_of_interest"]] <- variable_of_interest
    return_list[["variable_of_interest_type"]] <- variable_of_interest_type
    return_list[["correlation_var"]] <- correlation_var
    return_list[["stratify_flag"]] <- stratify_flag
    return_list[["stratify_var"]] <- stratify_var
    
    return(return_list)
  
  }
  
  generate_summary_table_variable <- function(dataset, variable_of_interest, indep_var=NA, stratify_var=NA, stratify_flag=NA, destination="screen"){
    
    variable_of_interest_type = class(dataset[[variable_of_interest]])
    
    if(variable_of_interest_type %in% c("numeric","integer")){
      # draw from  the summary table that has already been created for display on the first tab of dataset exploration
      # filter for that variable and transpose the row to a column
      summary_table <- summary_table_all_variables %>% filter(variable == variable_of_interest) %>% t %>% as.data.frame %>% 
        rownames_to_column(var="Element") %>% setNames(c("Element","Value")) %>% mutate(Element = str_to_title(gsub("_"," ",Element)))
      
      # Create a table with basic statistics
      summary_table <- flextable::qflextable(summary_table) %>%
        flextable::set_table_properties(width = .5, align = "center")
    }
    else if (variable_of_interest_type %in% c("factor","logical")){
      # draw from  the summary table that has already been created for display on the first tab of dataset exploration
      # filter for that variable and transpose the row to a column
      summary_table <- summary_table_all_variables %>% filter(variable == variable_of_interest) %>% 
        select(variable, type, missing_num, missing_pct, unique_values, mode) %>% 
        mutate(categories = cat(levels(dataset[[variable_of_interest]]), sep="\n")) %>% t %>% as.data.frame %>% 
        rownames_to_column(var="Element") %>% setNames(c("Element","Value")) %>% mutate(Element = str_to_title(gsub("_"," ",Element)))

      # Create a table with basic statistics
      summary_table <- flextable::qflextable(summary_table) %>%
        flextable::set_table_properties(width = .5, align = "center")
    }
    
    return(summary_table)
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
  
  #### Plot generating function definitions ####
  
  generate_summary_table_dataset <- function(dataset) {
    # Compute statistics for each column
    
    summary_df <- NULL
    
    for(column_name in names(dataset)) {
      column <- dataset[[column_name]]
      
      # Check if the column is numeric before calculating the statistics
      
      if(is.numeric(column)) {
        max_decimal_places <- max(sapply(column, function(x) {
          nchar(sub("^[^.]*\\.?", "", as.character(x)))
        }))
        # print(paste0("max decimal places for column '", column_name,"' = ", max_decimal_places))
        rounding_decimal_places <- max_decimal_places + 1
        format_string <- paste0("%.",rounding_decimal_places,"f")
        
        statistics_list <- list(
          variable = column_name,
          type = class(dataset[[column_name]]),
          missing_num = sum(is.na(dataset[[column_name]])),
          missing_pct = paste0(sprintf("%.f",round(sum(is.na(dataset[[column_name]]))/length(dataset[[column_name]]),2)*100),'%'),
          unique_values = length(unique(dataset[[column_name]])),
          mean = sprintf(format_string, mean(column, na.rm = TRUE)),
          median = sprintf(format_string, median(column, na.rm = TRUE)),
          mode = mode(column),
          variance = sprintf(format_string, var(column, na.rm = TRUE)),
          std_deviation = sprintf(format_string, sd(column, na.rm = TRUE)),
          range = diff(range(column, na.rm = TRUE)),
          min = min(column, na.rm = TRUE),
          max = max(column, na.rm = TRUE),
          iqr = IQR(column, na.rm = TRUE),
          skewness = sprintf("%.3f",skewness(column, na.rm = TRUE)),
          kurtosis = sprintf("%.3f",kurtosis(column, na.rm = TRUE) - 3)  # Excess kurtosis
        )
        # print(paste0("adding ", column_name," as numeric"))
      } else {
        statistics_list <- list(
          variable = column_name,
          type = class(dataset[[column_name]]),
          missing_num = sum(is.na(dataset[[column_name]])),
          missing_pct = paste0(sprintf("%.f",round(sum(is.na(dataset[[column_name]]))/length(dataset[[column_name]]),2)*100),'%'),
          unique_values = nlevels(dataset[[column_name]]),
          mean = NA,
          median = NA,
          mode = mode(column),
          variance = NA,
          std_deviation = NA,
          range = NA,
          min = NA,
          max = NA,
          iqr = NA,
          skewness = NA,
          kurtosis = NA
        )
        # print(paste0("adding ", column_name," as non-numeric"))
      }                        
      
      summary_df <- rbind(summary_df, statistics_list %>% as.data.frame)
    }
    
    return(summary_df)
  }
  
  generate_plot <- function(dataset, indep_var, dep_var=NA, stratify_var=NA, stratify_flag=F, 
                            labels_flag = F, legend_flag = F, legend_position = "right", destination="screen"){
    
    indep_var_type <- class(dataset[[indep_var]])[1]
    dep_var_type <- NA
    
    if(!is.na(dep_var)){
      
      dep_var_type <- class(dataset[[dep_var]])
      
      if(dep_var_type %in% c("factor","logical")){
        if(indep_var_type %in% c("factor","logical")){
          # generate bar plot, categorical vs categorical, counts
          return_plot <- generate_bar_plot(dataset = dataset,
                                          indep_var = indep_var,
                                          dep_var = dep_var,
                                          labels_flag = labels_flag,
                                          legend_flag = legend_flag)
        }
        else if(indep_var_type %in% c("numeric","integer")){
          # generate horizontal boxplot, numeric vs categorical
          # flip the variables so it plots right, but then flip the plot
          temp_var <- indep_var
          indep_var <- dep_var
          dep_var <- temp_var
          return_plot <- generate_boxplot(dataset = dataset,
                                         dep_var = dep_var,
                                         indep_var = indep_var,
                                         labels_flag = labels_flag,
                                         flip_axes_flag = T)
        } else if (indep_var_type == "POSIXct"){
          ## this is now a trend plot, make it a line graph
          return_plot <- generate_scatter_line_plot(dataset = dataset,
                                                   indep_var = indep_var,
                                                   dep_var = dep_var,
                                                   stratify_flag = stratify_flag,
                                                   labels_flag = labels_flag,
                                                   legend_flag = legend_flag,
                                                   destination = destination)
        }
      } else if (dep_var_type %in% c("numeric","integer")){
        if(indep_var_type %in% c("factor","logical")){
          # generate vertical boxplot, categorical vs numeric
          return_plot <- generate_boxplot(dataset = dataset,
                                         dep_var = dep_var,
                                         indep_var = indep_var,
                                         labels_flag = labels_flag,
                                         flip_axes_flag = F)
        } else if(indep_var_type %in% c("numeric","integer")){
          return_plot <- generate_scatter_plot(dataset = dataset,
                                              indep_var = indep_var,
                                              dep_var = dep_var)
        } else if (indep_var_type == "POSIXct"){
          ## this is now a trend plot, make it a line graph
          return_plot <- generate_scatter_line_plot(dataset = dataset,
                                                   indep_var = indep_var,
                                                   dep_var = dep_var,
                                                   stratify_flag = stratify_flag,
                                                   labels_flag = labels_flag,
                                                   legend_flag = legend_flag,
                                                   destination = destination)
        }
      } 
    }
    # single variable summaries, only used for bar plot for categorical variables for counts
    else if (is.na(dep_var)){
      # generate bar plot for counts
      if(indep_var_type %in% c("factor","logical")){
        return_plot <- generate_bar_plot(dataset, indep_var=indep_var, dep_var=dep_var, labels_flag=labels_flag)
      } 
      else if (indep_var_type %in% c("numeric","integer")){
        return(NULL)
      }
    }
    
    # style plot differently for different plots
    if (indep_var_type == "POSIXct"){
      # trend graphs
      return_plot <- style_plot(plot = return_plot,
                               indep_var = paste0("Trend over ",input$select_feature_trend_resolution),
                               dep_var = dep_var,
                               dep_var_type = dep_var_type,
                               stratify_var = stratify_var,
                               stratify_flag = stratify_flag,
                               legend_flag = T,
                               legend_position = legend_position,
                               destination = destination,
                               plot_type = "trend"
      )
    } else if (indep_var_type %in% c("factor","logical") && is.na(dep_var)){
      # single variable bar plot
      return_plot <- style_plot(plot = return_plot,
                               indep_var = indep_var, 
                               indep_var_type = indep_var_type,
                               stratify_var = stratify_var,
                               stratify_flag = stratify_flag,
                               legend_flag = legend_flag,
                               legend_position = legend_position,
                               plot_title = paste0("Summary of ",indep_var),
                               destination = destination,
                               plot_type = "count"
      )
    } else {
      # all correlations
      return_plot <- style_plot(plot = return_plot,
                               indep_var = indep_var, 
                               indep_var_type = indep_var_type,
                               dep_var = dep_var,
                               dep_var_type = dep_var_type, 
                               stratify_var = stratify_var,
                               stratify_flag = stratify_flag,
                               legend_flag = legend_flag,
                               legend_position = legend_position,
                               destination = destination,
                               plot_type = "correlation"
      )
    }
    
    return(return_plot)
  }
  
  generate_bar_plot <- function(dataset, indep_var, dep_var=NA, labels_flag=F, legend_flag = F){
    if(!is.na(dep_var)){
      # compare two variables boxplot, 
      # generate plot labels
      plot_labels <- dataset %>% group_by(!!as.symbol(indep_var), stratify_by, !!as.symbol(dep_var)) %>% 
        summarise(cnt = n()) %>% mutate(value = cnt/sum(cnt), cnt_all=sum(cnt)) %>% ungroup() %>% 
        complete(!!as.symbol(indep_var), stratify_by, !!as.symbol(dep_var), fill=list(cnt=0, value=0, cnt_all=0))
      
      # generate plot, bar plot
      plot_data <-   dataset %>% group_by(!!as.symbol(indep_var), stratify_by, !!as.symbol(dep_var)) %>% 
        summarise(cnt = n()) %>% mutate(pct_cnt=cnt/sum(cnt), cnt_all = sum(cnt)) %>% ungroup() %>%
        complete(!!as.symbol(indep_var), stratify_by, !!as.symbol(dep_var), fill=list(cnt=0, pct_cnt=0, cnt_all=0))
      
      return_plot <- plot_data %>% ggplot() +
        geom_bar(aes(x=!!as.symbol(indep_var), y=pct_cnt, fill=!!as.symbol(dep_var)), 
                 color="black", position=position_dodge(0.9, preserve = 'total'), stat="identity", show.legend=legend_flag) +
        coord_cartesian(ylim=c(-0.05,1.1)) + 
        facet_wrap(~stratify_by, labeller=label_value)
      
      # add plot labels
      if(labels_flag == T){
        return_plot <- return_plot +
          geom_label(data=plot_labels,
                     aes(x=!!as.symbol(indep_var), y=value, group=!!as.symbol(dep_var), label=scales::percent(value, accuracy=2)), 
                     alpha=0.7, size=label_size, position=position_dodge(0.9), show.legend = FALSE) +
          geom_text(data=plot_labels,
                    aes(x=!!as.symbol(indep_var), y=-Inf, fill=!!as.symbol(dep_var), 
                        label=paste0("n=",cnt,"/",cnt_all)),
                    position=position_dodge(0.9, preserve = 'total'), size=label_size, angle=45, hjust="left", show.legend = FALSE)
      }
    } else if(is.na(dep_var)){
      # single variable summary bar plot
      # generate plot labels
      plot_labels <- dataset %>% group_by(!!as.symbol(indep_var), stratify_by) %>%
        summarise(cnt = n()) %>% ungroup() %>%  mutate(value = cnt/sum(cnt), cnt_all=sum(cnt)) %>% 
        complete(!!as.symbol(indep_var), stratify_by, fill=list(cnt=0, value=0, cnt_all=0))
      
      # generate plot, bar plot
      plot_data <-   dataset %>% group_by(!!as.symbol(indep_var), stratify_by) %>%
        summarise(cnt = n()) %>% ungroup() %>% mutate(pct_cnt=cnt/sum(cnt), cnt_all = sum(cnt)) %>% 
        complete(!!as.symbol(indep_var), stratify_by, fill=list(cnt=0, pct_cnt=0, cnt_all=0))
      
      return_plot <- plot_data %>% ggplot() +
        geom_bar(aes(x=!!as.symbol(indep_var), y=pct_cnt, fill=!!as.symbol(indep_var)), 
                 color="black", position=position_dodge(0.9, preserve = 'total'), stat="identity", show.legend = FALSE) +
        coord_cartesian(ylim=c(-0.05,1.1)) + 
        facet_wrap(~stratify_by, labeller=label_value)
      
      # add plot labels
      if(labels_flag == T){
        return_plot <- return_plot +
          geom_label(data=plot_labels,
                     aes(x=!!as.symbol(indep_var), y=value, group=!!as.symbol(indep_var), label=scales::percent(value, accuracy=2)), 
                     alpha=0.7, size=label_size, position=position_dodge(0.9), show.legend = FALSE) +
          geom_text(data=plot_labels,
                    aes(x=!!as.symbol(indep_var), y=-Inf, fill=!!as.symbol(indep_var), 
                        label=paste0("n=",cnt,"/",cnt_all)),
                    position=position_dodge(0.9, preserve = 'total'), size=label_size, angle=45, hjust="left", show.legend = FALSE)
      }
    }
    return(return_plot)
  }
  
  generate_boxplot <- function(dataset, indep_var, dep_var, stratify_flag = F, flip_axes_flag=F, labels_flag){
    # labels
    
    plot_labels <- dataset %>% group_by(!!as.symbol(indep_var), stratify_by) %>%
      summarise(cnt=n(), value = quantile(!!as.symbol(dep_var), c(0.25,0.5,0.75), na.rm=TRUE), probs=c(0.25,0.5,0.75))
    # plot -  side by side grouped bar plot
    return_plot <- dataset %>% ggplot() +
      geom_violin(aes(x=get(indep_var), y=get(dep_var), fill=get(indep_var)), 
                  na.rm=TRUE, trim=FALSE) +
      geom_boxplot(aes(x=get(indep_var), y=get(dep_var)), fill="white", 
                   width = 0.2, na.rm=TRUE, outlier.shape=NA) +
      # jitter extendfs the yaxis too much
      # geom_jitter(data = subset(dataset, get(variable_of_interest) > quantile(get(variable_of_interest), 0.75) + 1.5 * IQR(get(variable_of_interest))),
      #             aes(x=get(correlate_var), y=get(variable_of_interest)),
      #             width = 0.2, height = 0.1, color = "black") + 
      # coord_cartesian(ylim=c(NA,ylim_max)) + 
      facet_wrap(~stratify_by, labeller=label_value)
    
    if(flip_axes_flag == T) return_plot <- return_plot + coord_flip()
    
    if(labels_flag==T){
      return_plot <- return_plot +
      geom_richtext(data=plot_labels,
                 aes(x=!!as.symbol(indep_var), y=value*1.01, label=value),
                 fill="white", size=label_size, position=position_dodge(0.9), angle=45*flip_axes_flag)+
      geom_text(data=plot_labels,
                aes(x=!!as.symbol(indep_var), y=-Inf, label=paste0("n=",cnt)),
                position=position_dodge(0.9), size=label_size, angle=45, hjust="left")
    }
    
    return(return_plot)
  }
  
  generate_scatter_plot <- function(dataset, indep_var, dep_var, stratify_flag = F, stratify_var = NA){
    return_plot <- dataset %>% ggplot() +
      geom_point(aes(x=get(indep_var), y=get(dep_var)), na.rm=TRUE) +
      geom_smooth(aes(x=get(indep_var), y=get(dep_var))) +
      # coord_cartesian(xlim=c(NA,xlim_max),ylim=c(NA,ylim_max)) + 
      stat_cor(aes(x=get(indep_var), y=get(dep_var)), method = "pearson", label.x.npc = "right", label.y.npc="top", hjust=1, size=4) +
      facet_wrap(~stratify_by, labeller=label_value)
    
    return(return_plot)
  }
  
  generate_qq_plot <- function(dataset, variable_of_interest, stratify_var, stratify_flag, destination="screen"){
    return_plot <- ggplot(dataset, aes(sample = !!as.symbol(variable_of_interest))) +
      geom_qq_line() + 
      geom_qq() +
      facet_wrap(~stratify_by)
    
    return_plot <- style_plot(plot = return_plot,
                             indep_var = variable_of_interest,
                             stratify_var = stratify_var, 
                             stratify_flag = stratify_flag,
                             plot_title="Q-Q Plot",
                             x_axis_title = "Theoretical",
                             y_axis_title = "Sample",
                             destination = destination)
    
    return(return_plot)
  }
  
  generate_histogram_plot <- function(dataset, variable_of_interest, stratify_var, stratify_flag, destination="screen"){
    if(length(unique(dataset[[variable_of_interest]]))<10){
      return_plot <- dataset %>% ggplot() + 
        geom_histogram(aes(x=!!as.symbol(variable_of_interest)), bins=length(unique(dataset[[variable_of_interest]])), fill="lightblue", color="black") +
        facet_wrap(~stratify_by)
    }
    else {
      binmin <- min(dataset[[variable_of_interest]], na.rm=T)
      binmax <- max(dataset[[variable_of_interest]], na.rm=T)
      bwidth <- (binmax - binmin) / 10
      return_plot <- dataset %>% ggplot() + geom_histogram(aes(x=!!as.symbol(variable_of_interest)), binwidth=bwidth, fill="lightblue", color="black") +
        facet_wrap(~stratify_by)
    }
    
    return_plot <- style_plot(plot = return_plot, 
                             indep_var = variable_of_interest, 
                             stratify_var = stratify_var, 
                             stratify_flag = stratify_flag,
                             plot_title="Histogram",
                             destination = destination,
                             plot_type = "count")
    
    return(return_plot)
  }
  
  generate_scatter_line_plot <- function(dataset, indep_var, dep_var, stratify_flag = F, labels_flag, legend_flag = T, destination){
    
    # init variables
    dep_var_type <- class(dataset[[dep_var]])
    legend_flag_func <- stratify_flag == T && legend_flag == T
    point_size <- 2
    line_width <- 4
    
    if(destination == "screen"){  
      point_size <- 4
      line_width <- 2
    }
    else if(destination == "slide"){
      point_size <- 6
      line_width <- 3
    }
      
    
    dataset_summary <- 
    # take dataset, group by column whos name is stored in indep_var, average the values in the column whos name is 
      # stored in dep_var, plot a scatter and line, color=column whos name is stored in stratify_var
    if(dep_var_type %in% c("numeric","integer")){
      
      plot_data <- dataset %>% group_by(!!as.symbol(indep_var), stratify_by) %>% 
        summarize(mean_val = mean(!!as.symbol(dep_var)), sd_val = sd(!!as.symbol(dep_var), na.rm=T))
      
      return_plot <- ggplot(data=plot_data) +
        geom_point(aes(x = !!as.symbol(indep_var), y = mean_val, color = stratify_by), size=point_size, shape=18, show.legend = F) +
        geom_line(aes(x = !!as.symbol(indep_var), y = mean_val, color = stratify_by), linewidth=line_width, show.legend = legend_flag_func) 
        # geom_errorbar(aes(x=!!as.symbol(indep_var), ymin = mean_val-sd_val, ymax = mean_val+sd_val, color=stratify_by),width = 0.2)
      
      if(labels_flag){ return_plot <- return_plot + geom_label(data=plot_data, aes(x = !!as.symbol(indep_var), 
                                                                                   y = mean_val, label = mean_val, color = stratify_by))}
    } 
    else if (dep_var_type %in% c("factor","logical")){
      
      plot_data <- dataset %>% group_by(stratify_by, !!as.symbol(indep_var), !!as.symbol(dep_var)) %>% 
        summarize(cnt = n())
      
      return_plot <- ggplot(data=plot_data) +
        geom_point(aes(x = !!as.symbol(indep_var), y = cnt, color = !!as.symbol(dep_var)), size=point_size, shape=18, show.legend = F) +
        geom_line(aes(x = !!as.symbol(indep_var), y = cnt, color = !!as.symbol(dep_var)), linewidth=line_width, show.legend = legend_flag_func) + 
        facet_wrap(~stratify_by)
      
      if(labels_flag){ return_plot <- return_plot + geom_label(data=plot_data, aes(x = !!as.symbol(indep_var), y = cnt, 
                                                                                   label = cnt, color = stratify_by))}
    }
    
    return_plot <- return_plot +
      theme(axis.text.x = element_text(angle = 45))
    
    return(return_plot)
  }
  
  style_plot <- function(plot, 
                         indep_var=NA, indep_var_type=NA, 
                         dep_var=NA, dep_var_type=NA, 
                         stratify_var=NA, stratify_flag=F,
                         legend_flag = F, legend_position = "right",
                         plot_title=NA, x_axis_title=NA, y_axis_title=NA, 
                         destination="screen", plot_type="correlation"){
    #3 possibilities
    
    # single variable bar plot
    if(plot_type == "count"){
      # plot titles config
      indep_var_title_func <- str_to_title(gsub("_", " ", indep_var))
      stratify_var_title_func <- str_to_title(gsub("_"," ",stratify_var))
      x_axis_title_func <- indep_var_title_func
      y_axis_title_func <- "Count"
      legend_title_func <- indep_var_title_func
      plot_title_func <- indep_var_title_func
      plot_title_func <- paste0(plot_title_func," stratified by ",stratify_var_title_func)
      plot_title_func <- str_to_title(plot_title_func)
      
      # no legend config for single variable bar plot
    }
    # trend graph
    else if (plot_type == "trend"){
      # plot titles config
      indep_var_title_func <- "Trend period"
      dep_var_title_func <- str_to_title(gsub("_", " ", dep_var))
      stratify_var_title_func <- str_to_title(gsub("_"," ",stratify_var))
      x_axis_title_func <- indep_var_title_func
      y_axis_title_func <- dep_var_title_func
      legend_title_func <- stratify_var_title_func
      plot_title_func <- paste0("Trend of ", dep_var_title_func)
      if(stratify_flag == T) plot_title_func <- paste0(plot_title_func," stratified by ",stratify_var_title_func)
      plot_title_func <- str_to_title(plot_title_func)
      
      #legend config
      legend_flag_local <- ifelse(stratify_flag==T,"legend","none")
      # plot <- plot + guides(color = legend_flag_local)
    }
    
    # dual variable - make some choices based on dep_var_type factor/logical
    else if (plot_type == "correlation") {
      # plot titles config
      dep_var_title_func <- str_to_title(gsub("_", " ", dep_var))
      indep_var_title_func <- str_to_title(gsub("_", " ", indep_var))
      stratify_var_title_func <- str_to_title(gsub("_"," ",stratify_var))
      x_axis_title_func <- indep_var_title_func
      y_axis_title_func <- ifelse(dep_var_type %in% c("factor","logical"),
                                  paste0(dep_var_title_func,": % of cases"),
                                  dep_var_title_func)
      plot_title_func <- paste0(indep_var_title_func, " vs ", dep_var_title_func)
      if(stratify_flag == T) plot_title_func <- paste0(plot_title_func," stratified by ",stratify_var_title_func)
      plot_title_func <- str_to_title(plot_title_func)
      
      #legend config
      legend_title_func <- ifelse(dep_var_type %in% c("factor","logical"),
                                  dep_var_title_func,
                                  indep_var_title_func)
      legend_flag_local <- ifelse(legend_flag==T,"legend","none")
      # if(coalesce(indep_var_type,"none") == "factor" && coalesce(dep_var_type,"none") == "factor")
      #     plot <- plot + guides(fill = guide_legend(legend_flag_local))
    }
    # earmark this as it works for the QQ plot but if other plot types arise, then this would be the template
    # as it is, it will never trigger because of default plot type parameter = "correlation"
    else {
      # plot titles config
      dep_var_title_func <- str_to_title(gsub("_", " ", dep_var))
      indep_var_title_func <- str_to_title(gsub("_", " ", indep_var))
      stratify_var_title_func <- str_to_title(gsub("_"," ",stratify_var))
      x_axis_title_func <- indep_var_title_func
      y_axis_title_func <- ifelse(dep_var_type %in% c("factor","logical"),
                                  paste0(dep_var_title_func,": % of cases"),
                                  dep_var_title_func)
      plot_title_func <- paste0(indep_var_title_func, " vs ", dep_var_title_func)
      if(stratify_flag == T) plot_title_func <- paste0(plot_title_func," stratified by ",stratify_var_title_func)
      plot_title_func <- str_to_title(plot_title_func)
      
      #legend config
      legend_title_func <- ifelse(dep_var_type %in% c("factor","logical"),
                                  dep_var_title_func,
                                  indep_var_title_func)
      legend_flag_local <- ifelse(legend_flag==T,"legend","none")
      # if(coalesce(indep_var_type,"none") == "factor" && coalesce(dep_var_type,"none") == "factor")
      #     plot <- plot + guides(fill = guide_legend(legend_flag_local))
    }
    #
    
    if(!is.na(x_axis_title)) x_axis_title_func <- x_axis_title
    if(!is.na(y_axis_title)) y_axis_title_func <- y_axis_title
    if(!is.na(plot_title)) plot_title_func <- plot_title
    
    #variables are now established based on variable and plot type
    #first, basic theming, so we can overwrite later on; if we do this at the end, things like legend position get overridden
    
    if(destination=="slide"){
      plot <- plot + theme_slide
    } 
    else if(destination == "screen"){
      plot <- plot + theme_screen
    }    

    # final modification to overrwrite any strict parameters passed to the function
    plot <- plot +
      ylab(y_axis_title_func) +
      xlab(x_axis_title_func) +
      scale_fill_brewer(name=legend_title_func, palette=input$select_fill_palette, ) +
      scale_color_brewer(name=legend_title_func, palette=input$select_fill_palette) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = legend_position) +
      ggtitle(plot_title_func)
    
    return(plot)
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
        db_connection <<- create_db_connection_toad()
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
        dataset_loaded <<- process_dataset(query_result)
        showNotification("Dataset loaded successfully.")
      }, error = function(e){
        showNotification(paste0("Error: ",e,"\nDataset load failure."))
        return()
      })
    }
    
  })
  
  observeEvent(input$button_load_dataset,{
    # filters <- list(
    #   "R data files" = list("rds"),
    #   "CSV" = list("csv"),
    #   "Excel" = list("xls, xlsx"),
    #   "All files" = list("*")
    # )
    
    tryCatch({
      filename <- rstudioapi::selectFile(caption = "Load dataset from file",
                                         filter = "Data files (*.rds | *.csv | *.xls | *.xlsx)",
                                         existing = TRUE)
      if(is.null(filename)){
        showNotification("Loading dataset canceled by user.")
        return()
      }
      if(!file.exists(filename)){
        showNotification("File does not exist.")
        return()
      }
      if(length(filename)>0 && file.exists(filename)){
        switch(tools::file_ext(filename),
              "rds" = {df_from_file <- readRDS(filename)},
              "csv" = {
                  date_formats <- col_datetime(format = c("ymd", "mdy", "dmy"))
                  df_from_file <- read_csv(filename, col_types=date_formats)
                  },
              "xls" = ,
              "xlsx" = {df_from_file <- read_excel(filename)}
        )
        process_dataset(df_from_file)
        showNotification("Loaded file successfully!")
      } else {
        showNotification("Unknown error loading dataset.")
      }
    }, error = function(e) {
    
      showNotification("Loading dataset failed.")
      showNotification(paste0("Error Message: ", conditionMessage(e)))
    }, finally =  {
      
    })
  })
  
  observeEvent(input$button_save_dataset,ignoreInit = T, {
    datafile <- NA
    try(filename <- rstudioapi::selectFile(caption = "Save dataset to file",
                                           filter = "RDS Files (*.rds)",
                                           existing = FALSE)
    )
    if(!is.na(filename)){
      saveRDS(dataset_loaded, file=filename)
      showNotification("Saved file successfully!")
    }
    else(showNotification("Saving dataset canceled"))
    
  })
  
  #### Cohort filtering/feature modification UI events ####
  
  observeEvent(input$button_feature_filter_add, ignoreInit = T, {
    feature_filter[[input$select_feature_filter_add]] <<- list(initialized=FALSE, feature_factor=NA, 
                                                               feature_range=NA, feature_factor_all_levels=NA, feature_factor_selected_levels=NA, feature_factor_cut_points=NA)
    shinyjs::enable("select_feature_filter_modify")
    updateSelectInput(session,inputId="select_feature_filter_modify", choices=names(feature_filter), selected=input$select_feature_filter_add)
    updateSelectInput(session,inputId="select_feature_filter_add", choices=names(dataset_loaded)[!(names(dataset_loaded) %in% names(feature_filter))])
    
  })
  
  observeEvent(input$button_feature_filter_remove, ignoreInit = T, {
    # print(paste0("feature filter length ",length(feature_filter)))
    # print(feature_filter)
    feature_filter[[input$select_feature_filter_modify]] <<- NULL
    updateSelectInput(session,inputId="select_feature_filter_add", choices=names(dataset_loaded)[!(names(dataset_loaded) %in% names(feature_filter))])
    if(length(feature_filter)>0)
      updateSelectInput(session,inputId="select_feature_filter_modify", choices=names(feature_filter), selected=names(feature_filter)[1])
    else
      updateSelectInput(session,inputId="select_feature_filter_modify", choices=NA)    
    
  })
  
  observeEvent(input$select_feature_filter_modify, ignoreInit=TRUE, {
    
    feature_type <- class(dataset_loaded[[input$select_feature_filter_modify]])
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
        feature_filter[[input$select_feature_filter_modify]]$initialized <<- TRUE
        if(feature_type %in% c("integer","numeric")){
          #set slider range and range element in list
          feature_filter[[input$select_feature_filter_modify]]$feature_range <<- c(min(dataset_loaded[[input$select_feature_filter_modify]], na.rm=TRUE),
                                                                                   max(dataset_loaded[[input$select_feature_filter_modify]], na.rm=TRUE))
          # init factoring to no
          feature_filter[[input$select_feature_filter_modify]]$feature_factor <<- 1
          feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <<- NA
          feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <<- NA
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
          feature_filter[[input$select_feature_filter_modify]]$feature_range <<- c(NA,NA)
          feature_filter[[input$select_feature_filter_modify]]$feature_factor <<- 1
          feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <<- levels(dataset_loaded[[input$select_feature_filter_modify]])
          feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <<- levels(dataset_loaded[[input$select_feature_filter_modify]])
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
                            min=min(dataset_loaded[[input$select_feature_filter_modify]], na.rm=TRUE),
                            max=max(dataset_loaded[[input$select_feature_filter_modify]], na.rm=TRUE))
          
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
    feature_filter[[input$select_feature_filter_modify]]$feature_range <<- input$slider_feature_filter_range
  })
  
  observeEvent(input$radio_feature_filter_factor_type, ignoreInit=TRUE, {
    feature_filter[[input$select_feature_filter_modify]]$feature_factor <<- input$radio_feature_filter_factor_type
    if(input$radio_feature_filter_factor_type==1){
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <<- NA
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <<- NA
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <<- NA
      shinyjs::enable("text_feature_filter_cut_points")
      shinyjs::enable("select_feature_filter_factor_levels")
      updateTextInput(session, inputId="text_feature_filter_factor_cutpoints", value="")
      updateSelectInput(session, inputId="select_feature_filter_factor_levels", choices=NA, selected=NA)
      shinyjs::disable("text_feature_filter_factor_cut_points")
      shinyjs::disable("select_feature_filter_factor_levels")
    }
    else if(input$radio_feature_filter_factor_type==2){
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <- {dataset_loaded[[input$select_feature_filter_modify]] %>% as.factor %>% levels}
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <- {dataset_loaded[[input$select_feature_filter_modify]] %>% as.factor %>% levels}
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <<- NA
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
        feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <<- unlist(strsplit(input$text_feature_filter_factor_cut_points,","))
      else
        feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points <<- NA
      
      print("saving cut point vector and default factor and selected factor levels")
      feature_cut_levels <- dataset_loaded[[input$select_feature_filter_modify]] %>% cut(breaks=c(-Inf,feature_filter[[input$select_feature_filter_modify]]$feature_factor_cut_points,Inf)) %>% levels()
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels <<- feature_cut_levels
      feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <<- feature_cut_levels
      updateSelectInput(session, inputId="select_feature_filter_factor_levels", choices=feature_filter[[input$select_feature_filter_modify]]$feature_factor_all_levels,
                        selected=feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels)
    }
  })
  
  observeEvent(input$select_feature_filter_factor_levels, ignoreInit=TRUE, {
    print("saving selected feature levels")
    feature_filter[[input$select_feature_filter_modify]]$feature_factor_selected_levels <<- input$select_feature_filter_factor_levels
  })

  # mark dataset for filtering moving between tabs if anything is changed
  # this will allow for updating of the selectInputs for variable selection based on new variable types
  observeEvent({input$button_feature_filter_add
               input$button_feature_filter_remove
               input$slider_feature_filter_range
               input$radio_feature_filter_factor_type
               input$text_feature_filter_factor_cut_points
               input$select_feature_filter_factor_levels}, ignoreInit = TRUE, {
                 feature_filter_modified <<- T           
               })
  
  # if dataset has been modified, filter dataset again, which also updates UI and variable selectInputs
  observeEvent(input$tabsetpanel_data_explorer_sidebar, {
    
    if(feature_filter_modified == T && input$tabsetpanel_data_explorer_sidebar == "Explore data"){
      filter_dataset()
      feature_filter_modified <<- F
    } else if (input$tabsetpanel_data_explorer_sidebar == "Cohort selection"){
      updateTabsetPanel(session, inputId="tabsetpanel_data_explorer_mainpanel", selected="Dataset")
      
    }
  })
  
  observeEvent(input$button_update_dataset, {
    if(feature_filter_modified == T){
      filter_dataset()
      update_dataset_descriptive_tables()
      feature_filter_modified <<- F
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
    if(class(dataset_loaded[[input$select_feature_a]]) %in% c("numeric","integer")) {
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
    if(class(dataset_loaded[[input$select_feature_b]]) %in% c("numeric","integer")) {
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
      stratify_dataset()
      
      # the below is present in the correlation button code, but NOT necessary for the variable summary, because we want this to show up for categoricals
      # # filter out any records that have NA in the variables of interest, and droplevels() to make sure if any factor levels are not present, they dont get graphed
      # dataset_loaded_local <- dataset_filtered %>% filter(!is.na(!!as.symbol(input$select_feature_a)),!is.na(!!as.symbol(input$select_feature_b))) %>% 
      #   droplevels()
      
      ## variables to pass
      dataset_loaded_local <- dataset_filtered
      variable_of_interest <- input$select_feature_a
      variable_of_interest_type <- class(dataset_loaded_local[[variable_of_interest]])
      stratify_flag <- input$checkbox_stratify
      stratify_var <- input$select_stratify
      stratify_type <- class(dataset_loaded_local[[input$select_stratify]])
      labels_flag <- input$checkbox_show_labels
      legend_flag <- input$checkbox_show_legend
      
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
                                                          destination = "screen")
      
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
      stratify_dataset()
      
      # filter out any records that have NA in the variables of interest, and droplevels() to make sure if any factor levels are not present, they dont get graphed
      # not present in variable summary because we WANT NAs there
      dataset_loaded_local <- dataset_filtered %>% filter(!is.na(!!as.symbol(input$select_feature_a)),!is.na(!!as.symbol(input$select_feature_b))) %>% 
        droplevels()
      
      variable_of_interest <- input$select_feature_a
      correlate_flag <- T
      correlate_var <- input$select_feature_b
      stratify_var <- input$select_stratify
      stratify_flag <- input$checkbox_stratify
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
                                    destination = "screen")
      
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
      stratify_dataset()
      # filter out any records that have NA in the variables of interest, and droplevels() to make sure if any factor levels are not present, they dont get graphed
      dataset_loaded_local <- dataset_filtered %>% filter(!is.na(!!as.symbol(input$select_feature_a)),!is.na(!!as.symbol(input$select_feature_trend))) %>% 
        droplevels()
      
      variable_of_interest <- input$select_feature_a
      # unused after removing correlate checkbox, and unused in this function anyway
      # correlate_flag <- input$checkbox_correlate
      correlate_var <- input$select_feature_b
      stratify_var <- input$select_stratify
      stratify_flag <- input$checkbox_stratify
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
                                  destination = "screen")
      
      data_explorer_plot(return_plot)
    }, error = function(e) {
      showNotification(paste0("Error in summarize feature: ",e))
    })
    
    shinyjs::hide("loading-overlay") # Show the loading overlay
  })

  #### Report generation events ####
  
  observeEvent(input$button_generate_table_one, ignoreInit = T, {
    
    stratify_dataset()
    updateTabsetPanel(session, inputId="tabsetpanel_data_explorer_mainpanel", selected="gt Table")
    
    returnval <- NA
    
    if(input$checkbox_stratify){
      returnval <- dataset_filtered %>% tbl_summary(by = !!as.symbol(input$select_stratify), 
                                                    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} / {N} ({p}%)")) %>%
        modify_header(label = paste0("**",input$select_stratify,"**")) %>% add_p()
    }
    else {
      returnval <- dataset_filtered %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                                     all_categorical() ~ "{n} / {N} ({p}%)"))
    }
    
    print(returnval)
    returnval %<>% as_gt()
    
    data_explorer_table(returnval)
  })
    
  observeEvent(input$button_generate_report,{
    stratify_dataset()
    
    # remove posixct and non-numeric and non-factor and non-logical variables from dataset
    dataset_report <- dataset_filtered %>% select_if(~ (is.numeric(.) || is.factor(.) || is.logical(.)) && !is.POSIXct(.))
    variable_of_interest <- input$select_feature_a
    variable_of_interest_role <- NA
    stratify_flag <- input$checkbox_stratify
    stratify_var <- input$select_stratify
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
    
    report_object_to_write <<- list(object_for_output = NULL, report_format = NULL)
    
    report_object_to_write <<- generate_report(dataset = dataset_report,
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
    
    report_filename <<- paste0("report-", Sys.Date(), ".", file_ext)
    
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
  
  output$button_download_report <- shiny::downloadHandler(
    filename = function() {report_filename},
    content = function(report_filename_temp) 
    {
      write_report_to_file(report_object_to_write, report_filename_temp)
    }
  )
  
  #### END Explore Data Events
  
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
  # output$text_debug <- renderText({names(dataset_loaded)
  # }
  
} #### END server main

#### END Server code ###

# Run app
shinyApp(ui = ui, server = server, onStart = onStart)

