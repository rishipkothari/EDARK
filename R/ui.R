#' @import shiny
#' @import shinyjs
#' @import shinythemes
#' @import shinyBS
#' @import reactable
#' @import gt

NULL

#### UI Definition ####

ui_edark <-
  navbarPage("EDARK", selected=1,
             theme = shinytheme("flatly"),
             useShinyjs(),
             # css to center the progress bar
             tags$head(
               tags$style(
                 shiny::HTML("
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
                                                                                   value="", height=400, placeholder="Enter query here", resize="both"),
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
                                                                                                                       choices=c("Word document","PDF","Powerpoint", "HTML Web Page"), selected="Powerpoint"), width=8)
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
                                                                                                  column(checkboxInput(inputId="checkbox_show_legend", label="Show legend?", value=FALSE), width=4)),
                                                                                                fluidRow(
                                                                                                  column(h5("Legend position"), width = 4),
                                                                                                  column(selectInput(inputId="select_legend_position", label=NULL,
                                                                                                                     choices=c("top","right","bottom"), selected="right"), width=8)
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
