# spinner and overlay

library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  
  # Button to start the time-consuming operation
  actionButton("startBtn", "Start Time-Consuming Task"),
  
  # Loading overlay
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
  "))
)

server <- function(input, output, session) {
  # Function to simulate a time-consuming task
  simulateTimeConsumingTask <- function() {
    Sys.sleep(3) # Simulate a 3-second delay
  }
  
  # Observe the button click event
  observeEvent(input$startBtn, {
    shinyjs::show("loading-overlay") # Show the loading overlay
    simulateTimeConsumingTask()
    shinyjs::hide("loading-overlay") # Hide the loading overlay when the task is complete
  })
}

shinyApp(ui, server)



# progress bar

library("shiny")

ui <- fluidPage(
  actionButton(inputId = "go", label = "Launch long calculation"), #, onclick = "$('#my-modal').modal().focus();"
  
  # css to center the progress bar
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           height: 100px;
           width: 800px;
           position:fixed;
           top: calc(50% - 50px);
           left: calc(50% - 400px);
           font-size: 250%;
           text-align: center;
           }
           "
      )
    )
  )
)

server <- function(input, output, session) {
  
  value <- reactiveVal(0)
  
  observeEvent(input$go, {
    withProgress(message = 'Calculation in progress', value = 0, detail="0%", {
      # run calculation
      for (i in 1:10) {
        Sys.sleep(0.5)
        newValue <- value() + 1
        value(newValue)
        incProgress(1/10,detail = paste0(i*10,"%"))
      }
      Sys.sleep(0.5)
    })
  })
  
}

shinyApp(ui = ui, server = server)
