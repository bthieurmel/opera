


# Shiny Modules -----------------------------------------------------------

library("opera")

suppressWarnings(source(system.file("modules/modules-data.R", package = "opera"), echo = FALSE, local = TRUE))




# mixture -----------------------------------------------------------------

library("shiny")

ui <- fluidPage(
  tags$h1("Explore mixture results"),
  br(),
  fluidRow(
    column(
      width = 4,
      radioButtons(inputId = "model", label = "Modele :", choices = c("MLpol", "Ridge", "BOA"), inline = TRUE)
    ),
    column(
      width = 6,
      checkboxGroupInput(inputId = "type", label = "Type :", choices = 1:6, selected = 1:6, inline = TRUE)
    ),
    column(
      width = 2,
      checkboxInput(inputId = "plotly", label = "Plotly", value = FALSE)
    )
  ),
  mixtureUI("opera")
)

server <- function(input, output, session) {
  
  # type_graph <- reactiveVal(1:6) 
  # observeEvent(input$type, {
  #   type_graph(input$type)
  # })
  type_graph <- reactive({
    input$type
  })
  
  opera_mixture <- reactive({
    if (input$model == "MLpol") {
      MLpol
    } else if (input$model == "Ridge") {
      Ridge
    } else if (input$model == "BOA") {
      BOA
    }
  })
  
  callModule(module = mixtureServer, id = "opera", mixture = opera_mixture, type = type_graph, ncol = 3, plotly = reactive({input$plotly})) #
}

shinyApp(ui = ui, server = server)
