#' User Interface for exploring mixture result
#'
#' @param id Module's Id
#' @param plotly Logical, add a dependance to plotly ?
#'
#' @return mixtureUI : a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#'
#' @rdname shiny-module-mixture
#'
#' @examples
#' \dontrun{
#' library("shiny")
#' runApp(system.file("modules/module-mixture.R", package = "opera"))
#' }
mixtureUI <- function(id, plotly = FALSE) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) 
    stop("shiny is needed for this function to work. Install it via install.packages(\"shiny\")", call. = FALSE)
  
  if (plotly && !requireNamespace("plotly", quietly = TRUE)) 
    stop("plotly is needed for this function to work. Install it via install.packages(\"plotly\")", call. = FALSE)
  
  # namespace
  ns <- NS(id)
  
  fluidRow(
    if (plotly) plotly::plotlyOutput(outputId = paste0("plotly-dep-", sample.int(1e9, 1)), height = 0),
    uiOutput(outputId = ns("ui"))
  )
}


#' Server for exploring mixture result
#'
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param mixture a reactive function containing an object of class mixture
#' @param type    which graphic to plot, a value between 1 and 6, can be a reactive function
#' @param ncol    number of column for displaying plots
#' @param plotly  logical, use plotly to plot output, can be a reactive function
#'
#' @export
#' 
#' @rdname shiny-module-mixture
#'
mixtureServer <- function(input, output, session, mixture, type, ncol = 2, plotly) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) 
    stop("shiny is needed for this function to work. Install it via install.packages(\"shiny\")", call. = FALSE)
  
  if ((!missing(plotly) && (is.logical(plotly) && plotly) | !is.logical(plotly)) && !require("plotly")) 
    stop("plotly is needed for this function to work. Install it via install.packages(\"plotly\")", call. = FALSE)
  
  
  # plotly params
  if (missing(plotly))
    plotly <- reactive({FALSE})
  if (is.logical(plotly))
    plotly <- reactive({plotly})
  
  
  # namespace
  ns <- session$ns
  
  
  # ncol
  colwidth <- 12 / ncol
  
  
  # output
  output$ui <- renderUI({
    if (plotly()) {
      funOutput <- plotly::plotlyOutput
    } else {
      funOutput <- shiny::plotOutput
    }
    lapply(
      X = type(),
      FUN = function(x) {
        column(
          width = colwidth,
          funOutput(outputId = ns(paste(ifelse(plotly(), "ggplotly-mixture", "ggopera-mixture"), x, sep = "-")))
        )
      }
    )
  })
  
  
  # render
  observeEvent(list(type(), plotly()), {
    lapply(
      X = type(),
      FUN = function(x) {
        if (plotly()) {
          output[[paste("ggplotly-mixture", x, sep = "-")]] <- plotly::renderPlotly({
            plotly::ggplotly(ggopera(mixture(), type = x))
          })
        } else {
          output[[paste("ggopera-mixture", x, sep = "-")]] <- shiny::renderPlot({
            ggopera(mixture(), type = x)
          })
        }
      }
    )
  })
  
}




