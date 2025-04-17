library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(dplyr)
library(mongolite)

# Load environment variables
Sys.getenv("EMAIL_USER")
Sys.getenv("EMAIL_PASSWORD")
Sys.getenv("MONGO_DB_USERNAME")
Sys.getenv("MONGO_DB_PASSWORD")
Sys.getenv("JAVA_HOME")

# Load helper functions
source("mongodb_functions.R")
source("vig_models.R")
source("helpers.R")

# Load modules
source("birdieboard_module.R")

# Define UI
ui <- navbarPage(
    theme = shinytheme("slate"),
    title = "ForeCaddy",
    header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tabPanel("BirdieBoard", birdieboardUI("birdie"))
)
# Define Server
server <- function(input, output, session) {
    callModule(birdieboardServer, "birdie")
}

# Run App
shinyApp(ui = ui, server = server)