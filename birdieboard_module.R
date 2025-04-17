birdieboardUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
             selectInput(ns("master_golfers"), "Choose Golfers:", choices = NULL, multiple = FALSE, width = '200px'),
             actionButton(ns("add_golfer"), "Add >>"),
             actionButton(ns("remove_golfer"), "<< Remove"),
             selectInput(ns("selected_golfers"), "Selected Golfers:", choices = NULL, multiple = TRUE, width = '100%'),
             br()
      ),
      column(6,
             selectInput(ns("bbperiod"), "Choose Period:", choices = c("front 9", "back 9", "full 18"), selected = "full 18", width = "150px"),
             numericInput(ns("risk"), "Risk Amount:", value = 5, step = 5, width = "150px"),
             br()
      ),
      column(6,
             actionButton(ns("placeWager"), "Place Wager"),
             br(),
      ),
      column(12,
             textOutput(ns("roundData"))
             ),
      br(),
      column(12,
             DTOutput(ns("wagerTicket"))
      ),
      br()
    ),
    hr(),
    fluidRow(
      column(12,
             DTOutput(ns("scoreCard"))
      )
    ),
    hr()
  )
}

birdieboardServer <- function(input, output, session) {
  ns <- session$ns
  
  all_golfers <- reactive({
    pull_from_mongo(tour = "pga", db = "PreWorkFlow", collection = "Field") %>%
      distinct(playername) %>%
      arrange(playername) %>%
      pull(playername)
  })
  
  observe({
    updateSelectInput(session, "master_golfers", choices = all_golfers())
  })
  
  selected_golfers <- reactiveValues(list = c())
  
  observeEvent(input$add_golfer, {
    # Ensure a golfer is selected before adding
    if (!is.null(input$master_golfers) && input$master_golfers != "") {
      selected_golfers$list <- unique(c(selected_golfers$list, input$master_golfers))
      
      # Update the UI with the new selected golfers
      updateSelectInput(session, "selected_golfers", choices = selected_golfers$list, selected = selected_golfers$list)
    }
  })
  
  observeEvent(input$remove_golfer, {
    if (!is.null(input$selected_golfers)) {
      selected_golfers$list <- setdiff(selected_golfers$list, input$selected_golfers)
      updateSelectInput(session, "selected_golfers", choices = selected_golfers$list)
    }
  })
  
  reactiveDisplay <- reactive({
    selected_players <- selected_golfers$list
    
    if (is.null(selected_players) || length(selected_players) < 2) {
      df <- data.frame(message = "Choose 2+ golfers to include in your ticket.")
      return(df)
    }
    
    df <- birdie_board_lineup(players = selected_players, bbperiod = input$bbperiod) 
    return(df)
  })
  
  reactivePricing <- reactive({
    selected_players <- selected_golfers$list
    
    if (length(selected_players) > 1) {
      dt <- birdie_board_ticket(players = selected_players, 
                                bbperiod = input$bbperiod,
                                vig_cents = 20 + 2 * length(selected_players))
      
      if (!is.null(input$risk) && !is.na(input$risk) && input$risk >= 0) {
        dt <- dt %>%
          mutate(
            riskAmount = sprintf("$%.2f", input$risk),
            toWin = sprintf("$%.2f", input$risk * ifelse(as.numeric(mlPrice) < 0, abs(100 / as.numeric(mlPrice)), as.numeric(mlPrice) / 100))
          )
      }
    } else {
      dt <- data.frame(message = "No Golfers Selected")
    }
    
    return(dt)
  })
  
  output$roundData <- renderText({
    field <- pull_from_mongo(tour = "pga", db = "PreWorkFlow", collection = "Field")
    event <- field$event_name[1]
    course <- field$course_name[1]
    round <- field$current_round[1]
    metadata <- paste0(event, " | ", course, " | Round ", round)
    return(metadata)
  })
  
  output$wagerTicket <- renderDT({
    data <- reactivePricing()
    if (is.null(data)) {
      data <- data.frame(message = "No golfers selected or no data available.")
      return(DT::datatable(data, 
                           options = list(
                             searching = FALSE,
                             paging = FALSE,
                             info = FALSE,
                             autoWidth = FALSE),
                           rownames = FALSE))
    } else {
    
      DT::datatable(data, options = list(
        paging = FALSE,
        info = FALSE,
        autoWidth = FALSE, 
        searching = FALSE,
        lengthChange = FALSE,
        initComplete = JS("
              function(settings, json) {
                  $(this.api().table().body()).css({'color': 'white'});
                  $(this.api().table().header()).css({'color': 'white'});
              }
          ")
        ),
      rownames = FALSE
      )
    }
  }, server = FALSE)
  
  output$scoreCard <- renderDT({
    data <- reactiveDisplay()
    
    if (is.null(data)) {
      data <- data.frame(message = "No golfers selected or no data available.")
      return(DT::datatable(data, 
                           options = list(
                             paging = FALSE,
                             searching = FALSE,
                             info = FALSE,
                             autoWidth = FALSE),
                           rownames = FALSE))
    } else {
    
      # Only format if there are enough columns
      if (ncol(data) >= 2) {
        data[ , 2:ncol(data)] <- lapply(data[ , 2:ncol(data)], function(x) {
          if (is.numeric(x)) {
            sprintf("%.1f%%", 100 * x)
          } else {
            x
          }
        })
      }
      
      # Create optional columnDefs only if enough columns
      column_defs <- if (ncol(data) >= 2) {
        list(list(
          targets = 1:(ncol(data) - 1),
          className = 'dt-right'
        ))
      } else {
        list()
      }
      
      DT::datatable(
        data,
        options = list(
          paging = FALSE,
          info = FALSE,
          autoWidth = FALSE,
          searching = FALSE,
          lengthChange = FALSE,
          pageLength = max(18, nrow(data)),
          columnDefs = column_defs,
          initComplete = JS("
      function(settings, json) {
        $(this.api().table().body()).css({'color': 'white'});
        $(this.api().table().header()).css({'color': 'white'});
      }
    ")
        ),
        rownames = FALSE
      )
    }
  }, server = FALSE)
}