# app.R  

# Load required libraries  
library(shiny)  
library(shinydashboard)  
library(httr)  
library(jsonlite)  
library(ggplot2)  
library(DT)  
library(plotly)  

# API URL - update if your API is running on a different host/port  
api_url <- "http://127.0.0.1:8080/predict"  

# Function to call the API and return the response  
call_api <- function(payload) {  
  tryCatch({  
    res <- POST(api_url, body = payload, encode = "json", timeout(5))  
    return(res)  
  }, error = function(e) {  
    message("API call error: ", e$message)  
    return(NULL)  
  })  
}  

# Define the UI  
ui <- dashboardPage(  
  dashboardHeader(title = "Machine Failure Prediction"),  
  dashboardSidebar(  
    sidebarMenu(  
      menuItem("Prediction", tabName = "prediction", icon = icon("dashboard")),  
      menuItem("History", tabName = "history", icon = icon("history")),  
      menuItem("Sensitivity", tabName = "sensitivity", icon = icon("chart-line"))  
    )  
  ),  
  dashboardBody(  
    tabItems(  
      # Prediction tab  
      tabItem(tabName = "prediction",  
              fluidRow(  
                box(  
                  title = "Sensor Inputs", status = "primary", solidHeader = TRUE, width = 6,  
                  dateInput("date", "Date:", value = Sys.Date()),  
                  fluidRow(  
                    column(4, selectInput("hour", "Hour:", choices = sprintf("%02d", 0:23), selected = format(Sys.time(), "%H"))),  
                    column(4, selectInput("minute", "Minute:", choices = sprintf("%02d", 0:59), selected = format(Sys.time(), "%M"))),  
                    column(4, selectInput("second", "Second:", choices = sprintf("%02d", 0:59), selected = format(Sys.time(), "%S")))  
                  ),  
                  sliderInput("sensor1", "Sensor 1:", min = 0, max = 1, value = 0.5, step = 0.01),  
                  sliderInput("sensor2", "Sensor 2:", min = 0, max = 1, value = 0.5, step = 0.01),  
                  sliderInput("sensor3", "Sensor 3:", min = 0, max = 1, value = 0.5, step = 0.01),  
                  actionButton("predict", "Predict", class = "btn-primary")  
                ),  
                box(  
                  title = "Prediction Result", status = "info", solidHeader = TRUE, width = 6,  
                  plotlyOutput("gauge"),  
                  verbatimTextOutput("prediction_text"),  
                  DTOutput("result_table")  
                )  
              ),  
              box(  
                title = "Debug Info", status = "warning", solidHeader = TRUE, width = 12,  
                verbatimTextOutput("debug_info")  
              )  
      ),  
      # History tab  
      tabItem(tabName = "history",  
              fluidRow(  
                box(  
                  title = "Prediction History", status = "primary", solidHeader = TRUE, width = 12,  
                  DTOutput("history_table")  
                )  
              )  
      ),  
      # Sensitivity tab  
      tabItem(tabName = "sensitivity",  
              fluidRow(  
                box(  
                  title = "Sensitivity Analysis", status = "primary", solidHeader = TRUE, width = 6,  
                  selectInput("sensitivity_sensor", "Sensor to vary:", choices = c("sensor1", "sensor2", "sensor3"), selected = "sensor1"),  
                  sliderInput("sensitivity_range", "Range for sensor:", min = 0, max = 1, value = c(0, 1)),  
                  actionButton("analyze", "Analyze", class = "btn-primary")  
                ),  
                box(  
                  title = "Sensitivity Plot", status = "info", solidHeader = TRUE, width = 6,  
                  plotlyOutput("sensitivity_plot")  
                )  
              )  
      )  
    )  
  )  
)  

# Define server logic  
server <- function(input, output, session) {  
  # Reactive value to store debug messages  
  debug_msg <- reactiveVal("No API request made yet")  
  
  # Reactive value to store prediction history  
  history_data <- reactiveVal(data.frame(  
    DateTime = character(),  
    Sensor1 = numeric(),  
    Sensor2 = numeric(),  
    Sensor3 = numeric(),  
    Prediction = character(),  
    Probability = numeric(),  
    stringsAsFactors = FALSE  
  ))  
  
  # Initialize prediction text  
  output$prediction_text <- renderText({  
    "No prediction available"  
  })  
  
  # Initialize result table  
  output$result_table <- renderDT({  
    data.frame("Message" = "No prediction available")  
  }, options = list(dom = "t"))  
  
  # Initialize debug info  
  output$debug_info <- renderPrint({  
    cat(debug_msg())  
  })  
  
  # Initialize history table  
  output$history_table <- renderDT({  
    history_data()  
  })  
  
  # Initialize gauge  
  output$gauge <- renderPlotly({  
    plot_ly(  
      type = "indicator",  
      mode = "gauge+number",  
      value = 0,  
      title = list(text = "Failure Probability"),  
      gauge = list(  
        axis = list(range = list(0, 1)),  
        bar = list(color = "darkblue"),  
        steps = list(  
          list(range = c(0, 0.3), color = "green"),  
          list(range = c(0.3, 0.7), color = "yellow"),  
          list(range = c(0.7, 1), color = "red")  
        ),  
        threshold = list(  
          line = list(color = "red", width = 4),  
          thickness = 0.75,  
          value = 0.5  
        )  
      )  
    )  
  })  
  
  # Prediction observer  
  observeEvent(input$predict, {  
    # Construct timestamp using date and time inputs  
    timestamp <- paste(input$date, paste(input$hour, input$minute, input$second, sep = ":"))  
    
    # Payload as expected by the API  
    payload <- list(data = list(  
      time_stamp = timestamp,  
      sensor_1 = input$sensor1,  
      sensor_2 = input$sensor2,  
      sensor_3 = input$sensor3  
    ))  
    
    # Debug info: payload and API URL  
    debug_text <- paste0("API URL: ", api_url, "\nPayload: ", toJSON(payload, auto_unbox = TRUE))  
    debug_msg(debug_text)  
    output$debug_info <- renderPrint({ cat(debug_msg()) })  
    
    # Call the API  
    res <- call_api(payload)  
    
    if (!is.null(res)) {  
      status <- status_code(res)  
      debug_text <- paste0(debug_text, "\nResponse Status: ", status)  
      
      if (status == 200) {  
        res_content <- fromJSON(content(res, as = "text", encoding = "UTF-8"))  
        debug_text <- paste0(debug_text, "\nResponse Body: ", toJSON(res_content, pretty = TRUE, auto_unbox = TRUE))  
        
        # Update gauge output with prediction probability  
        output$gauge <- renderPlotly({  
          plot_ly(  
            type = "indicator",  
            mode = "gauge+number",  
            value = res_content$probability[1],  
            title = list(text = "Failure Probability"),  
            gauge = list(  
              axis = list(range = list(0, 1)),  
              bar = list(color = "darkblue"),  
              steps = list(  
                list(range = c(0, 0.3), color = "green"),  
                list(range = c(0.3, 0.7), color = "yellow"),  
                list(range = c(0.7, 1), color = "red")  
              ),  
              threshold = list(  
                line = list(color = "red", width = 4),  
                thickness = 0.75,  
                value = 0.5  
              )  
            )  
          )  
        })  
        
        # Update prediction text  
        output$prediction_text <- renderText({  
          paste("Failure Prediction:", res_content$failure_prediction[1],   
                "\nProbability:", round(res_content$probability[1], 4))  
        })  
        
        # Update prediction history  
        new_entry <- data.frame(  
          DateTime = timestamp,  
          Sensor1 = input$sensor1,  
          Sensor2 = input$sensor2,  
          Sensor3 = input$sensor3,  
          Prediction = res_content$failure_prediction[1],  
          Probability = round(res_content$probability[1], 4),  
          stringsAsFactors = FALSE  
        )  
        
        history_data(rbind(history_data(), new_entry))  
        
        # Update result table  
        output$result_table <- renderDT({  
          new_entry  
        }, options = list(dom = "t"))  
        
        # Update history table  
        output$history_table <- renderDT({  
          history_data()  
        })  
      } else {  
        debug_text <- paste0(debug_text, "\nError in API response.")  
        output$prediction_text <- renderText("Error in API response.")  
      }  
      
      debug_msg(debug_text)  
      output$debug_info <- renderPrint({ cat(debug_msg()) })  
    } else {  
      debug_text <- paste0(debug_text, "\nError: Could not connect to server. Make sure the API is running.")  
      debug_msg(debug_text)  
      output$prediction_text <- renderText("Error: API request failed. Check debug info.")  
      output$debug_info <- renderPrint({ cat(debug_msg()) })  
    }  
  })  
  
  # Sensitivity analysis observer  
  observeEvent(input$analyze, {  
    # Create a progress bar  
    progress <- shiny::Progress$new()  
    progress$set(message = "Running sensitivity analysis", value = 0)  
    on.exit(progress$close())  
    
    # Generate sequence of sensor values  
    sensor_values <- seq(input$sensitivity_range[1], input$sensitivity_range[2], length.out = 20)  
    probabilities <- numeric(length(sensor_values))  
    
    # Update progress bar function  
    updateProgress <- function(value) {  
      progress$set(value = value)  
    }  
    
    # Run analysis for each sensor value  
    for (i in seq_along(sensor_values)) {  
      updateProgress(i / length(sensor_values))  
      
      temp_payload <- list(data = list(  
        time_stamp = paste(as.character(Sys.Date()), "12:00:00"),  
        sensor_1 = if (input$sensitivity_sensor == "sensor1") sensor_values[i] else input$sensor1,  
        sensor_2 = if (input$sensitivity_sensor == "sensor2") sensor_values[i] else input$sensor2,  
        sensor_3 = if (input$sensitivity_sensor == "sensor3") sensor_values[i] else input$sensor3  
      ))  
      
      res <- call_api(temp_payload)  
      
      if (!is.null(res) && status_code(res) == 200) {  
        res_content <- fromJSON(content(res, as = "text", encoding = "UTF-8"))  
        probabilities[i] <- res_content$probability[1]  
      } else {  
        probabilities[i] <- NA  
      }  
    }  
    
    # Create data frame for plotting  
    sensitivity_data <- data.frame(  
      sensor_value = sensor_values,  
      probability = probabilities  
    )  
    
    # Create plot  
    output$sensitivity_plot <- renderPlotly({  
      p <- plot_ly(sensitivity_data, x = ~sensor_value, y = ~probability,   
                   type = "scatter", mode = "lines+markers",  
                   line = list(color = "blue", width = 2),  
                   marker = list(color = "blue", size = 8)) %>%  
        layout(  
          title = paste("Effect of", gsub("sensor", "Sensor ", input$sensitivity_sensor), "on Failure Probability"),  
          xaxis = list(title = paste(gsub("sensor", "Sensor ", input$sensitivity_sensor), "Value")),  
          yaxis = list(title = "Failure Probability", range = c(0, 1)),  
          shapes = list(  
            list(  
              type = "line",  
              x0 = 0,  
              x1 = 1,  
              y0 = 0.5,  
              y1 = 0.5,  
              line = list(color = "red", dash = "dash")  
            )  
          )  
        )  
      
      return(p)  
    })  
  })  
}  

# Run the app  
shinyApp(ui, server)  

