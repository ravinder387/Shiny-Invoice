library(shiny)
library(bslib)
library(httr)
library(jsonlite)
library(tidyverse)
library(rhandsontable)

# grocery dataset
grocery <- read_csv("BigBasket.csv")
grocery <- grocery |>
  rename(Items = Item,
         Price = `Average Price (INR per kg or unit)`) |>
  mutate(Qty = 0, Amount = 0)

# LLM api
url <- "https://api.mistral.ai/v1/chat/completions"


ui <- page_navbar( 
  title = "Shiny Invoice System", 
  nav_panel("Camera", 
            actionButton("startCamera", "Start Camera", class = "btn-primary w-100 mb-2"),
            div(
              style = "width: 100%; text-align: center;",
              tags$div(id = "camera-container", style = "width: 100%; max-width: 600px; margin: 0 auto;"),
              tags$video(
                id = "camera", 
                width = "100%", 
                autoplay = NA, 
                playsinline = NA, 
                style = "display: none; max-width: 100%; border-radius: 8px; margin-bottom: 10px;"
              )
            ),
            actionButton("capture", "Capture Photo", class = "btn-primary w-100", disabled = TRUE),
            card(
              card_header("Captured Photo"),
              div(
                style = "width: 100%; text-align: center;",
                uiOutput("capturedPhoto")
              )
            ),
            actionButton("send", "Send Photo", class = "btn-success w-100")
            ), 
  nav_panel("Invoice", 
            card(
              card_header(
                "Mini Spreadsheet",
                div(
                  style = "float: right;",
                  actionButton("add_row", "Add Row", class = "btn-primary btn-sm")
                )
              ),
              card_body(
                rHandsontableOutput("hot")
              ),
              card_footer(
                div(
                  h4("Summary:"),
                  div(
                    strong("Total Amount: "), 
                    textOutput("total_amount", inline = TRUE)
                  ),
                  div(
                    strong("Items: "), 
                    textOutput("items_summary", inline = TRUE)
                  )
                )
              )
            )
            ), 
  nav_panel("Sales", uiOutput("test")),
  nav_panel("Purchase", "Purchase content"), 
  nav_panel("Debtors", "Debtors content"), 
  nav_panel("Creditors", "Creditors content"), 
  navbar_options =  navbar_options(position = "static-top"),
  tags$head(
    includeScript("www/myscript.js"),
    includeCSS("www/style.css")
  )
) 

# Define server logic
server <- function(input, output, session) {
  # Create a reactive value to store the dynamically generated dataframe
  values <- reactiveVal(NULL)
  # Enable/disable capture button based on camera status
  observeEvent(input$cameraActive, {
    if (!is.null(input$cameraActive)) {
      updateActionButton(session, "capture", 
                         disabled = !input$cameraActive)
    }
  })
  
  # Status message
  output$status <- renderText({
    if (!is.null(input$cameraStatus)) {
      return(input$cameraStatus)
    } else {
      return("Camera not started")
    }
  })
  
  # Display captured image
  output$capturedPhoto <- renderUI({
    req(input$capturedImage)
    
    tags$div(
      tags$img(
        src = input$capturedImage,
        width = "100%",
        style = "max-width: 600px; border-radius: 8px; margin: 0 auto;"
      )
    )
  })
  
  # Send captured photo to LLM
  observeEvent(input$send, {
    # Convert base64 to binary and save
    if (!is.null(input$capturedImage)) {
      # Extract the actual base64 data (remove the data:image/jpeg;base64, prefix)
      # base64Data <- sub("^data:image/jpeg;base64,", "", input$capturedImage)
      mistral_rest <- toJSON(list(
        model = "pixtral-12b-2409",
        messages = data.frame(
          role = "user",
          content = I(list(
            data.frame(
              type = c("text", "image_url"),
              text = c("Extract grocery items's names from the image and names should in R vector format for ex:- c('Apples', 'Onions').Make sure item's name is in camelcase and Return only R vector", NA),
              image_url = c(NA, input$capturedImage )
            )
          )
          )),
        max_tokens = 100
      ), auto_unbox = T, pretty = T)
      # headers
      headers <- c(`Content-Type` = 'application/json',
                   Accept = 'application/json',
                   Authorization = paste("Bearer",Sys.getenv("MISTRAL_API_KEY"))
      )
      
      # post request
      req <- POST(url, add_headers(headers), body = mistral_rest)
      
      # response
      res <- content(req, type = "text", encoding = "UTF-8")
      mistral_response <- fromJSON(res)
      content <- mistral_response$choices$message$content
      # Extract vector part using a regex
      vector_code <- regmatches(content, regexpr("c\\([^)]+\\)", content))
      # Evaluate the extracted code to get the actual vector
      r_vector <- eval(parse(text = vector_code))
      result <- tibble(
        Items = character(0),
        Price = 0,
        Qty = 0,
        Amount = 0
      )
      for(i in 1:length(r_vector)){
        tbl_filtered <- grocery %>%
          filter(grepl(paste0("^",r_vector[i]), Items))
        result <- bind_rows(result,tbl_filtered)
      }
      output$test <- renderText({
        result
      })
      # Initialize reactive values for the data
      values(result)
      # Render the dynamic table when data is available
      output$hot <- renderRHandsontable({
        # Check if we have data yet
        req(values())
        
        # Render the table with the current data
        rhandsontable(values(),
                      stretchH = "all",
                      height = 300,
                      width = "100%") %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      })
      }
  })

  # Calculate initial amounts
  observe({
    data <- values()
    data$Amount <- data$Price * data$Qty
    values(data)
  })
  
  
  
  # Update the data when the table is edited
  observeEvent(input$hot, {
    hot_data <- hot_to_r(input$hot)
    hot_data$Amount <- hot_data$Price * hot_data$Qty
    values(hot_data)
  })
  
  # Add a new row
  observeEvent(input$add_row, {
    data <- values()
    new_row <- data.frame(
      Items = "item",
      Price = 0,
      Qty = 0,
      Amount = 0,
      stringsAsFactors = FALSE
    )
    new_row$Amount <- new_row$Price * new_row$Qty
    values(rbind(data, new_row))
  })
  
  
  # Calculate total amount
  output$total_amount <- renderText({
    data <- values()
    sprintf("Rs%.2f", sum(data$Amount, na.rm = TRUE))
  })
  
  # Create items summary text
  output$items_summary <- renderText({
    data <- values()
    if(nrow(data) != 0){
      items_with_qty <- sapply(1:nrow(data), function(i) {
        paste0(data$Items[i], " (", data$Qty[i], ")")
      })
      paste(items_with_qty, collapse = ", ")
    } else {
      "No item found"
    }

  })
}

shinyApp(ui = ui, server = server)