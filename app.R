library(shiny)
library(leaflet)
library(compiler)
library(shinythemes)
library(stringr)

enableJIT(3)

# global functions for chat
vars <- reactiveValues(chat=NULL, users=NULL)
# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome to The Shipping Optimizer!"
}
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

# #read data
node_coords <- readRDS("data/node_coords.rds")
routes <- readRDS("data/routes.rds")
paths <- readRDS("data/paths.rds")
ports <- readRDS("data/ports.rds")
trucks <- readRDS("data/trucks.rds")

ui <- bootstrapPage(theme = shinytheme("united"),
  titlePanel(
    tags$img(src="headerImg.jpg", width="100%"),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "cartoon-ship-vector-clipart.png"), tags$title("Optimize Your Shipping Routes"))
  # windowTitle = "Optimize Your Shipping Routes"
  ),
  
  includeCSS("www/shinychat.css"),
  includeScript("sendOnEnter.js"),
  div(
    class = "container-fluid", 
    div(
      class = "row-fluid", 
      sidebarPanel(
        p("IP addresses are logged, be nice!"),
        uiOutput("chat"),
        
        fluidRow(
          div(class="span10",
              textInput("entry", "")
          ),
          div(class="span2 center",
              actionButton("send", "Send")
          )
        ),
        textInput("user", "Your User ID:", value=""),
        tags$hr(),
        h5("Connected Users"),
        # Create a spot for a dynamic UI containing the list of users.
        uiOutput("userList")
      )
    ),
    
    mainPanel(
      uiOutput("mainui")
    )
  )                            
)

server <- function(input, output, session) {
  
  sessionVars <- reactiveValues(username = "")
  init <- FALSE
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
      vars$chat <- c(vars$chat, paste0(linePrefix(),
                                       tags$span(class="user-exit",
                                                 sessionVars$username,
                                                 "left the room.")))
    })
  })
  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user
    
    if (!init){
      # Seed initial username
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-enter",
                                                    sessionVars$username,
                                                    "entered the room.")))
      })
      init <<- TRUE
    } else{
      # A previous username was already given
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          # No change. Just return.
          return()
        }
        
        # Updating username      
        # First, remove the old one
        vars$users <- vars$users[vars$users != sessionVars$username]
        
        # Now update with the new one
        sessionVars$username <- input$user
      })
    }
    # Add this user to the global list of users
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })
  
  # Keep the username updated with whatever sanitized/assigned username we have
  observe({
    updateTextInput(session, "user", 
                    value=sessionVars$username)    
  })
  
  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })
  
  # Listen for input$send changes (i.e. when the button is clicked)
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
      # Add the current entry to the chat log.
      vars$chat <<- c(vars$chat, 
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), sessionVars$username)
                             ),
                             ": ",
                             tagList(input$entry)))
    })
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
  })
  
  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(vars$chat) > 500){
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")
    
    # Pass the chat log through as HTML
    HTML(vars$chat)
  })
  
  
  front_display <- function() {
    output$mainui <- renderUI({
      tagList(
        absolutePanel(
          h2("Enter your shipment details"),
          selectInput("origin_location", choices=node_coords$V1, label="Origin"),
          selectInput("destination_location", choices=node_coords$V1,label="Destination"),
          dateInput("departure_date", "Date of Departure:", format = "dd/mm/yy"),
          dateInput("arrival_date", "Date of Arrival:", format = "dd/mm/yy"),         
          br(),
          numericInput("cargo_weight", "Cargo Weight in Tonnes:", value=""),
          numericInput("cargo_volume", "Cargo Volume in CBM:", value=""),
          numericInput("cargo_value", "Cargo Value in USD:", value=""),
          selectInput("cargo_type", choices=c("","Apparel", "Chemical", "Electrical", "Food"), label="Type Of Cargo:"),
          numericInput("order_qty", "Order Quantity of Shipment:", value=""),
          actionButton("submit_button", "Submit"),
          br(),
          br(),
          
          left = "10%"
        )
      )
    })
  }
  
  front_display()
  observeEvent(input$back_button, {front_display()})
  observeEvent(input$back_button2, {front_display()})
   
  observeEvent(input$submit_button, {
    if(input$cargo_weight == "" | input$cargo_volume == "" | input$cargo_value == "" | input$order_qty == "") {
      showNotification("You have not filled up your form completely!")
    } else if(input$origin_location == input$destination_location) {
      showNotification("Your origin and destination is the same!")
    }
    else {
      duration <- reactive({
        as.numeric(difftime(as.Date(input$arrival_date),as.Date(input$departure_date), units="hours"))
      })
      
      withProgress(message = 'Optimizing your routes',
                detail = 'This may take a while...', value = 1, {
      
        source("optimize.R")            
        optimized_result <- optimize(node_coords[V1 == input$origin_location]$V2,
                                    node_coords[V1 == input$destination_location]$V2,
                                    duration(), input$cargo_value, input$cargo_weight,
                                    input$cargo_volume, input$order_qty)
    
        
        cost = as.character(optimized_result[1])
        days = format(round((as.numeric(optimized_result[2])/24), 1), nsmall = 1)
        path = unlist(optimized_result[3])
        colour_of_path = unlist(optimized_result[4])
        
        if(path == "") {
          output$mainui <- renderUI({
            tagList(
              br(),
              br(),
              h2("Your departure date and arrival date are too close to each other!"),
              br(),
              h3("There are no routes that can meet your deadline. Please try again."),
              br(),
              actionButton("back_button2", "Back"),
              br()
            )
          })  
        }
        
        else {
          
          ######################################################################
          #show path breakdown
          ###################################################
          num_of_40_containers = floor(input$cargo_volume/67)
          num_of_20_containers = ceiling((input$cargo_volume %% 67)/33)
          path_details <- as.data.frame(path)
          rownum = nrow(path_details)
          path_details <- as.data.frame(cbind(as.character(path_details[1:(rownum-1),]), 
                                              as.character(path_details[2:rownum,])))
          
          costlist <- c()
          durationlist <- c()
          
          for(p in 1:(rownum-1)) {
            currrow = path_details[p,]
            currrow$V1 <- as.character(currrow$V1)
            currrow$V2 <- as.character(currrow$V2)
            rowmatch = paths[which(paths$from_name == currrow$V1 & paths$to_name == currrow$V2),]
            
            if(nrow(rowmatch) == 0) {
              dur = trucks[which(trucks$X == currrow$V1),][,as.character(currrow$V2)]
              currcost = 6.9463*input$cargo_volume*dur
              currduration = dur
              
              nextnode = ports[which(ports$name == currrow$V2),]
              if(ports[which(ports$name == currrow$V1),]$country != nextnode$country) {
                currcost = currcost + input$cargo_value*nextnode$tax
              }
            }
            else {
              currcost = 
                rowmatch$cost_air*50*input$cargo_volume + 
                rowmatch$cost_train_20*33*num_of_20_containers +
                rowmatch$cost_train_40*67*num_of_40_containers +
                rowmatch$cost_ship_20*33*num_of_20_containers +
                rowmatch$cost_ship_40*67*num_of_40_containers
              
              currduration = rowmatch$duration
              
              if(rowmatch$to_country != rowmatch$from_country) {
                currcost = currcost + input$cargo_value*ports[which(ports$name == currrow$V2),]$tax
              }
            }
            
            newrow = ports[which(ports$name == currrow$V2),]
            if(nrow(newrow) == 0) {next} else {
              currcost = currcost + newrow$handling_cost*input$cargo_weight + newrow$custom_cost*input$order_qty
              currduration = currduration + newrow$handling_duration + newrow$custom_duration
            }
            
            costlist = c(costlist, currcost)
            durationlist = c(durationlist, currduration)
          }
          
          for(i in 1:nrow(path_details)) {
            path_details$From[i] = node_coords[node_coords$V2 == path_details$V1[i]]$V1
            path_details$To[i] = node_coords[node_coords$V2 == path_details$V2[i]]$V1
          }
          path_details$cost = costlist
          path_details$duration = format(as.numeric(round(durationlist/24, 1)), nsmall = 1)
          path_details = path_details[,3:6]
          names(path_details) <- c("From", "To", "Cost(USD)", "Duration(Days)")
          
          output$mytable <- renderTable(path_details)
          ###########################################################################
          ###########################################################################
          
          output$total_cost <- renderText({ cost })
          output$hours_taken <- renderText({ paste(days,"days") })
          source("map.R")
          output$map <- renderLeaflet(mapFunction(path, colour_of_path))
          
          output$mainui <- renderUI({
            tagList(
                br(),
                leafletOutput("map"),
                br(),
                absolutePanel(
                  h2("Here's your optimized path!"),
                  br(),
                  h4("Total cost (USD):"),
                  textOutput("total_cost"),
                  br(),
                  h4("Number of days required:"),
                  textOutput("hours_taken"),
                  br(),
                  h4("Breakdown:"),
                  tableOutput("mytable"),
                  actionButton("back_button", "Back"),
                  br(),
                  br()
                )
            )
          })
        }
      })
    }
  })
   
}

shinyApp(ui = ui, server = server)