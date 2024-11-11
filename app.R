library(shiny)
library(shinydashboard)
library(fmsb)
library(dplyr)
library(plotly)
library(treemapify)
library(ggplot2)
library(googleway)
library(leaflet)
library(leaflet.extras)
library(DT)
library(shinythemes)
library(shinyalert)

api_key <- "Put your own Google Map API key"
hotel <- read.csv("https://raw.githubusercontent.com/ChunHan519/FindMyHotel/main/hotel.csv")

history <- sample_n(hotel, 2)
history <- select(history,-1,-c("Latitude","Longitude"))
Check_in_Date <- c(Sys.Date() - 15, Sys.Date() - 5)
Check_out_Date <- c(Sys.Date() - 12, Sys.Date() - 1)
Total_Price <- c((history[1,]$Price * as.numeric(Check_out_Date[1] - Check_in_Date[1])),
                 history[2,]$Price * as.numeric(Check_out_Date[2] - Check_in_Date[2]))

history <- cbind(history, Check_in_Date, Check_out_Date, Total_Price)

ui <- dashboardPage(
  dashboardHeader(title = "Find My Hotel"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Find Hotel", tabName = "findhotel", icon = icon("dashboard")),
      menuItem("Booking History", tabName = "bookinghistory", icon = icon("table")),
      menuItem("Hotel Statistic", tabName = "hotelstatistic", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "findhotel",
              fluidPage( theme = shinytheme("flatly"),
                         titlePanel("Find All Malaysia Hotels Here!"),
                         mainPanel(
                           tabsetPanel(id = "mainPanel", 
                                       tabPanelBody(value = "searchPanel",
                                                    fluidRow(
                                                      sidebarPanel(
                                                        style = "background-color: orange",
                                                        width = 4,
                                                        selectInput(inputId = "inputState", label = "Select state:", multiple = FALSE, choices = sort(hotel$State), selected = "Wilayah Persekutuan Kuala Lumpur"),
                                                        sliderInput(inputId = "priceRange",
                                                                    label = h3("Price Range:"),
                                                                    min = min(hotel$Price),
                                                                    max = max(hotel$Price),
                                                                    value = c(min(hotel$Price), max(hotel$Price))),
                                                        selectInput(inputId = "inputRating", label = "Choose your rating:", multiple = FALSE, choices = sort(hotel$Rating), selected = "5"),
                                                        actionButton("search", "Search")
                                                      ),
                                                      mainPanel(
                                                        HTML("<div style='width: 150%'>"),
                                                        width = 8,
                                                        google_mapOutput(outputId = "search_map"),
                                                        HTML("</div>")
                                                      )
                                                    ),
                                                    fluidRow(
                                                      br(),
                                                      HTML("<div style='width: 125%;'>"),
                                                      textOutput(outputId = "title1"),
                                                      br(),
                                                      dataTableOutput(outputId = "table"),
                                                      HTML("</div>")
                                                    )
                                       ),
                                       tabPanelBody(value = "hotelDetail",
                                                    h2("Hotel Details"),
                                                    fluidRow(
                                                      column(
                                                        width = 4,
                                                        tableOutput(outputId = "display_details"),
                                                        uiOutput(outputId = "photo")
                                                      ),
                                                      column(
                                                        HTML("<div style='width: 150%'>"),
                                                        width = 8,
                                                        google_mapOutput(outputId = "locate_hotel"),
                                                        HTML("</div>")
                                                      )    
                                                    ),
                                                    fluidRow(
                                                      br(),
                                                      column(
                                                        align = "center",
                                                        width = 6,
                                                        actionButton("book", "Book Now")
                                                      ),
                                                      column(
                                                        align = "center",
                                                        width = 6,
                                                        actionButton("back","Back")
                                                      ),
                                                    )
                                       ),
                                       
                                       tabPanelBody(value = "bookingPage",
                                                    fluidRow(
                                                      mainPanel(
                                                        align = "center",
                                                        style = "background-color: white;
                                                        border: 1px double lightgrey;",
                                                        br(),
                                                        h2("Booking Page"),
                                                        br(),
                                                        helpText("This is the booking page."),
                                                        helpText("Kindly fill in your personal information to make a booking."),
                                                        textInput(inputId = "name", label = h3("Name")),
                                                        textInput(inputId = "phonenumber", label = h3("Contact Number")),
                                                        dateInput(inputId = "datein", label = h3("Check-in Date")),
                                                        dateInput(inputId = "dateout", label = h3("Check-out Date"), value = Sys.Date() + 1),
                                                        h3("Total Price"),
                                                        tableOutput("totalprice"),
                                                        textInput(inputId = "creditcard", label = h3("Credit card info")),
                                                        br(),
                                                        column(
                                                          width =6,
                                                          actionButton("pay", "Pay")
                                                        ),
                                                        column(
                                                          width =6,
                                                          actionButton("back2","Back")
                                                        ),
                                                        h2(".")
                                                      )
                                                    )
                                       )
                                       
                           ), type = "hidden"
                         )
              )
      ),
      tabItem(tabName = "bookinghistory",
              titlePanel("Booking History"),
              actionButton("checkhistory", "Check History"),
              br(),
              fluidRow(
                box(
                  width = 12,
                  DT::dataTableOutput(outputId = "display_history")
                )
                
              )
              
      ),
      tabItem(tabName = "hotelstatistic",
          fluidRow(
            box(
              "The circular barplot describes the number of hotels in each state in Malaysia.",
              title="Circular Barplot",
              width=12,
              plotOutput(outputId="visual1")
          ),
            br(),
            box(
              "The tree map shows the ratio between hotels with different rating in Malaysia.",
              title="Tree Map",
              width=12,
              plotOutput(outputId="visual2")
            ),
            box(
              "The heat map visualise the density of hotel location distribution in Malaysia.",
              title="Heat Map",
              width=12,
              leafletOutput(outputId="visual3")
            ),
            box(
              "The box plot describes the price of hotels by its rating in Malaysia.",
              title="Box Plot",
              width=12,
              plotOutput(outputId="visual4")
            )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    hotel %>%
      filter(State%in% input$inputState) %>% filter(Price >= input$priceRange[1] & Price <= input$priceRange[2])  %>%
      filter(Rating %in% input$inputRating) %>%
      mutate(INFO = paste0(Hotel_Name, " | ", Tel_No, " | ", Rating))
  })
  
  search_result <- reactive({
    select(data(),ID,Hotel_Name,Address,Tel_No,Price)
  })
  
  hotel_selected <- reactive({
    hotel_selected <- filter(data(), ID %in% search_result()[input$table_rows_selected,]$ID)
  })
  
  booking_detail <- reactive({
    Check_in_Date <- as.Date(input$datein)
    Check_out_Date <- as.Date(input$dateout)
    Total_Price <- select(hotel_selected(), Price) * 
      as.numeric(difftime(as.Date(input$dateout), as.Date(input$datein)))
    hotel_selected <- select(hotel_selected(),-1,-c("Latitude","Longitude","INFO"))
    newRow <- c(hotel_selected, as.character(Check_in_Date), as.character(Check_out_Date), Total_Price)
  })
  
  output$visual1<-renderPlot({
    
    df_grp_region <- hotel %>% group_by(State) %>%
      summarise(State_Count = length(which(State == State)),
                State = unique(State),
                .groups = 'drop')
    
    p <- ggplot(df_grp_region, aes(x = State, y = State_Count), fill = State_Count) +
      geom_bar(stat = "identity", color = "white",
               lwd = 1, show.legend = FALSE)+
      labs(title = "Circular Barplot of Number of Hotels by State", x = 'State', y = "Number of Hotels")
    
    
    p + coord_polar()
    
  })
  
  output$visual2<-renderPlot({
    df_rate = hotel %>% group_by(Rating) %>%
      summarise(Count = length(which(Rating == Rating)),
                .groups = 'drop')
    
    ggplot(df_rate, aes(area = Count, fill = Rating, label = Rating)) +
      geom_treemap() +
      geom_treemap_text(fontface = "bold", colour = "white", place = "center",
                        grow = TRUE)
    
  })
  
  output$visual3<-renderLeaflet({
    
    leaflet(hotel) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView( lat = 3.1390, lng = 101.6869, 5) %>%
      addHeatmap(
        lng = ~Longitude, lat = ~Latitude ,
        blur = 20, max = 0.05, radius = 15
      )
    
  })
  
  output$visual4 <- renderPlot({
    df_vis <- hotel
    df_vis$Rating <- as.factor(df_vis$Rating)
    p <- ggplot(df_vis, aes(x=Rating, y=Price, fill = Rating)) +
      geom_boxplot() +
      labs(title = "Box Plot of Price by Rating")+
      theme_classic()
    p
  })
  
  output$search_map <- renderGoogle_map({
    if(input$search >0){
      isolate(google_map(data = data(), key = api_key) %>%
                add_markers(lat = "Latitude", lon = "Longitude", mouse_over = "INFO"))
    }
  })
  
  output$title1 <- renderText({
    if(input$search>0){
      isolate("Search Result (Click on the hotel in the table to show more details.)")
    }
  })
  
  output$table <- DT::renderDataTable({
    if(input$search>0){
      isolate(search_result())
    }
  }, selection  = "single")
  
  output$locate_hotel <- renderGoogle_map({
    google_map(data = hotel_selected(),key=api_key) %>% add_markers(lat=hotel_selected()$Latitude, lon = hotel_selected()$Longitude)
  })
  
  output$display_history <- DT::renderDataTable({
    if(input$checkhistory){
      isolate(datatable(history))
    }
  }, selection = "none")
  
  observeEvent(input$back,{
    if(input$back>0){
      updateTabsetPanel(session, inputId = "mainPanel",selected="searchPanel")
      google_map_update(map_id = "search_map") %>% clear_markers()
      google_map_update(map_id = "search_map") %>% add_markers(data = data(), lat="Latitude" , lon = "Longitude", mouse_over = "INFO")
    }
  })
  
  
  observeEvent(input$table_rows_selected,{
    hotel_selected <- select(hotel_selected(),-1,-c("Latitude","Longitude","INFO"))
    photo <- paste0('https://www.google.com/search?q=',chartr(" ", "+", hotel_selected$Hotel_Name),'&sxsrf=ALiCzsaDy1-GnDQzXQUc1jtKkE9RuPw27Q:1654709916616&source=lnms&tbm=isch&sa=X&ved=2ahUKEwi846Tmsp74AhW07zgGHTNfDVAQ_AUoAnoECAIQBA&biw=1707&bih=948&dpr=1.5')
    photo <- a("View Image", href= photo,target="blank")
    hotel_selected <- t(hotel_selected)
    output$display_details <- renderTable(hotel_selected, striped= TRUE,rownames = TRUE, colnames = FALSE)
    output$photo <- renderUI(photo)
    updateTabsetPanel(session, inputId = "mainPanel", selected = "hotelDetail")
  })
  
  observeEvent(input$search,{
    if(input$search>0){
      isolate({
        google_map_update(map_id = "search_map") %>% clear_markers()
        google_map_update(map_id = "search_map") %>% add_markers(data= data(), lat = "Latitude", lon = "Longitude", mouse_over = "INFO")
      })
    }  
  })
  
  observeEvent(hotel_selected(),{
    df <- data.frame(lat = hotel_selected()$Latitude, lon = hotel_selected()$Longitude, Info = hotel_selected()$INFO)
    if(nrow(df) == 1){
      google_map_update(map_id = "locate_hotel") %>% clear_markers()
      google_map_update(map_id = "locate_hotel") %>% add_markers(data = df)
    }
  })
  
  observeEvent(input$book,{
    if(input$book>0){
      updateTabsetPanel(session, inputId = "mainPanel",selected="bookingPage")
      totalprice <- reactive({
        ifelse(select(hotel_selected(), Price) * as.numeric(difftime(as.Date(input$dateout), as.Date(input$datein))) > 0,
               select(hotel_selected(), Price) * as.numeric(difftime(as.Date(input$dateout), as.Date(input$datein))), 0
        )
      })
      output$totalprice <- 
        renderTable(paste("RM",as.numeric(totalprice())), colnames = FALSE, bordered = TRUE)
    }
  })
  
  observeEvent(input$back2,{
    if(input$back2>0){
      updateTabsetPanel(session, inputId = "mainPanel",selected="hotelDetail")
    }
  })
    
  observeEvent(input$pay,{
    if(input$pay>0){
      if(nchar(input$name) == 0 | nchar(input$phonenumber) == 0 | nchar(input$creditcard) == 0){
        shinyalert::shinyalert(title = "Missing data",
                               text = "Please key in all the information needed",
                               type = "error")
      }
      else if(input$dateout <= input$datein | input$datein < Sys.Date()){
        shinyalert::shinyalert(title = "Invalid date input",
                               text = "Please enter the correct check in and check out date",
                               type = "error")
      }
      else{
        newRow <- data.frame(booking_detail())
        colnames(newRow) <- colnames(history)
        history <<- rbind(history, newRow)
        shinyalert::shinyalert(title = "Payment Successful!", 
                               text = "Thank you! Redirecting you back to the main page...", 
                               type = "success")
        updateTabsetPanel(session, inputId = "mainPanel", selected = "searchPanel")
      }
    }
  })

}

shinyApp(ui = ui, server = server)
