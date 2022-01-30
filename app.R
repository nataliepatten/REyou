library(shiny)
library(ggmap)
library(leaflet)
library(bslib)
library(dplyr)
library(rsconnect)
library(shinyWidgets)

maths_list <- list(
  "Algebra",
  "Applied Mathematics",
  "Coding Theory",
  "Combinatorics",
  "Complex Analysis",
  "Cryptography",
  "Data Science",
  "Differential Equations",
  "Discrete Mathematics",
  "Dynamical Systems",
  "Financial Mathematics",
  "Game Theory",
  "Geometry",
  "Graph Theory",
  "Knot Theory",
  "Logic",
  "Machine Learning",
  "Mathematical Biology",
  "Number Theory",
  "Optimization",
  "Probability Theory",
  "Real Analysis",
  "Statistics",
  "Topology", 
  "Select all"
)
cs_list <- list(
  "Algorithm Development",
  "Artificial Intelligence",
  "Cyber Security",
  "Data Analytics",
  "Machine Learning",
  "Parallel Computing",
  "Virtual Reality",
  "Select all"
)
# nsf_maths <- read.csv("nsf_maths.csv")
# nsf_cs <- read.csv("nsf_cs.csv")
# nsf_maths <- mutate_geocode(nsf_maths, location = Institution, output = "latlona")
# nsf_cs <- mutate_geocode(nsf_cs, location = Institution, output = "latlona")
# write.csv(nsf_maths, "nsf_maths_loc.csv")
# write.csv(nsf_cs, "nsf_cs_loc.csv")


nsf_maths <- read.csv("nsf_maths_loc.csv")
nsf_cs <- read.csv("nsf_cs_loc.csv")

ui <- navbarPage(theme = bs_theme(bg = "#2b3e50", fg = "#FFFFFF", primary = "#b87d9a",
                                  base_font = font_google("Prompt"),
                                  code_font = font_google("Open Sans")), title = "REyou", 
  tabPanel("Start Here", 
           h1("Welcome to REyou!", style = "font-size: 40px; font-family: fantasy; color: #5fb08c"),
           p(em("REyou makes finding NSF REU programs easier for YOU. 
           Please select your desired subject area and topic. Then, browse the map to look for REU programs that meet your critieria in 
           a particular region. Hover over a point to view the institution and click on a point to see the link to the REU's website.
           You can also click on the 'Your Search Information' tab to view additional details about REU programs that fulfill your criteria.",
                style = "font-size: 18px; font-family: Open Sans; color: #b87d9a")),
           sidebarLayout(
             sidebarPanel(selectInput(inputId = "subject", label = "Subject Area", choices = list("Select", "Mathematical Sciences", "Computer and Information Science and Engineering")),
               uiOutput(outputId = "choose_subject"),
               #submitButton("Submit")
               ),
             mainPanel(uiOutput("leaflet"))), style = 'height: 1000px'
  ),
  
  tabPanel("Your Search Information", 
           h1("Thank you for using REyou!", style = "font-size: 40px; font-family: fantasy; color: #5fb08c"),
           div(verbatimTextOutput(outputId = "topic_info"), style = "font-size: 18px;"),
           div(tableOutput(outputId = "my_info"), style = "font-family: Sans-serif")
  ),
)

server <- function(input, output) {
  icons_calc <- awesomeIcons(icon = "fa-calculator", iconColor = "pink", library = "fa", squareMarker = TRUE, markerColor = "darkpurple")
  icons_comp <-awesomeIcons(icon = "fa-desktop", iconColor = "pink", library = "fa", squareMarker = TRUE, markerColor = "darkpurple")
  output$leaflet <- renderUI({leafletOutput("myMap", width = "75%", height = 450)})
  
  re <- eventReactive(input$subject,{input$subject})
  output$choose_subject <- renderUI(
    if (re() == "Mathematical Sciences") 
      tagList(awesomeCheckboxGroup(inputId = "topic", label = "Specific Topic", choices = maths_list), actionButton(inputId = "submitted", label = "Submit"))
    else if (re() == "Computer and Information Science and Engineering")
      tagList(awesomeCheckboxGroup(inputId = "topic", label = "Specific Topic", choices = cs_list), actionButton(inputId = "submitted", label = "Submit"))
    )
  
  re_topic <- eventReactive(input$topic, {input$topic})
  
  re2 <- reactive({
    if (input$subject != "Select") {
      string <- paste0("Selected subject: ", input$subject)
      string <- paste(string, " ", sep = "\n")
      
      if (length(input$topic) > 0) {
        string <- paste(string, "Selected topic(s): ", sep = "\n")
        if ("Select all" %in% re_topic())
          string <- paste(string, "All topics", sep = "\n")
        else
          for (i in 1:length(input$topic)) {
            string <- paste(string, input$topic[i], sep = "\n")
          }
      }
      string
    }
    else
      paste0("You have not selected a subject yet. Please select a subject under the 'Start Here' tab to get started.")
  })
  output$topic_info <- renderText({re2()})
  
  
  re_text <- reactive({
    if (re() == "Mathematical Sciences")
      file <- nsf_maths
    else if (re() == "Computer and Information Science and Engineering")
      file <- nsf_cs

    if ("Select all" %in% re_topic()) 
      data <- file
    else {
      data <- data.frame(matrix(ncol = ncol(file), nrow = nrow(file)))    
      colnames(data) <- colnames(file)
      for (i in 1:nrow(file)) {
        for (j in 1:length(input$topic)) {
          if (grepl(input$topic[j], file$Research.Topics.Keywords[i], ignore.case = TRUE)) {
            data[i, ] <- file[i, ]
          }
        }
      }
    }

    rows <- c()
    for (i in 1: nrow(data)) {
      indices <- which(is.na(data[i, ]))
      if (length(indices) == ncol(data))
        rows <- append(rows, i)
    }
    if (length(rows) > 0)
      data <- data[-rows, ]
    data <- select(data, Institution, Site.Name, Site.URL, City, State, Contact.Name, Contact.Phone, Contact.Email, Research.Topics.Keywords, Award)
    
    #https://stackoverflow.com/questions/21909826/r-shiny-open-the-urls-from-rendertable-in-a-new-tab
    refs <- paste0("<a href='",  data$Site.URL, "' target='_blank'>LINK</a>")
    data$Site.URL <- data.frame(refs)
    refs <- paste0("<a href='",  data$Award, "' target='_blank'>LINK</a>")
    data$Award <- data.frame(refs)
    
    if (re() == "Mathematical Sciences" || re() == "Computer and Information Science and Engineering")
      data
    else if (re() == "Select")
      data <- colnames(data)
  })
  output$my_info <- renderTable({re_text()}, na = "", sanitize.text.function = function(x) x)
  
  re_submit <- eventReactive(input$submitted, {TRUE})
  #https://stackoverflow.com/questions/38713809/how-to-dynamically-change-the-size-of-leaflet-map-in-shiny-r
  output$myMap <- renderLeaflet({
    if (re() == "Mathematical Sciences" & re_submit()) {
      if (all(re_topic() %in% maths_list) & !("Select all" %in% re_topic())) {
        file <- nsf_maths
        data <- data.frame(matrix(ncol = ncol(file), nrow = nrow(file)))
        colnames(data) <- colnames(file)
        for (i in 1:nrow(file)) {
          for (j in 1:length(input$topic)) {
            if (grepl(input$topic[j], file$Research.Topics.Keywords[i], ignore.case = TRUE)) {
              data[i, ] <- file[i, ]
            }
          }
        }
        leaflet(data) %>%
          leaflet::addProviderTiles(providers$OpenStreetMap) %>%
          leaflet::addAwesomeMarkers(lng = ~lon, lat = ~lat, icon = icons_calc, popup = ~Site.URL, label = ~Institution) %>%
          leaflet::addMiniMap(toggleDisplay = TRUE)
      }
      else if ("Select all" %in% re_topic() & all(re_topic() %in% maths_list)) {
        leaflet(nsf_maths) %>%
              leaflet::addProviderTiles(providers$OpenStreetMap) %>%
              leaflet::addAwesomeMarkers(lng = ~lon, lat = ~lat, icon = icons_calc, popup = ~Site.URL, label = ~Institution) %>%
              leaflet::addMiniMap(toggleDisplay = TRUE)
      }
      else {
        leaflet() %>%
          leaflet::addProviderTiles(providers$OpenStreetMap) %>%
          leaflet::setView(-98.5795, 39.8283, zoom = 4) %>%
          leaflet::addMiniMap(toggleDisplay = TRUE)
      }
    }
    else if (re() == "Computer and Information Science and Engineering" & re_submit()) {
      if (length(re_topic()) > 0 & all(re_topic() %in% cs_list) & !("Select all" %in% re_topic())) {
        file <- nsf_cs
        data <- data.frame(matrix(ncol = ncol(file), nrow = nrow(file)))
        colnames(data) <- colnames(file)
        for (i in 1:nrow(file)) {
          for (j in 1:length(input$topic)) {
            if (grepl(input$topic[j], file$Research.Topics.Keywords[i], ignore.case = TRUE)) {
              data[i, ] <- file[i, ]
            }
          }
        }
        leaflet(data) %>%
          leaflet::addProviderTiles(providers$OpenStreetMap) %>%
          leaflet::addAwesomeMarkers(lng = ~lon, lat = ~lat, icon = icons_comp, popup = ~Site.URL, label = ~Institution) %>%
          leaflet::addMiniMap(toggleDisplay = TRUE)
      }
      else if ("Select all" %in% re_topic() & all(re_topic() %in% cs_list)) {
        leaflet(nsf_cs) %>%
          leaflet::addProviderTiles(providers$OpenStreetMap) %>%
          leaflet::addAwesomeMarkers(lng = ~lon, lat = ~lat, icon = icons_comp, popup = ~Site.URL, label = ~Institution) %>%
          leaflet::addMiniMap(toggleDisplay = TRUE)
      }
      else {
        leaflet() %>%
          leaflet::addProviderTiles(providers$OpenStreetMap) %>%
          leaflet::setView(-98.5795, 39.8283, zoom = 4) %>%
          leaflet::addMiniMap(toggleDisplay = TRUE)
      }
    }
        
    else {
      leaflet() %>%
        leaflet::addProviderTiles(providers$OpenStreetMap) %>%
        leaflet::setView(-98.5795, 39.8283, zoom = 4) %>%
        leaflet::addMiniMap(toggleDisplay = TRUE)
    }
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
