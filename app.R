library(shiny)
library(httr2)
library(bslib)
library(shiny)
library(shinythemes)
library(bsicons)
library(leaflet)
library(htmltools)
library(dplyr)
library(ggplot2)
library(ggimage)
library(glue)
library(thematic)

thematic::thematic_shiny()

# Define UI for the application
ui <- fluidPage(
  tags$head(
    includeCSS("./www/styles.css")
  ),
  # JS to fade in the main panel data
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('fadeMainContent', function(message) {
        
        const container = document.getElementById('fade');
        if (!container) return;

        // Temporarily disable transition to instantly hide
        container.style.transition = 'none';
        container.style.opacity = 0;

        // Force a reflow to apply the style immediately
        void container.offsetWidth; // trick to flush styles

        // Re-enable transition and fade in
        container.style.transition = 'opacity 0.5s ease-in';
        container.style.opacity = 1;
      });
    "))
  ),
  # Left side panel
  fluidRow(
    column(
      width = 3,
      div(class = "sidebar",
          tags$div(style = "text-align: center;",
                   tags$img(src = "profile.jpeg", 
                            style = "width: 15vh; height: 15vh; object-fit: cover;
                        border-radius: 50%; border: 3px solid white; margin-bottom: 10px;"
                            ),
                   h3("Max Patterson", style = "color: var(--brand-cream); margin-bottom: 5px;"),
                   tags$br(),
                   
                   # Social Icons
                   tags$div(
                     # LinkedIn icon
                     tags$a(
                       href = "https://www.linkedin.com/in/maxwell-patterson/",
                       target = "_blank",
                       bs_icon("linkedin", class = "icon-bounce linkedin-icon")
                     ),
                     # GitHub icon
                     tags$a(
                       href = "https://github.com/mmpatterson",
                       target = "_blank",
                       bs_icon("github", class = "icon-bounce github-icon")
                     ),
                     tags$a(
                       href = "mailto:mmpatterson94@gmail.com?subject=Just%20saw%20your%20website", 
                       bs_icon("envelope-arrow-up-fill", class = "icon-bounce email-icon")
                      )
                   )
          ),
          tags$br(),
          # Render the available navigation links
          uiOutput("nav_links")  
      )
    ),
    
    column(
      width = 9,
      div(class = "main",
          div(id = "fade", class = "fade-container",
              uiOutput("main_content")
          )
      )
    )
  ),
  # Reference _brand.yml
  theme = bs_theme(brand = TRUE)
)


server <- function(input, output, session) {
  addResourcePath("data", "./data")
  
  # Section Headings
  current_section <- reactiveVal("About Me")
  observeEvent(input$about, current_section("About Me"))
  observeEvent(input$work, current_section("Work Experience"))
  observeEvent(input$projects, current_section("Projects & Publications"))
  observeEvent(input$resume, current_section("Resume"))
  
  output$nav_links <- renderUI({
    sections <- c("About Me" = "about", 
                  "Projects & Publications" = "projects", "Resume" = "resume")
    
    tagList(
      lapply(names(sections), function(label) {
        id <- sections[[label]]
        class <- if (current_section() == label) "nav-link active" else "nav-link"
        actionLink(inputId = id, label = label, class = class)
      })
    )
  })
  
  # Display data on main section based on current section selected
  output$main_content <- renderUI({
    # Send JS message indicating new content on main panel
    session$sendCustomMessage("fadeMainContent", list())
    
    switch(current_section(),
           "About Me" = tagList(
             # Main Container
             div(
               style = "display: flex; height: 600px; gap: 30px;",
               # Left Side
               div(
                 style = "width: 40%; display: flex; flex-direction: column; justify-content: space-between; padding-right: 20px;",
                 h3("About Me"),
                 p("My name is Max Patterson, and I'm a data scientist living in the Greater Boston area."),
                 p("I have a passion for data and crafting insightful stories with it"),
                 p("I've been able to call 5 different cities my home"),
                 p("I was born and raised in Buffalo, NY. I am a diehard Buffalo Bills fan. I'm also a Buffalo Sabres fan, but it's tough to be a fan right now"),
                 p("I attended Dickinson College in Carlisle, PA, where I earned my bachelor's in physics and mathematics and participated on the cross country and track & field teams. I also have a master's in data science from the University of Texas at Austin"),
                 p(glue::glue("I have ", as.numeric(format(Sys.Date(), "%Y")) - 2016, " years of experience in SQL and ", as.numeric(format(Sys.Date(), "%Y")) - 2019, " years of experience in python and R" )),
                 p("Feel free to look at some of my projects and work history to get a better sense of my background")
               ),
               # Right Side
               div(
                 style = "width: 60%; display: flex; gap: 20px; flex-wrap: wrap;",
                 
                   div(style = "flex: 1 1 50%; min-width: 250px;", leafletOutput("cityMap")),
                   tags$br(),
                   div(style = "flex: 1 1 40%; min-width: 250px;", imageOutput("skiing")),
                   div(style = "flex: 1 1 40%; min-width: 250px;", imageOutput("goose")),
                   div(style = "flex: 1 1 50%; min-width: 250px;", imageOutput("bills")),
                   
               )
               
             ),
             
           ),
           "Projects & Publications" = tagList(
             h3("Projects & Publications"),
             # Cards for each project
             div(class = "project-grid",
                 card(
                   full_screen = FALSE,
                   card_header("Multi-omic signatures of host response associated with presence, type, and outcome of enterococcal bacteremia"),
                   card_body("I was an author of a paper published in the mSystems journal, a publication operated by the American Society for Microbiology. To help the primary author validate a difference in the data samples between two distinct groups of patients, I constructed a logistic regression model that nearly perfectly predicted the two groups in the test set. Utilized R to create the model."),
                   card_footer(
                     tags$a(href = "https://journals.asm.org/doi/10.1128/msystems.01471-24", target = "_blank", "View Paper")
                   )
                 ),
                 card(
                   full_screen = FALSE,
                   card_header("Computer Vision Paper"),
                   card_body("An object detection paper I wrote during my Deep Learning course while earning my master's at the University of Texas. The paper was written as part of our final project, where my three groupmates and I had to construct a guidance system for a 2v2 video game ice hockey team. As part of the project, we trained a Convolutional Neural Network to detect objects in the game, and we would use this information to direct our team players to the right location. We used pyTorch to develop the object detection model"),
                   card_footer(
                     tags$a(href = "data/object_detection_project.pdf", target = "_blank", "View Paper")
                   )
                 ),
                 card(
                   full_screen = FALSE,
                   card_header("Workflows with Posit Team"),
                   card_body("As a data scientist at Suffolk Construction, I partnered with Posit (formerly RStudio) to present about Suffolk's predictive safety model, which aims to assess project risk."),
                   card_footer(
                     tags$a(href = "https://youtu.be/yavHEWpgrCQ?si=MQ_OMf8dLCf1rdM9", target = "_blank", "View Presentation")
                   )
                 ),
                 card(
                   full_screen = FALSE,
                   card_header("Worldwide Earthquake Tracker"),
                   card_body("A simple app to track earthquakes around the world over the past 7 days. Built using javascript."),
                   card_footer(
                     tags$a(href = "https://mmpatterson.github.io/leaflet-earthquake-tracker/earthquake-tracker/", target = "_blank", "View Project")
                   )
                 )
             )
           ),
           "Resume" = tagList(
             h3("Resume"),
             tags$p("Download my resume or preview it below."),
             tags$br(), tags$br(),
             tags$iframe(
               src = "data/maxwell_patterson.pdf",
               style = "width: 100%; height: 80vh; border: none;"
             )
           )
    )
  })
  
  # Locations Map
  cities <- data.frame(
    name = c("Buffalo, NY", "Carlisle, PA", "Indianapolis, IN", "Chicago, IL", "Boston, MA"),
    lat = c(42.8864, 40.2010, 39.7684, 41.8781, 42.3601),
    lng = c(-78.8784, -77.2003, -86.1581, -87.6298, -71.0589),
    stringsAsFactors = FALSE
  )
  
  # Render map based on cities
  output$cityMap <- renderLeaflet({
    leaflet(data = cities) %>%
      addTiles() %>%
      setView(lng = mean(cities$lng), lat = mean(cities$lat), zoom = 5) %>%
      addMarkers(~lng, ~lat, popup = ~name)
  })
  
  # Bills section Image
  output$bills <- renderImage({
    list(
      src = './www/Bills_Photo.jpeg',
      contentType = 'image/jpeg',
      width = "60%",
      height = "70%",
      alt = "A picture of my wife and me at a Bills game"
    )
  }, deleteFile = FALSE)
  
  # Goose Image
  output$goose <- renderImage({
    list(
      src = './www/goose.jpeg',
      contentType = 'image/jpeg',
      width = "60%",
      height = "70%",
      alt = "My dog, Goose"
    )
  }, deleteFile = FALSE)
  
  # Skiing Image
  output$skiing <- renderImage({
    list(
      src = './www/skiing.jpeg',
      contentType = 'image/jpeg',
      width = "60%",
      height = "70%",
      alt = "A picture of my wife, Katy, and me skiing in Vermont"
    )
  }, deleteFile = FALSE)
  
}

# Run the application
shinyApp(ui = ui, server = server)
