#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(httr2)
library(bslib)
library(shiny)
library(shinythemes)
library(bsicons)
library(scrollytell)
library(leaflet)
library(htmltools)

# Define UI for the application
ui <- fluidPage(
    
  theme = shinytheme("flatly"),
  # shinythemes::themeSelector(),
  # tags$head(
  #   includeCSS("./www/styles.css")
  # ),
  tags$head(
    tags$style(HTML("
      #fadeContainer {
        transition: opacity 2.5s ease-in-out;
        opacity: 1;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('fade', function(message) {
        const container = document.getElementById('fadeContainer');
        if (!container) return;
        container.style.opacity = 0;
        setTimeout(function() {
          container.style.opacity = 1;
        }, 100);
      });
    "))
  ),
  # Create a top bar with dropdowns
  navbarPage(
    title = "Maxwell Patterson",
    tabPanel("",icon = icon("home", lib = "glyphicon"),
             h1("Welcome to My Personal Website"),
             p("This is the home page where you can find an overview of my work and links to other sections of the site."),
             p("Use the navigation bar above to access my resume, LinkedIn profile, and projects.")
    ),
    tabPanel(title = tags$div(
      onclick = "https://www.linkedin.com/in/maxwell-patterson/",
      target = "_blank",
      bsicons::bs_icon("linkedin", size = "2.5rem"),   # Adding a Bootstrap icon
      # "External Link"
    ),
    value = "external_link"
    ),
    tabPanel(title = tags$div(
      onclick = "https://github.com/mmpatterson",
      target = "_blank",
      bsicons::bs_icon("github", size = "2.5rem"),   # Adding a Bootstrap icon
    ),
    value = "external_link"
    ),

    # Dropdown for Resume
    tabPanel("Resume",
             div(
               style = "text-align: center;",  # Center the content
               tags$iframe(
                 src = "data/maxwell_patterson.pdf",
                 width = "80%",              # Set width to 80%
                 height = "1000px",           # Set a fixed height
                 frameborder = "0"
               )
             )
    ),

    # Dropdown for Projects
    navbarMenu("Projects",
               tabPanel("Deep Learning",
                        h3("Wikiracer Object Detection"),
                        p("This project was completed during my master's program. Our goal was to utilize an open source driving game, SuperTuxKart, to train a team of karts to detect a puck and score goals on the opposing team."),
                        div(
                          style = "text-align: center;",  # Center the content
                          tags$iframe(
                            src = "./data/object_detection_project.pdf",  # URL to your PDF using the resource path
                            width = "80%",              # Set width to 80%
                            height = "1000px",           # Set a fixed height
                            frameborder = "0"
                          )
               )
               ),
               tabPanel(a(href = "https://mmpatterson.github.io/leaflet-earthquake-tracker/earthquake-tracker/", "Earthquake Tracker", target = "_blank")#,
               )
               # Add more projects as needed
    )
  ),
  sidebarLayout( sidebarPanel("About me"),
       tabsetPanel(id="tab",
           tabPanel("bla",
                fluidRow(
                  scrolly_container("scr", 
                      scrolly_graph(
                        div(id = "fadeContainer", uiOutput("dynamicOutput"))
                      ), 
                      scrolly_sections(
                        scrolly_section(id = "red",
                          h3("About me"),
                          p("Hello! My name is Max Patterson, and I am a data scientist in the Greater Boston area")
                        ),
                        scrolly_section(id = "pink","I have a passion for data and crafting insightful stories with it"),
                        scrolly_section(id = "locations","I've been lucky to live in a few different cities in my life"),
                        scrolly_section(id = "bills","I was born and raised in Buffalo, NY. I am a diehard Buffalo Bills fan. I also like the Buffalo Sabres, but it's hard rooting for such a downtrodden team.")
                      )
                  )
               )
          )
       )
  ),
  div("Footer")
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  addResourcePath("data", "./data")
  
  output$dynamicOutput <- renderUI({
    if(input$scr == "locations"){
      leafletOutput("cityMap", height = "60vh", width = "60vw")
    }
    
    else if (input$scr == "bills"){
      imageOutput("bills")
    }
    else{
      imageOutput("bills")
    }
  })
  
  
  # Locations Map
  cities <- data.frame(
    name = c("Buffalo, NY", "Carlisle, PA", "Indianapolis, IN", "Chicago, IL", "Boston, MA"),
    lat = c(42.8864, 40.2010, 39.7684, 41.8781, 42.3601),
    lng = c(-78.8784, -77.2003, -86.1581, -87.6298, -71.0589),
    stringsAsFactors = FALSE
  )
  
  output$cityMap <- renderLeaflet({
    leaflet(data = cities) %>%
      addProviderTiles("Stadia.AlidadeSmooth") %>%
      setView(lng = mean(cities$lng), lat = mean(cities$lat), zoom = 5) %>%
      addMarkers(~lng, ~lat, popup = ~name)
  })
  
  # Bills section Image
  output$bills <- renderImage({
    list(
      src = './www/Bills_Photo.jpeg',
      contentType = 'image/jpeg',
      width = "50%",
      heiht = "30%",
      alt = "Me at a Bills game"
    )
  }, deleteFile = FALSE)
  
  output$scr <- renderScrollytell({scrollytell()})
  output$section <- renderText(paste0("Section: ", input$scr))
  
  observe({cat("section:", input$scr, "\n")})
  
  observeEvent(input$scr, {
    session$sendCustomMessage("fade", list())
  })

}

# Run the application
shinyApp(ui = ui, server = server)
