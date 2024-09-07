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


# Define UI for the application
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  # shinythemes::themeSelector(),
  
  # Application title
  # titlePanel("My Personal Website"),
  
  # Create a top bar with dropdowns
  navbarPage(
    title = "Maxwell Patterson",
    tabPanel("",icon = icon("home", lib = "glyphicon"),
             h1("Welcome to My Personal Website"),
             p("This is the home page where you can find an overview of my work and links to other sections of the site."),
             p("Use the navigation bar above to access my resume, LinkedIn profile, and projects.")
    ),
    
    # tabPanel(title = tags$div(
    #   class = "social",
    #   a(href = "https://www.linkedin.com/in/maxwell-patterson/",target="_blank", icon('linkedin','fab fa-linkedin social_icons'), style = "color: #2c3e50;")#,
    #   # a(href = "https://github.com/chrisselig",target="_blank", icon("fab fa-github","fab fa-github social_icons"), style = "color: #2c3e50;"),
    #   # a(href = "mailto:chris.selig@bidamia.ca",target="_blank", icon("envelope","fal fa-envelope float-right social_icons"), style = "color: #2c3e50;")
    #   )
    # ),
    
    tabPanel(title = tags$div(
      onclick = "https://www.linkedin.com/in/maxwell-patterson/",
      # target = "_blank",
      bsicons::bs_icon("linkedin", size = "2.5rem"),   # Adding a Bootstrap icon
      # "External Link"
    ),
    value = "external_link"
    ),
    
    # tabPanel("", icon = bsicons::bs_icon("linkedin", size = "2.5rem"),
    #          title = tags$a(href = "https://www.linkedin.com/in/maxwell-patterson/", "LinkedIn", target = "_blank"),
    #          value = "external_link"
    # ),
    
    # Dropdown for Resume
    tabPanel("Resume", 
             div(
               style = "text-align: center;",  # Center the content
               tags$iframe(
                 src = "data/maxwell_patterson.pdf",  # URL to your PDF using the resource path
                 width = "80%",              # Set width to 80%
                 height = "1000px",           # Set a fixed height
                 frameborder = "0"
               )
             )
    ),
    
    # tabPanel(title = "LinkedIn",
    #          a(href = "https://www.linkedin.com/in/maxwell-patterson/", "LinkedIn", target = "_blank")
    # ),
    
    
    # Dropdown for Projects
    navbarMenu("Projects",
               tabPanel("Deep Learning",
                        h3("Wikiracer Object Detection"),
                        p("This project was completed during my master's program. Our goal was to utilize an open source driving game, SuperTuxKart, to train a team of karts to detect a puck and score goals on the opposing team."),
                        div(
                          style = "text-align: center;",  # Center the content
                          tags$iframe(
                            src = "data/object_detection_project.pdf",  # URL to your PDF using the resource path
                            width = "80%",              # Set width to 80%
                            height = "1000px",           # Set a fixed height
                            frameborder = "0"
                          )
               )
               ),
               tabPanel(a(href = "https://mmpatterson.github.io/leaflet-earthquake-tracker/earthquake-tracker/", "Earthquake Tracker", target = "_blank")#,
                        # h3("Worldwide Earthquake Tracker"),
                        
                        # p("Details about Project 2."),
                        
                        # a(href = "https://linktoyourproject2.com", "View Project 2", target = "_blank")
               )
               # Add more projects as needed
    )
  )
)
# Define server logic
server <- function(input, output) {
  # No server logic required for this basic example
  addResourcePath("data", "../data")
}

# Run the application
shinyApp(ui = ui, server = server)

# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
#     
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
