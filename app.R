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
library(nflfastR)
library(dplyr)
library(gt)
library(gtExtras)
library(DT)
library(ggplot2)
library(ggimage)
library(glue)
library(thematic)

thematic::thematic_shiny()

# Define UI for the application
ui <- fluidPage(
    
  # theme = bs_theme(brand = TRUE),
  # shinythemes::themeSelector(),
  tags$head(
    includeCSS("./www/styles.css")
  ),
  tags$head(
    tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function () {
      const container = document.getElementById('scrolly-section-holder');
      const sections = container.querySelectorAll('.scrolly-section');
      let current = 0;
      let isScrolling = false;

      function scrollToSection(index) {
        if (index < 0 || index >= sections.length) return;
        isScrolling = true;
        sections[index].scrollIntoView({ behavior: 'smooth' });
        current = index;
        setTimeout(() => { isScrolling = false; }, 700); // delay to prevent rapid scrolling
      }

      container.addEventListener('wheel', function (e) {
        if (isScrolling) return;
        if (e.deltaY > 0) {
          scrollToSection(current + 1); // scroll down
        } else {
          scrollToSection(current - 1); // scroll up
        }
        e.preventDefault(); // stop default scrolling
      }, { passive: false });
    });
  "))
  ),
  tags$head(
    # tags$style(HTML("
    #   #fadeContainer {
    #     opacity: 1;
    #     transition: opacity 0.5s ease-in;
    #   }
    # ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('fade', function(message) {
        const container = document.getElementById('fadeContainer');
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
  # Create a top bar with dropdowns
  # page_navbar(
  #   title = "Maxwell Patterson",
  #   tabPanel("",icon = icon("home", lib = "glyphicon")
  #   ),
  #   tabPanel(title = tags$div(
  #     onclick = "https://www.linkedin.com/in/maxwell-patterson/",
  #     target = "_blank",
  #     bsicons::bs_icon("linkedin", size = "2.5rem"),   # Adding a Bootstrap icon
  #     # "External Link"
  #   ),
  #   value = "external_link"
  #   ),
  #   tabPanel(title = tags$div(
  #     onclick = "https://github.com/mmpatterson",
  #     target = "_blank",
  #     bsicons::bs_icon("github", size = "2.5rem"),   # Adding a Bootstrap icon
  #   ),
  #   value = "external_link"
  #   ),
  # 
  #   # Dropdown for Resume
  #   tabPanel("Resume",
  #            div(
  #              style = "text-align: center;",  # Center the content
  #              tags$iframe(
  #                src = "data/maxwell_patterson.pdf",
  #                width = "80%",              # Set width to 80%
  #                height = "1000px",           # Set a fixed height
  #                frameborder = "0"
  #              )
  #            )
  #   ),
  # 
  #   # Dropdown for Projects
  #   navbarMenu("Projects",
  #              tabPanel("Deep Learning",
  #                       h3("Wikiracer Object Detection"),
  #                       p("This project was completed during my master's program. Our goal was to utilize an open source driving game, SuperTuxKart, to train a team of karts to detect a puck and score goals on the opposing team."),
  #                       div(
  #                         style = "text-align: center;",  # Center the content
  #                         tags$iframe(
  #                           src = "./data/object_detection_project.pdf",  # URL to your PDF using the resource path
  #                           width = "80%",              # Set width to 80%
  #                           height = "1000px",           # Set a fixed height
  #                           frameborder = "0"
  #                         )
  #              )
  #              ),
  #              tabPanel(a(href = "https://mmpatterson.github.io/leaflet-earthquake-tracker/earthquake-tracker/", "Earthquake Tracker", target = "_blank")#,
  #              )
  #              # Add more projects as needed
  #   )
  # ),

  scrolly_container("scr",
                    scrolly_graph(
                      div(id = "fadeContainer", uiOutput("dynamicOutput"))
                    ),
                    
                    scrolly_sections(id = "scrolly-section-holder",
                                     
                      scrolly_section(id = "intro",
                                      
                                      p("Hello! My name is Max Patterson, and I am a data scientist in the Greater Boston area")
                      ),
                      scrolly_section(id = "data",
                                      "I have a passion for data and crafting insightful stories with it"
                      ),
                      scrolly_section(id = "locations",
                                      "I've been able to call 5 different cities my home"
                      ),
                      scrolly_section(id = "bills",
                                      "I was born and raised in Buffalo, NY. I am a diehard Buffalo Bills fan. I also like the Buffalo Sabres, but it's hard rooting for such a downtrodden team"
                      ),
                      scrolly_section(id = "education",
                                      "I attended Dickinson College in Carlisle, PA, where I earned my bachelor's in physics and mathematics and participated on the cross country and track & field teams. I also have a master's in data science from the University of Texas at Austin"
                      ),
                      scrolly_section(id = "career-high-level",
                                      "I started out of undergrad as an IT consultant, and I kept getting drawn to the technical portions of my work. I decided to do a data science bootcamp, and I realized that a career in data science was what I was looking for."
                      ),
                      scrolly_section(id = "data-science-specifics",
                                      glue::glue("I have ", as.numeric(format(Sys.Date(), "%Y")) - 2016, " years of experience in SQL and ", as.numeric(format(Sys.Date(), "%Y")) - 2019, " years of experience in python and R" )
                      )
                  )
  ),
  theme = bs_theme(brand = TRUE)
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  addResourcePath("data", "./data")
  
  # Perform NFL Data Calculations
  pbp <- calculate_stats(
    seasons = nflreadr::most_recent_season(),
    summary_level = c("season"),
    stat_type = c("team"),
    season_type = c("REG")
  ) %>%
    select(team, passing_yards, passing_yards_after_catch) %>%
    mutate(passing_yards_before_catch = passing_yards - passing_yards_after_catch) %>%
    inner_join(
      teams_colors_logos %>%
        select(team_abbr, team_wordmark, team_logo_espn),
      by = c('team' = 'team_abbr')
    )
  
  pbp_data <- renderDT(as.data.frame(pbp %>% head(10)))
  
  team_table <- gt(pbp %>% select(team_wordmark, team_logo_espn, passing_yards, passing_yards_before_catch, passing_yards_after_catch)) %>%
    cols_move(
      columns = passing_yards_after_catch,
      after = passing_yards_before_catch
    ) %>%
    gt_img_rows(columns = team_wordmark, height = 25) %>%
    gt_img_rows(columns = team_logo_espn, img_source = "web", height = 30) %>%
    cols_label(
      passing_yards = "Passing Yards",
      passing_yards_before_catch = "Passing Yards Before Catch",
      passing_yards_after_catch = "Passing Yards After Catch",
      team_wordmark = "",
      team_logo_espn = ""
    ) %>%
    tab_options(
      table.background.color = "#FAFAF7"
    )
  
  med_before_catch <- median(pbp$passing_yards_before_catch)
  med_after_catch <- median(pbp$passing_yards_after_catch)
  
  team_plot <- renderPlot(
    pbp %>%
    ggplot(aes(x = passing_yards_before_catch, y = passing_yards_after_catch)) + 
    geom_image(aes(image = team_logo_espn)) +
    geom_vline(xintercept = med_before_catch) +
    geom_hline(yintercept = med_after_catch) +
    labs(
      title = "Passing Yards Before/After Catch"
    ) +
    xlab("Passing Yards Before Catch") +
    ylab("Passing Yards After Catch") +
    theme_minimal()
  ) 
  
  timer <- reactiveTimer(3000)
  options <- c("pbp_data", "team_table", "team_plot")
  print(options)
  counter <- reactiveVal(1)
  
  observe({
    timer()
    
    isolate({
    new_val <- counter() %% length(options) + 1
    counter(new_val)
    })
  
  })
  
  output$dynamicOutput <- renderUI({
    if(input$scr == "data"){
      val <- options[counter()]
      session$sendCustomMessage("fade", list())
      if(val == "pbp_data"){
        pbp_data
      }
      else if(val == "team_table") {
        render_gt(team_table)
      }
      else if(val == "team_plot"){
        tags$div(id = "shiny-plot-output", team_plot)
      }
        
    }
    else if(input$scr == "locations"){
      leafletOutput("cityMap", height = "60vh", width = "60vw")
    }

    else if (input$scr == "bills"){
      imageOutput("bills")
    }
    else{
      
    }
  })
  
  # output$dynamicOutput <- renderUI({
  #   req(input$scr)
  #   switch(input$scr,
  #          "intro" = h2("Intro Panel: Overview of Max"),
  #          "pink" = h2("Pink Panel: Data Passion"),
  #          "locations" = h2("Locations Panel: Where I've Lived"),
  #          "bills" = h2("Bills Panel: Sports Fandom")
  #   )
  # })
  
  
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
      width = "60%",
      height = "70%",
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
