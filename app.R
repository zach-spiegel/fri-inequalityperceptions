#install.packages("surveydown")

library(surveydown)
library(sf)
library(shiny)
library(tigris)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(treemapify)
library(scales)
library(shinycssloaders)

if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2', 'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')

devtools::install_github('bbc/bbplot')

# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/manuals/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.



# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {
  
  total_guess <- reactive({
    input$top20_guess + input$sec20_guess +
      input$third20_guess + input$fourth20_guess +
      input$bottom20_guess
  })
  
  output$display_total_guess <- renderText({
    paste("Total percent:", total_guess())
  })
  
  guess_data = reactive({
    data.frame(
      Bucket = factor(
        c("Top 20%", "2nd 20%", "3rd 20%", "4th 20%", "Bottom 20%"),
        levels = c("Top 20%", "2nd 20%", "3rd 20%", "4th 20%", "Bottom 20%")),
      Percentage = c(
        as.numeric(input$top20_guess), 
        as.numeric(input$sec20_guess), 
        as.numeric(input$third20_guess), 
        as.numeric(input$fourth20_guess), 
        as.numeric(input$bottom20_guess)
      )
    )
  })
  
  
  
  output$guess_plot = renderPlot({
    ggplot(guess_data(), aes(area = Percentage, fill = Bucket)) +
      geom_treemap() + 
      bbc_style() +
      scale_fill_manual(values=c("#8e44ad", "#2980b9", "#27ae60", "#f1c40f", "#c0392b"))
  })
  
  output$guess_percentages <- renderUI({
    HTML(paste0(
      "<p style='font-size:14px; margin-top:8px;'>",
      "<b>Top 20%:</b> ", input$top20_guess, "% &nbsp;|&nbsp; ",
      "<b>2nd 20%:</b> ", input$sec20_guess, "% &nbsp;|&nbsp; ",
      "<b>3rd 20%:</b> ", input$third20_guess, "% &nbsp;|&nbsp; ",
      "<b>4th 20%:</b> ", input$fourth20_guess, "% &nbsp;|&nbsp; ",
      "<b>Bottom 20%:</b> ", input$bottom20_guess, "%",
      "</p><hr style='margin: 24px 0;'>"
    ))
  })
  
  total_ideal = reactive({
    input$top20_ideal + input$sec20_ideal +
      input$third20_ideal + input$fourth20_ideal +
      input$bottom20_ideal
  })
  
  output$display_total_ideal = renderText({
    paste("Total percent:", total_ideal())
  })
  
  sd_stop_if(
    (input$top20_guess + input$sec20_guess + input$third20_guess + 
       input$fourth20_guess + input$bottom20_guess) != 100 ~ 
      "Your guess percentages must add up to 100% before continuing.",
    (input$top20_ideal + input$sec20_ideal + input$third20_ideal + 
       input$fourth20_ideal + input$bottom20_ideal) != 100 ~ 
      "Your ideal percentages must add up to 100% before continuing."
  )
  
  
  ideal_data = reactive({
    data.frame(
      Bucket = factor(
        c("Top 20%", "2nd 20%", "3rd 20%", "4th 20%", "Bottom 20%"),
        levels = c("Top 20%", "2nd 20%", "3rd 20%", "4th 20%", "Bottom 20%")),
      Percentage = c(
        as.numeric(input$top20_ideal), 
        as.numeric(input$sec20_ideal), 
        as.numeric(input$third20_ideal), 
        as.numeric(input$fourth20_ideal), 
        as.numeric(input$bottom20_ideal)
      )
    )
  })
  
  output$ideal_plot = renderPlot({
    ggplot(ideal_data(), aes(area = Percentage, fill = Bucket)) +
      geom_treemap() + 
      bbc_style() +
      scale_fill_manual(values=c("#8e44ad", "#2980b9", "#27ae60", "#f1c40f", "#c0392b"))
  })
  
  output$ideal_percentages <- renderUI({
    HTML(paste0(
      "<p style='font-size:14px; margin-top:8px;'>",
      "<b>Top 20%:</b> ", input$top20_ideal, "% &nbsp;|&nbsp; ",
      "<b>2nd 20%:</b> ", input$sec20_ideal, "% &nbsp;|&nbsp; ",
      "<b>3rd 20%:</b> ", input$third20_ideal, "% &nbsp;|&nbsp; ",
      "<b>4th 20%:</b> ", input$fourth20_ideal, "% &nbsp;|&nbsp; ",
      "<b>Bottom 20%:</b> ", input$bottom20_ideal, "%",
      "</p><hr style='margin: 24px 0;'>"
    ))
  })
  
  
  real_data_treeplot = reactive({
    data.frame(
      Bucket = factor(
        c("Top 20%", "2nd 20%", "3rd 20%", "4th 20%", "Bottom 20%"),
        levels = c("Top 20%", "2nd 20%", "3rd 20%", "4th 20%", "Bottom 20%")),
      Percentage = c(76, 15, 6, 2, 1)
    )
  })
  
  output$real_plot = renderPlot({
    ggplot(real_data_treeplot(), aes(area = Percentage, fill = Bucket)) +
      geom_treemap() + 
      bbc_style() +
      scale_fill_manual(values=c("#8e44ad", "#2980b9", "#27ae60", "#f1c40f", "#c0392b"))
  })
  
  output$real_percentages <- renderUI({
    HTML(paste0(
      "<p style='font-size:14px; margin-top:8px;'>",
      "<b>Top 20%:</b> 76% &nbsp;|&nbsp; ",
      "<b>2nd 20%:</b> 15% &nbsp;|&nbsp; ",
      "<b>3rd 20%:</b> 6% &nbsp;|&nbsp; ",
      "<b>4th 20%:</b> 2% &nbsp;|&nbsp; ",
      "<b>Bottom 20%:</b> 1%",
      "</p>"
    ))
  })
  

  combined_data = reactive({
    guess <- guess_data()
    guess$Type <- "Guess"
    
    ideal = ideal_data()
    ideal$Type <- "Ideal"
    
    real = real_data_treeplot()
    real$Type <- "Actual"
    
    combined = rbind(guess, ideal, real)
  })
  
  
  output$lollipop = renderPlot({
    df = combined_data()
    df$Bucket = factor(df$Bucket, levels = rev(c("Top 20%", "2nd 20%", "3rd 20%", "4th 20%", "Bottom 20%")))
    df$Type = factor(df$Type, levels = c("Guess", "Ideal", "Actual"))
    
    line_data = df %>%
      group_by(Bucket) %>%
      summarise(
        xmin = min(Percentage),
        xmax = max(Percentage),
        .groups = "drop"
      )
    ggplot() +
      geom_segment(data = line_data,
                   aes(x = xmin, xend = xmax, y = Bucket, yend = Bucket),
                   color = "gray60", size = 1) +
      geom_point(data = df,
                 aes(x = Percentage, y = Bucket, color = Type),
                 size = 3) +
      scale_color_manual(
        values = c(
          "Guess" = "red",
          "Ideal" = "blue",
          "Actual" = "green"
        )
      ) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10),
        labels = scales::label_percent(scale = 1)
      ) +
      labs(
        x = "Wealth (%)", 
        y = "Income Bracket",
        color = "Data Type"
      ) +
      bbc_style() # for making nice looking graphs
  })
  
  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if()

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if()

  # Database designation and other settings
  sd_server(db = NULL)

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
