library(shiny)
library(ggplot2)
library(DT)
library(rnaturalearth)

# Wczytywanie danych raz przy starcie serwera
data_map <- readRDS("data_map.rds")
data_table <- readRDS("data_table.rds")
data_future <- readRDS("data_future.rds")

ui <- fluidPage(
  titlePanel("Geographic access to radiotherapy"),
  tabsetPanel(
    tabPanel("Current map",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("threshold", "Travel time threshold in minutes:",
                             min = 60, max = 360, value = 120)
               ),
               mainPanel(
                 plotOutput("mapPlot")
               )
             )
    ),
    tabPanel("Current table",
             DTOutput("dataTable")
    ),
    tabPanel("Proposed future locations",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("n_centers", "Number of new proposed centers:",
                             min = 0, max = 500, value = 100)
               ),
               mainPanel(
                 plotOutput("futureMap"),
                 br(),
                 DTOutput("futureTable")
               )
             )
    )
  ),
  br(), br(),
  tags$footer(
    tags$p(
      "This application is a visualization of findings presented in the article ",
      tags$em("\"Global access to radiotherapy: a geospatial analysis of current disparities and optimal facility placement\""),
      " by Dominik Wawrzuta, Justyna Klejdysz, Katarzyna Pędziwiatr, Marzanna Chojnacka.",
      style = "text-align: center; font-size: 0.9em; color: #666;"
    )
  )
)

server <- function(input, output, session) {
  
  output$mapPlot <- renderPlot({
    country_access <- data_map %>%
      group_by(country) %>%
      summarise(
        pop_under_120 = sum(population[time <= input$threshold], na.rm = TRUE),
        total_pop = sum(population, na.rm = TRUE)
      ) %>%
      mutate(
        access_percent = pop_under_120 / total_pop * 100,
        access_percent = ifelse(is.na(access_percent), 0, access_percent)  # Convert NA to 0
      )
    
    world <- ne_countries(scale = "medium", returnclass = "sf") %>%
      left_join(country_access, by = c("iso_a3" = "country")) %>%
      mutate(access_percent = ifelse(is.na(access_percent), 0, access_percent))  # Convert NA to 0 again after join
    
    plot_map <- ggplot(world) +
      geom_sf(aes(fill = cut(access_percent, 
                             breaks = c(-Inf, 20, 40, 60, 80, 100)))) +
      scale_fill_viridis_d(
        option = "mako",
        name = "Population with\naccess <120 minutes",
        labels = c("0%-20%", "20%-40%", "40%-60%", "60%-80%", "80%-100%")
      ) +
      labs(title = paste("Access to Radiotherapy Center Within", input$threshold, "Minutes", sep=" ")) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      ) +
      coord_sf(crs = "+proj=robin")
    
    return(plot_map)
  })
  
  output$dataTable <- renderDT({
    df <- data_table
    for (i in 2:13) {
      df[[i]] <- round(df[[i]])
    }
    colnames(df) <- c("Country", "Number of subregions", "Median area (km2)", "Q1 area (km2)", "Q3 area (km2)", "Median population", "Q1 population", "Q3 population", "% of population with access <60 minutes", "% of population with access <120 minutes", "% of population with access <240 minutes", "Median travel time (minutes)", "Q1 travel time (minutes)", "Q3 travel time (minutes)")
    datatable(df, options = list(pageLength = 100), rownames = FALSE)
  })
  
  output$futureMap <- renderPlot({
    points_sf <- data_future[0:input$n_centers, ] %>%
      st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
      st_transform(crs = "+proj=robin")
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    plot_centers <- ggplot() +
      geom_sf(data = world, fill = "white", color = "gray80") +
      geom_sf(data = points_sf, color = "#e76f51", size = 1) +
      labs(title = paste("Optimal Locations of", input$n_centers, "New Centers", sep=" ")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      ) +
      coord_sf(crs = "+proj=robin")
    
    return(plot_centers)
    
    plot_map_centers(data_future, input$n_centers)
  })
  
  output$futureTable <- renderDT({
    df <- data_future[0:input$n_centers, ]
    colnames(df) <- c("Country code", "Latitude", "Longitude", "Population gaining 120-minute access")
    df[[4]] <- floor(df[[4]])  # lub round(df[[4]])
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui, server)
