
# Setup -------------------------------------------------------------------

library(DT)
library(rnaturalearth)
library(shiny)
library(fresh)
library(wbstats)
library(sf)
library(leaflet)
library(tidyverse)


# Data --------------------------------------------------------------------

## Define indicators to be retrieved using wbstats

indicators = c(
  # Population, total
  pop = "SP.POP.TOTL",
  
  # GDP (current US$)
  gdp = "NY.GDP.MKTP.CD",
  
  # GDP per capita, PPP
  gdp_pc = "NY.GDP.PCAP.PP.KD")


## Retrieve corresponding data using wbstats

country_data <- 
  wb_data(indicators, 
          country = "countries_only",
          mrnev = 1, 
          return_wide = TRUE) %>% 
  mutate_at(vars(gdp, gdp_pc), 
            ~ floor(.)) %>% 
  select(-iso2c)


## Import country background information from wbstats

wb_geo <-
  wb_cachelist$countries %>%
  select(iso3c, region, income_level)

## Get country boundaries from rnaturalearth

worldmapshapes <- 
  rnaturalearth::ne_countries(scale = 110, returnclass = "sf") %>% 
  filter(iso_a3_eh != "ATA") %>% 
  select(iso_a3_eh, continent, income_grp, geometry) #%>% 
# st_make_valid() %>% 
# st_is_valid()




# ui ----------------------------------------------------------------------

ui <- fluidPage(
  
  # Set page background
  
  style = "background-color: #ebebeb;",
  
  # Set up import of icons from font awesome
  
  tags$script(src = "https://kit.fontawesome.com/f377e22697.js"),
  
  # Use Google font
  
  use_googlefont("DM Serif Display"),
  use_theme(create_theme(
    theme = "default",
    bs_vars_font(
      family_sans_serif = "'DM Serif Display', serif"))),
  
  
  # Interactive map and datatables
  
  fluidRow(h2("Exploring incomes and populations of countries", 
              align = "center")),
  
  br(),
  br(),
  
  fluidRow(
    column(3,
           offset = 1,
           
           # Create a dependent slider group to modify map and table outputs
           
           wellPanel(
             
             # Define wellpanel color
             
             style = "background-color: #f0ebd8;",
             
             # import icon from fontawesoms
             
             tags$i(class="fa-solid fa-gears", 
                    style ="font-size:3rem;"),
             
             style = "text-align: center;",
             strong(style = "font-size:2rem;", 
                    "Modify ranges"),
             br(),
             br(),
             br(),
             
             sliderInput(
               inputId = "sliderpop",
               label = "Population",
               dragRange = TRUE,
               round = 1,
               min = min(country_data$pop, na.rm = TRUE), 
               max = max(country_data$pop, na.rm = TRUE),
               value = c(min(country_data$pop, na.rm = TRUE), 
                         max(country_data$pop, na.rm = TRUE))),
             
             sliderInput(
               inputId = "slidergdp",
               label = "GDP",
               dragRange = TRUE,
               min = min(country_data$gdp, na.rm = TRUE), 
               max = max(country_data$gdp, na.rm = TRUE),
               value = c(min(country_data$gdp, na.rm = TRUE), 
                         max(country_data$gdp, na.rm = TRUE))),
             
             sliderInput(
               inputId = "slidergdp_pc",
               label = "GDP per capita",
               dragRange = TRUE,
               min = min(country_data$gdp_pc, na.rm = TRUE), 
               max = max(country_data$gdp_pc, na.rm = TRUE),
               value = c(min(country_data$gdp_pc, na.rm = TRUE), 
                         max(country_data$gdp_pc, na.rm = TRUE)))
           )),
    
    
    column(7,
           tabsetPanel(
             type = "tabs",
             tabPanel(
               "Map", 
               leafletOutput("indicator_map")),
             tabPanel(
               "Continent Summary",
               DT::dataTableOutput("consummary")),
             tabPanel(
               "Income Group Summary",
               DT::dataTableOutput("incsummary")),
             tabPanel(
               "Data",
               DT::dataTableOutput("data"))))
  ),
  
  br(),
  br(),
  
  
  # Plot static map
  
  fluidRow(h2("Visualising GDP per capita of countries", 
              align = "center")),
  br(),
  br(),
  
  plotOutput("gdpmap"),
  
  br(),
  br(),
  
  
  
  br(),
  br(),
  br()
)





# server ------------------------------------------------------------------

server <- 
  function(input, output, session) {
    
    ## static map -------------------------------------------------------------
    
    
    ### World map of latest GDP per capita income 
    
    output$gdpmap <- 
      renderPlot({
        country_data %>% 
          left_join(wb_geo,
                    by = join_by(iso3c == iso3c)) %>% 
          left_join(worldmapshapes, 
                    .,
                    by = join_by(iso_a3_eh == iso3c)) %>% 
          ggplot() +
          geom_sf(aes(fill = gdp_pc)) +
          # facet_wrap(~ region) +
          scale_fill_viridis_c(alpha = 0.7,
                               direction = -1,
                               na.value = "grey95",
                               option = "D") +
          theme_void() +
          labs(fill = "GDP per capita")
      })
    
    
    
    # interactive map ---------------------------------------------------------
    
    
    # Create reactive df based on slider inputs
    
    country_filtered <- 
      reactive({
        country_data %>% 
          filter(
            between(pop, 
                    input$sliderpop[1], 
                    input$sliderpop[2]) & 
              between(gdp, 
                      input$slidergdp[1], 
                      input$slidergdp[2]) &
              between(gdp_pc, 
                      input$slidergdp_pc[1], 
                      input$slidergdp_pc[2]))
      })
    
    # when pop changes, update gdp and gdp_pc range selections
    
    observeEvent(input$sliderpop,  {
      updateSliderInput(session = session, 
                        inputId = "slidergdp", 
                        value = c(country_filtered() %>% 
                                    pull(gdp) %>% 
                                    min(na.rm = TRUE),
                                  country_filtered() %>% 
                                    pull(gdp) %>% 
                                    max(na.rm = TRUE)))
      updateSliderInput(session = session, 
                        inputId = "slidergdp_pc", 
                        value = c(country_filtered() %>% 
                                    pull(gdp_pc) %>% 
                                    min(na.rm = TRUE),
                                  country_filtered() %>% 
                                    pull(gdp_pc) %>% 
                                    max(na.rm = TRUE)))
    })
    
    # when gdp changes, update pop and gdp_pc range selections
    
    observeEvent(input$slidergdp,  {
      updateSliderInput(session = session, 
                        inputId = "sliderpop", 
                        value = c(country_filtered() %>% 
                                    pull(pop) %>% 
                                    min(na.rm = TRUE),
                                  country_filtered() %>% 
                                    pull(pop) %>% 
                                    max(na.rm = TRUE)))
      updateSliderInput(session = session, 
                        inputId = "slidergdp_pc", 
                        value = c(country_filtered() %>% 
                                    pull(gdp_pc) %>% 
                                    min(na.rm = TRUE),
                                  country_filtered() %>% 
                                    pull(gdp_pc) %>% 
                                    max(na.rm = TRUE)))
    })
    
    
    # when gdp_pc changes, update pop and gdp range selections
    
    observeEvent(input$slidergdp_pc,  {
      updateSliderInput(session = session, 
                        inputId = "sliderpop", 
                        value = c(country_filtered() %>% 
                                    pull(pop) %>% 
                                    min(na.rm = TRUE),
                                  country_filtered() %>% 
                                    pull(pop) %>% 
                                    max(na.rm = TRUE)))
      updateSliderInput(session = session, 
                        inputId = "slidergdp", 
                        value = c(country_filtered() %>% 
                                    pull(gdp) %>% 
                                    min(na.rm = TRUE),
                                  country_filtered() %>% 
                                    pull(gdp) %>% 
                                    max(na.rm = TRUE)))
    })
    
    
    output$indicator_map <- 
      renderLeaflet({
        country_filtered() %>%
          left_join(worldmapshapes, 
                    .,
                    by = join_by(iso_a3_eh == iso3c)) %>%   
          leaflet() %>% 
          # setMaxBounds(lng1 = -180, lat1 = -55.61183,
          #              lng2 = 180, lat2 = 83.64513) %>% 
          addProviderTiles("CartoDB.VoyagerNoLabels",
                           options = providerTileOptions(minZoom = 1, 
                                                         maxZoom = 3)) %>% 
          addPolygons(group = "GDP per capita",
                      color = "#444444",
                      weight = 1,
                      opacity = 0.2, 
                      fillOpacity = 0.5,
                      fillColor = ~ colorNumeric("Greens", gdp_pc)(gdp_pc),
                      # label = ~ country_data$country, 
                      highlightOptions = highlightOptions(color = "brown",
                                                          weight = 2,
                                                          bringToFront = TRUE)) %>% 
          # addLegend(group = "GDP per capita",
          #           pal = colorNumeric,
          #           values = ~ country_data$gdp_pc,
          #           opacity = 0.7) %>%
          addPolygons(group = "GDP",
                      color = "#444444",
                      weight = 1,
                      opacity = 0.2, 
                      fillOpacity = 0.5,
                      fillColor = ~ colorNumeric("Blues", gdp)(gdp),
                      # label = ~ country_data$country,
                      highlightOptions = highlightOptions(color = "brown",
                                                          weight = 2,
                                                          bringToFront = TRUE)) %>% 
          addPolygons(group = "Population",
                      color = "#444444",
                      weight = 1,
                      opacity = 0.2, 
                      fillOpacity = 0.5,
                      fillColor = ~ colorNumeric("Purples", pop)(pop),
                      highlightOptions = highlightOptions(color = "brown",
                                                          weight = 2,
                                                          bringToFront = TRUE)) %>% 
          # addScaleBar() %>% 
          addLayersControl(baseGroups = 
                             c('GDP per capita', 'GDP', 'Population'),
                           options = layersControlOptions(collapsed = FALSE))
      })
    
    
    
    # datatable ---------------------------------------------------------------
    
    output$consummary <- 
      DT::renderDataTable({
        DT::datatable(
          country_filtered() %>%
            left_join(wb_geo,
                      by = join_by(iso3c == iso3c)) %>%
            drop_na(gdp_pc) %>%
            summarise(n = n(),
                      mean_gdppc = mean(gdp_pc, na.rm = TRUE),
                      mean_pop = mean(pop, na.rm = TRUE),
                      mean_gdp = mean(gdp, na.rm = TRUE),
                      .by = region), 
          options = list(
            paging = FALSE,
            dom = "t",
            ordering = FALSE,
            searching = FALSE),
          colnames=c("Region", 
                     "Number of countries",
                     "Mean GDP per capita",
                     "Mean Population",
                     "Mean GDP")) %>% 
          formatRound(c('n', 'mean_gdp', 'mean_pop', 'mean_gdppc'), 
                      digits = 0)
      })
    
    output$incsummary <- 
      DT::renderDataTable({
        DT::datatable(
          country_filtered() %>%
            left_join(wb_geo,
                      by = join_by(iso3c == iso3c)) %>%
            drop_na(gdp_pc) %>%
            summarise(n = n(),
                      mean_gdppc = mean(gdp_pc, na.rm = TRUE),
                      mean_pop = mean(pop, na.rm = TRUE),
                      mean_gdp = mean(gdp, na.rm = TRUE),
                      .by = income_level),
          options = list(
            paging = FALSE,
            dom = "t",
            ordering = FALSE,
            searching = FALSE),
          colnames=c("Income Level", 
                     "Number of countries",
                     "Mean GDP per capita",
                     "Mean Population",
                     "Mean GDP")) %>% 
          formatRound(c('n', 'mean_gdp', 'mean_pop', 'mean_gdppc'), 
                      digits = 0)
      })
    
    
    output$data <- 
      DT::renderDataTable({
        DT::datatable(
          country_filtered() %>%
            left_join(wb_geo,
                      by = join_by(iso3c == iso3c)) %>% 
            select(-c(iso3c, date)),
          options = list(
            paging = TRUE,
            ordering = TRUE,
            searching = TRUE),
          colnames=c("Country",
                     "GDP",
                     "GDP per capita",
                     "Population",
                     "Region",
                     "Income Level")) %>% 
          formatRound(c('gdp', 'pop', 'gdp_pc'), 
                      digits = 0)
      })
  }

# shinyapp ----------------------------------------------------------------


shinyApp(ui = ui, server = server)

