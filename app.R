library(raster)
library(tidyverse)
library(leaflet)
library(shiny)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(tigris)
library(sf)
library(tmap)
library(janitor)
library(here)


### WRANGLING DATA

# Socio data
source("heatmap_creation.R")

# Study region
study_region_table <- read_csv("study_region.csv") %>%
    clean_names() %>%
    select(area_ha:assisting_org)

# Connectivity map layers
barriers <- here('connectivity/barriers1.tif')
pinch_points <- here('connectivity/pinchpoints.tif')
lc_corridor <- here('connectivity/corridors.tif')
resistance <- here('connectivity/resistance.tif')

### UI

ui <- shinyUI(fluidPage(

    # load custom stylesheet
    includeCSS("www/style.css"),

    # remove shiny "red" warning messages on GUI
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),

    # load page layout
    dashboardPage(

        skin = "green",

        dashboardHeader(title = span(img(src = "PP_logo.png", width = 150)),
                        titleWidth = 250,

                        dropdownMenu(
                            type = "notifications",
                            headerText = strong("Learn More"),
                            icon = icon("share-alt"),
                            badgeStatus = NULL,
                            notificationItem(
                                text = "Primate Pathways",
                                icon = icon("globe"),
                                href = "https://primatepathways.weebly.com/"),
                            notificationItem(
                                text = "Contact us",
                                icon = icon("envelope"),
                                href = "https://primatepathways.weebly.com/contact-us.html"))),

        dashboardSidebar(width = 250,
                         sidebarMenu(
                             menuItem("Home",
                                      tabName = "home",
                                      icon = icon("home")
                             ),

                             menuItem("Study Region",
                                      tabName = "map",
                                      icon = icon("map-marker-alt")),

                             menuItem("Corridor Network",
                                      tabName = "charts",
                                      icon = icon("map")),

                             menuItem("Socioeconomic",
                                      tabName = "socio",
                                      icon = icon("user-friends")),

                             menuItem("Climate Change",
                                      tabName = "climate",
                                      icon = icon("sun"))

                         )), # end dashboardSidebar

        dashboardBody(

            tabItems(

                tabItem(tabName = "home",
                        includeMarkdown("www/delete.Rmd")
                ),

                tabItem(tabName = "map",
                        h1(strong("Study Region")),
                        p("The study region is located in the eastern Democratic Republic of Congo in the Congo Basin rainforest within the Albertine Rift. Strong Roots Congo is working with local communities to designate an ecological corridor connecting Kahuzi-Biega National Park and Itombwe Nature Reserve that is made up of a network of 7 community forests. Strong Roots is also designating an additional 5 sections adjacent to the community forests as reforestation zones. The two protected areas, individual community forests, and reforestation zones can all be explored on the map below."),
                        br(),
                        fluidRow(
                            column(4,box(width = 10, status = "success", solidHeader = TRUE, title="Area Type:",
                                         checkboxGroupInput(inputId = "projectInput",
                                                            label = NULL,
                                                            choices = c("Commmunity Forest", "Reforestation Zone", "National Park", "Nature Reserve"),
                                                            selected = c("Commmunity Forest", "Reforestation Zone", "National Park", "Nature Reserve")),
                                         tableOutput("table3"))),
                            column(8, leafletOutput("regionBase") %>% withSpinner(color = "green"))
                        ),
                        br(),
                        dataTableOutput("regionTable") %>% withSpinner(color = "green")

                ),

                tabItem(tabName = "charts",
                        h1(strong("Corridor Connectivity")),
                        p("To identify areas of the corridor that should be prioritized for conservation and restoration we modeled Grauer's gorillla connectivity between Kahuzi-Biega National Park and Itombwe Nature Reserve. To run the analysis we created a resistance layer to parameterize the cost of movement for Grauer's gorillas through each cell of the landscape, which can be seen on the map below. The analysis produces a least-cost path (LCP), least-cost corridor, pinchpoints, and barriers. The LCP is the least energetically costly route in terms of physical distance and resistance between the two protected areas and can be seen as the blue line on the map below. The least-cost corridor models the cost of movement across the entire landscape, with dark green indicating the least-costly areas of movement and red indicating the most-costly areas to move through on the map below. Pinch points are key locations where the flow of movement bottlenecks through a consitricted area, seen as red areas in the map below. Lastly, barriers represent areas in the landscape of a higher resistance that negatively impact movement, but if restored can improve overall connectivity. Barriers are idenified as the neon yellow circles, with the size of the circle corresponding to the physcial size of the barrier."),
                        br(),
                        fluidRow(
                            column(4, box(width = 10, status = "success", solidHeader = TRUE, title="Area Type:",
                                          radioButtons(inputId = "projectInput",
                                                       label = NULL,
                                                       choices = c("Resistance Layer", "Least-Cost Corridor", "Least-Cost Path", "Pinch Points", "Barriers"),
                                                       selected = "Resistance Layer")),
                                   tableOutput("table2")),
                            column(8, leafletOutput("networkBase") %>% withSpinner(color = "green"))
                        )

                ),

                tabItem(tabName = "socio",
                        h1(strong("Community Opinions")),
                        p("To determine the degree of local support for and primary community concerns about community forest management, we analyzed local communities' opinions about existing forest protections. The analysis focused on 3 sections: the natural resource section which examined sentiment on the importance of natural resources in community forest involvement, the governance section which gathered sentiment on interactions of communities with governing groups, and the knowledge of community forestry section which measured how communities grade their accessibility to resources. The results of the analysis can be explored in the heat map below."),
                        br(),
                        fluidRow(
                            column(4,
                                   box(width = 10, status = "success", solidHeader = TRUE, title="Choose Topic:",
                                       checkboxGroupInput(inputId = "mean_pick",
                                                          label = "Choose mean:",
                                                          choices = c("Road Condition Status" = "mean_138",
                                                                      "Agricultural Service Roads" = "mean_140",
                                                                      "Road Infrastructure Good Status" = "mean_143",
                                                                      "Road Infrastructure Bad Status" = "mean_144",
                                                                      "Vehicles in the Community Forest" = "mean_146",
                                                                      "Gender Violence" = "mean_192",
                                                                      "Regulatory Instruments" = "mean_107",
                                                                      "Community Involvement" = "mean_106",
                                                                      "Pressure on the RNI" = "mean_153",
                                                                      "Illegal Logging & Deforestation" = "mean_172"),
                                                          selected = c("Road Condition Status" = "mean_138",
                                                                       "Agricultural Service Roads" = "mean_140",
                                                                       "Road Infrastructure Good Status" = "mean_143",
                                                                       "Road Infrastructure Bad Status" = "mean_144",
                                                                       "Vehicles in the Community Forest" = "mean_146",
                                                                       "Gender Violence" = "mean_192",
                                                                       "Regulatory Instruments" = "mean_107",
                                                                       "Community Involvement" = "mean_106",
                                                                       "Pressure on the RNI" = "mean_153",
                                                                       "Illegal Logging & Deforestation" = "mean_172"))),
                                   tableOutput("table4")),
                            column(8, plotOutput(outputId = "heatmap_plot"))
                        )),

                tabItem(tabName = "climate",
                        h1(strong("Climate Change Projections")),
                        p("To identify areas that are likely to be resilient for Grauer's gorillas under future climate change we projected the future extent of suitable forest habitat under 2 climate change scenarios. The resulting maps of forest extent under RCP 4.5 and RCP 8.5 and resilient areas that remain suitable into the future can be explored on the map below for years 2050 and 2070."),
                        br(),
                        fluidRow(
                            column(4,
                                   box(width = 10, status = "success", solidHeader = TRUE, title="Area Type:",
                                       selectInput(inputId = "projectInput",
                                                   label = NULL,
                                                   choices = c("Current Year", "Year 2050", "Year 2070"),
                                                   selected = NULL)),
                                   tableOutput("table1")),
                            column(8,
                                   leafletOutput("climateBase") %>% withSpinner(color = "green")
                            )
                        ))

            )

        ) # end dashboardBody

    )# end dashboardPage

))


##########
##########
##########

server <- shinyServer(function (input, output) {

    # region map
    output$regionBase <- renderLeaflet({
        leaflet() %>%
            setView(lng = 28.45861, lat = -3.05, zoom = 8.5) %>%
            addProviderTiles(providers$Esri.WorldTopoMap)
    })


    #REGION TABLE
    table_select <- reactive({
        study %>%
            filter(mean %in% c(input$mean_pick))
    })

    # region table
    output$regionTable <- renderDataTable({

        study_region_table <- read_csv("study_region.csv") %>%
            clean_names() %>%
            select(area_ha:assisting_org)

    })


    # network map
    output$networkBase <- renderLeaflet({
        leaflet() %>%
            setView(lng = 28.45861, lat = -3.05, zoom = 8.5) %>%
            addProviderTiles(providers$Esri.WorldTopoMap)
    })

    # climate map
    output$climateBase <- renderLeaflet({
        leaflet() %>%
            setView(lng = 28.45861, lat = -3.05, zoom = 8.5) %>%
            addProviderTiles(providers$Esri.WorldTopoMap)
    })

    ###

    mean_select <- reactive({
        data_for_heat_long %>%
            filter(mean %in% c(input$mean_pick))
    })
    output$heatmap_plot <- renderPlot({
        ggplot(data = mean_select(), aes(x=mean_distance, y=mean, fill=score)) +
            geom_tile(color="white", size= 0.25) +
            labs(title = "Monica's Green Machine") +
            scale_fill_distiller(palette = "Greens", direction=1) +
            scale_y_discrete(expand = c(0,0)) +
            scale_x_discrete("Distance (meters)", labels = c("1000", "", "", "", "","","", "", "", "","","", "", "", "","", "", "", "","", "", "", "", "26000")) +
            coord_fixed() +
            theme_grey(base_size=8) +
            theme(
                axis.text = element_text(face="bold"),
                axis.ticks=element_line(size=0.4),
                plot.background = element_blank(),
                panel.border = element_blank())
    })

})


shinyApp(ui = ui, server = server)
