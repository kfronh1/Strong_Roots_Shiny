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

### WRANGLING

study_region_table <- read_csv("study_region.csv") %>%
    clean_names() %>%
    select(area_ha:assisting_org)


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
                                      tabName = "choropleth",
                                      icon = icon("sun"))

                         )), # end dashboardSidebar

        dashboardBody(

            tabItems(

                tabItem(tabName = "home",
                        includeMarkdown("www/delete.Rmd")
                ),

                tabItem(tabName = "map",
                        h1(strong("Study Region")),
                        p("The study region is located in the eastern Democratic Republic of Congo. On the map, once can locate Kahuzi-Beiga National Park and Itombwe Nature Reserve. The two largest remaining populations of Grauer's gorillas are located in those two protected areas. Thus, Strong Roots Congo is working with local communities to legally designate 7 community forests, which can also be displayed on the map. To improve connecvity of the landscape Strong Roots Congo has also designated reforestation zones along the borders of the entire community forest network, which can be displayed by choosing Reforestation Zone."),
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
                        p("Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again. Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again. Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again. Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again."),
                        br(),
                        fluidRow(
                            column(4, box(width = 10, status = "success", solidHeader = TRUE, title="Area Type:",
                                          radioButtons(inputId = "projectInput",
                                                       label = NULL,
                                                       choices = c("Resistance Raster", "Connectivity Layer", "Barrier Map", "Least Cost Path"),
                                                       selected = "Resistance Raster")),
                                   tableOutput("table2")),
                            column(8, leafletOutput("networkBase") %>% withSpinner(color = "green"))
                        )

                ),

                tabItem(tabName = "socio",
                        h1(strong("Community Opinions")),
                        p("Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again. Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again. Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again. Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again."),
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

                tabItem(tabName = "choropleth",
                        h1(strong("Climate Change Projections")),
                        p("Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again. Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again. Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again. Here there would be a description about what this project map shows. Now to show what this would look like as a paragrpah, we're going to just copy and paste these two sentences over and over again."),
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
                                   leafletOutput("choroplethCategoriesPerState") %>% withSpinner(color = "green")
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
