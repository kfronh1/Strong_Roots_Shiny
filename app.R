library(raster)
library(tidyverse)
library(shiny)
library(bslib)
library(shinythemes)
library(leaflet)
library(janitor)
library(lubridate)
library(here)

corridor <- read_csv("Corridor.csv") %>%
    clean_names()

projects_all <- read_csv("project_df.csv") %>%
    clean_names() %>%
    mutate(date = lubridate::mdy(date))

barriers <- here('barriers.tif')
barrier_rast <- raster(barriers)

SR_theme <- bs_theme(
    bg = "white",
    fg = "green",
    secondary = "forestgreen"
)


# ui
ui <- fluidPage(theme = SR_theme,

                navbarPage(title = div(id = "img-id",
                                           img(src = "logo.png",
                                               height = "100"),
                                       "Strong Roots Congo Project Directory"),

                           tabPanel("About",
                                    headerPanel("Strong Roots Mission & Vision"),
                                    h5("Strong Roots Congo empowers local communities by providing knowledge, tools and opportunities for a sustainable way of life while also supporting the long term preservation of endemic species.",
                                       style = "margin: 5px 20px"),
                                    br(),
                                    splitLayout(cellWidths = c("50%", "50%"),
                                                div(
                                                    img(src = "gorilla1.jpg",
                                                        width = "350",
                                                        style = "vertical-align:middle;margin:5px 50px"),
                                                    br(),
                                                    br(),
                                                    p("Strong Roots is working to create a corridor of community forests that connect the Kahuzi-Biega National Park and the Itombwe Nature Reserve. Once designated by the government as a community forest, the corridor would promote the long-term connectivity and protection of Grauer's gorilla populations in the region. The pilot project requires thecombinationof multiple smaller strategies ranging from community education to species mapping to climate modeling.",
                                                      style = "margin: 20px 20px"),
                                                    ),
                                                div(h5("Vision:"),
                                                    p("To see local communities, indigenous peoples, and wildlife thrive together in the Eastern Democratic Republic of the Congo."),
                                                    br(),
                                                    h5("Mission:"),
                                                    p("To simultaneously conserve gorilla populations and support local communities through the empowerment and engagement of local indigenous peoples and communities in conservation practices."),
                                                    br(),
                                                    br(),
                                                    img(src = "child.jpg",
                                                        width = "350",
                                                        style = "vertical-align:middle;margin:5px 50px"))
                                    ),
                                    br(),
                                    br(),
                                    h2("Contact Strong Roots"),
                                    br(),
                                    h4("Website"),
                                    a("www.strongrootscongo.org",
                                      href = "www.strongrootscongo.org"),
                                    h4("Founder & Executive Director"),
                                    p("Dominique Bikaba, bikaba@strongrootscongo.org"),
                                    actionButton(inputId = "donate",
                                                 label = "Donate to Stronng Roots",
                                                 icon = NULL)
                                    ),


                           tabPanel("Projects",
                                    sidebarLayout(
                                        sidebarPanel("Projects",
                                                     checkboxGroupInput(inputId = "pick_type",
                                                                        label = "Choose project:",
                                                                        choices = unique(corridor$type)),
                                                     sliderInput(inputId = "pick_date",
                                                                 label = "Date Range:",
                                                                 min = as.Date("2009-01-01","%Y-%m-%d"),
                                                                 max = as.Date("2021-01-01","%Y-%m-%d"),
                                                                 value = as.Date(c("2009-01-01","2021-01-01"),"%Y"),
                                                                 step = NULL,
                                                                 round = FALSE,
                                                                 timeFormat = "%Y",
                                                                 locale = "us",
                                                                 ticks = TRUE)
                                        ),
                                        mainPanel("Output",
                                                  plotOutput("sr_plot"))
                                    )
                           ),

                           tabPanel("Project Impact",
                                    sidebarLayout(
                                        sidebarPanel("Projects",
                                                     radioButtons(inputId = "pick_project",
                                                                  label = "Choose project:",
                                                                  choices = unique(projects_all$project),
                                                                  selected = NULL),
                                                     sliderInput(inputId = "pick_date",
                                                                 label = "Date Range:",
                                                                 min = as.Date("2009-01-01","%Y-%m-%d"),
                                                                 max = as.Date("2021-01-01","%Y-%m-%d"),
                                                                 value = as.Date(c("2009-01-01","2021-01-01"),"%Y"),
                                                                 step = NULL,
                                                                 round = FALSE,
                                                                 timeFormat = "%Y",
                                                                 locale = "us",
                                                                 ticks = TRUE),
                                        ),
                                        mainPanel("Output",
                                                  plotOutput("impact_plot"))
                                    ))

                )

)

server <- function(input, output) {

    sr_reactive <- reactive({

        corridor %>%
            filter(type %in% input$pick_type)

    })

    output$sr_plot <- renderPlot(
        ggplot(data = sr_reactive(), aes(x = area_sq, y = area_ha)) +
            geom_point(aes(color = type))
    )

    # For Project Impact tab

    impact_reactive <- reactive({

        projects_all  %>%
           # filter(date %in% input$pick_date) %>%
            filter(project %in% input$pick_project)

    })

    output$impact_plot <- renderPlot(
        ggplot(data = impact_reactive(), aes(x = date, y = area_hct)) +
            geom_point()
    )



}

shinyApp(ui = ui, server = server)

