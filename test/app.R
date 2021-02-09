library(tidyverse)
library(shiny)
library(bslib)
library(shinythemes)
library(leaflet)
library(janitor)
library(lubridate)

corridor <- read_csv("Corridor.csv") %>%
    clean_names()

projects_all <- read_csv("project_df.csv") %>%
    clean_names() %>%
    mutate(date = lubridate::mdy(date))


SR_theme <- bs_theme(
    bg = "white",
    fg = "green",
    primary = "forestgreen"
)


# ui
ui <- fluidPage(theme = SR_theme,

                navbarPage(title = div(id = "img-id",
                                       img(src = "logo.png",
                                           height = "100"),
                                       "Strong Roots Congo Project Website"),


                           tabPanel("About",
                                    sidebarLayout(
                                        sidebarPanel(id = "img-id",
                                                     img(src = "logo.png",
                                                         height = "100")),
                                        mainPanel(
                                            p("Strong Roots Congo is a grassroots conservation and sustinable development organization
                                              operating in the eastern Democratic Republic on Congo. Srong Roots works with local and
                                              international partners to conserve Great Apes and empower local and indigenous communities.

                                               Vision: To see local communities, indigenous people, and wildlife thrive together in the eastern DRC.

                                              Mission: To conserve Grauer's gorillas and other wildlife by enganing local and indigenous people in conservation
                                              while improving well-being.")
                                        ))),

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

