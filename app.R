library(tidyverse)
library(shiny)
library(bslib)
library(shinythemes)

SR_theme <- bs_theme(
    bg = "forestgreen",
    fg = "white"
)

# ui
ui <- fluidPage(theme = SR_theme,

                navbarPage("Strong Roots Congo Project Website",
                           tabPanel("About"),

                           tabPanel("Projects",
                                    sidebarLayout(
                                        sidebarPanel("Projects",
                                                     checkboxGroupInput(inputId = "pick_type",
                                                                        label = "Choose project:",
                                                                        choices = unique(corridor$type))
                                        ),
                                        mainPanel("Output",
                                                  plotOutput("sr_plot"))
                                    )
                           ),



                           tabPanel("Project Impact")

                )

)

server <- function(input, output) {

    sr_reactive <- reactive({

        corridor %>%
            filter(type %in% input$pick_type)
    })

    # Create output / renderTable etc

    output$sr_plot <- renderPlot(
        ggplot(data = sr_reactive(), aes(x = area_sq, y = area_ha)) +
            geom_point(aes(color = type))
    )

}

shinyApp(ui = ui, server = server)

