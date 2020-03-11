library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(tidyr)
library(scales)



# Data preparation --------------------------------------------------------

source(here::here("R/data-preparation.R"))

V1_alternatives = dta %>%
    filter(value > 100) %>% 
    distinct(country) %>% pull(country)


ui <- fluidPage(

    titlePanel("2020 - CORONAVIRUS"),
    theme = shinytheme("flatly"),
    
    sidebarLayout(
        sidebarPanel(
            width = 2,

    # Sidebar with a slider input for number of bins 
    selectInput(inputId = 'countries_plot', 
                label = 'country',
                choices = V1_alternatives,
                multiple = TRUE, selectize = TRUE, width = 200, 
                selected =  c("Italy", "Iran", "Spain", "South Korea", "France", "Germany", "US", "Japan", "Mainland China"))
    ), 

                
        # SHOW PLOT
        mainPanel(
            # h3("2020 - CORONAVIRUS"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    final_df = reactive({ 
        
        dta %>%
            # selection
            filter(country %in% !! input$countries_plot)
    })
    
    
    PLOT = reactive({
        
        ggplot(data = final_df(), aes(x = days_after_100, y = value, color = country)) +
            geom_line() + 
            geom_point() + 
            ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .3) + #, segment.linetype = 5 
            scale_y_log10(
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
            scale_x_continuous(breaks = seq(0, max(final_df()$value), 2)) +
            labs(
                title = "Confirmed cases after first 100 cases",
                subtitle = "Arranged by number of days since 100 or more cases",
                x = "Days after 100 confirmed cases",
                y = "Confirmed cases (log scale)", 
                caption = "Source: Johns Hopkins CSSE"
            ) +
            theme_minimal() +
            theme(legend.position = "none")
        
    })
    
    # Show plot
    output$distPlot <- renderPlot({
            PLOT()
    })
    
    output$tableSHOW <- renderTable({
        final_df()
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
