
# Libraries ---------------------------------------------------------------

library(curl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(tidyr)
library(scales)
library(shiny)
library(shinythemes)
library(shinyWidgets)



# Data preparation --------------------------------------------------------

source(here::here("R/data-preparation.R"))

V1_alternatives = dta %>%
    filter(value > 100) %>% 
    distinct(country) %>% pull(country)

top_countries = dta %>%
    group_by(country) %>% 
    filter(value == max(value), 
           country != "Cruise Ship") %>% 
    distinct(country, value) %>% 
    ungroup() %>%
    top_n(n = 10, wt = value) %>% 
    pull(country)


# Time last commit of source file
source(here::here("R/fetch_last_update_data.R"))
last_commit_time = fetch_last_update_data()$result


# UI ----------------------------------------------------------------------

ui <- fluidPage(

    titlePanel("2020 - Coronavirus"),
    theme = shinytheme("flatly"),
    
    sidebarLayout(
        sidebarPanel(
            width = 2,

    # Sidebar with a slider input for number of bins 
    selectInput(inputId = 'countries_plot', 
                label = 'country',
                choices = V1_alternatives,
                multiple = TRUE, selectize = TRUE, width = 200, 
                selected = c(top_countries, "United Kingdom", "Denmark") 
                    # c("China", "Denmark", "France", "Germany", "Italy", "Iran", "Japan", "South Korea", "Spain", "US", "United Kingdom")
                ),
    
    shinyWidgets::switchInput(inputId = "log_scale", label = "Log scale", value = TRUE, size = "mini", width = '100%'),
    
    hr(),
    
    HTML(paste0("Using code from ",  
                a(" @JonMinton", href="https://github.com/JonMinton/COVID-19"), " and ", 
                a(" @christoph_sax", href="https://gist.github.com/christophsax/dec0a57bcbc9d7517b852dd44eb8b20b"), 
                " this repo shows a simple visualization using the ", 
                a(" @JHUSystems Coronavirus data", href="https://github.com/CSSEGISandData/COVID-19"), "."))
         
    ), 

                
        # SHOW PLOT
        mainPanel(
            p(HTML(paste0(a("Data", href="https://github.com/CSSEGISandData/COVID-19"), " updated on: ", as.character(last_commit_time), "GMT"))),
            # p(HTML(paste0(a("Data", href="https://github.com/CSSEGISandData/COVID-19")))),
            HTML(paste0("Github repo: ", a(" github.com/gorkang/2020-corona ", href="https://github.com/gorkang/2020-corona"))),
            hr(),
           plotOutput("distPlot", height = "700px", width = "100%")
        )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {

    final_df = reactive({ 
        
        dta %>%
            # selection
            filter(country %in% !! input$countries_plot)
    })
    
    
    PLOT = reactive({
        
        p_temp = ggplot(data = final_df(), aes(x = days_after_100, y = value, color = country)) +
            geom_line() + 
            geom_point() + 
            ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .3) + #, segment.linetype = 5 
            scale_x_continuous(breaks = seq(0, max(final_df()$value), 2)) +
            labs(
                title = "Confirmed cases after first 100 cases",
                subtitle = "Arranged by number of days since 100 or more cases",
                x = "Days after 100 confirmed cases",
                y = "Confirmed cases (log scale)", 
                caption = "Source: Johns Hopkins CSSE"
            ) +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none")
        
        if (input$log_scale == TRUE) {
            p_temp + 
                scale_y_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE))
        } else {
            p_temp +
                scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
                labs(y = "Confirmed cases")
        }
            
        
    })
    
    # Show plot
    output$distPlot <- renderPlot({
            PLOT()
    })

        
}

# Run the application 
shinyApp(ui = ui, server = server)
