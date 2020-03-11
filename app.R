
# Libraries ---------------------------------------------------------------

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

# Time last commit of source file
curl::curl_download('https://api.github.com/repos/CSSEGISandData/COVID-19/commits?path=csse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Confirmed.csv&page=1&per_page=1', destfile = "temp.html")
last_commit = jsonlite::read_json("temp.html") 
date_last_commit_raw = last_commit[[1]]$commit$author$date
last_commit_time = strptime(date_last_commit_raw, "%FT%T", tz = "GMT")


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
                selected =  c("Italy", "Iran", "Spain", "South Korea", "France", "Germany", "US", "Japan", "Mainland China"))
    ), 

                
        # SHOW PLOT
        mainPanel(
            p("Data last update: ", as.character(last_commit_time), "GMT"),
            HTML(paste0("Using code from ",  
                        a(" @JonMinton", href="https://github.com/JonMinton/COVID-19"), " and ", 
                        a(" @christoph_sax", href="https://gist.github.com/christophsax/dec0a57bcbc9d7517b852dd44eb8b20b"), 
                        " this repo shows a simple visualization using the ", 
                        a(" @JHUSystems Coronavirus data", href="https://github.com/CSSEGISandData/COVID-19"), ".",
                        "<BR>See Github repo: ", a(" github.com/gorkang/2020-corona ", href="https://github.com/gorkang/2020-corona"))),
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
        
        ggplot(data = final_df(), aes(x = days_after_100, y = value, color = country)) +
            geom_line() + 
            geom_point() + 
            ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .3) + #, segment.linetype = 5 
            scale_y_log10(
                labels = function(x) format(x, big.mark = ",", scientific = FALSE)
                # breaks = scales::trans_breaks("log10", function(x) 10^x),
                # labels = scales::trans_format("log10", scales::math_format(10^.x))
                ) + 
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
        
    })
    
    # Show plot
    output$distPlot <- renderPlot({
            PLOT()
    })

        
}

# Run the application 
shinyApp(ui = ui, server = server)
