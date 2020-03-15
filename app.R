
# Libraries ---------------------------------------------------------------

# library(curl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(httr)
library(readr)
library(tidyr)
library(scales)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tabulizer)



# Data preparation --------------------------------------------------------

source(here::here("R/data-preparation.R"))
source(here::here("R/join_latest_chile.R"))

# Time last commit of source file
source(here::here("R/fetch_last_update_data.R"))
last_commit_time = fetch_last_update_data()$result


# UI ----------------------------------------------------------------------

ui <- 
    function(request) {
        fluidPage(

    titlePanel("2020 - Coronavirus"),
    theme = shinytheme("flatly"),
    
    sidebarLayout(
        sidebarPanel(
            width = 2,

    selectInput(inputId = 'countries_plot', 
                label = 'country',
                choices = V1_alternatives,
                multiple = TRUE, selectize = TRUE, width = 200, 
                selected = c(top_countries, "United Kingdom", "Denmark", "Australia")),
    
    sliderInput("min_n", "Min number of cases:",
                min = 10, max = 200, value = 100),
    
    sliderInput("growth", "Daily growth (%):",
                min = 0, max = 100, value = 30),
    
    shinyWidgets::switchInput(inputId = "log_scale", label = "Log scale", 
                              value = TRUE, size = "mini", width = '100%'),
    
    hr(),
    
    HTML(paste0("Using code from ",  
                a(" @JonMinton", href="https://github.com/JonMinton/COVID-19"), " and ", 
                a(" @christoph_sax", href="https://gist.github.com/christophsax/dec0a57bcbc9d7517b852dd44eb8b20b"), 
                ", this repo shows a simple visualization using the ", 
                a(" @JHUSystems Coronavirus data", href="https://github.com/CSSEGISandData/COVID-19"), ".",
                "<BR>Growth line and Min number of cases ideas from" , a(" @nicebread303", href="https://github.com/nicebread/corona"))),
    
    HTML("<BR><BR>"),
    bookmarkButton(label = "Get URL")
    
    ), 

                
        # SHOW PLOT
        mainPanel(
            p(HTML(
                paste0(
                    a("Johns Hopkins Data", href="https://github.com/CSSEGISandData/COVID-19"), " updated on: ", as.character(last_commit_time), " GMT",
                    "<BR>Final big point from ", a("worldometers.info", href="https://www.worldometers.info/coronavirus/#countries"), ": ", as.POSIXct(time_worldometer, format = "%B %d, %Y, %H:%M", tz = "GMT"), "GMT",
                    "<BR>Chilean latest data: ", a("minsal.cl", href="https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/"), ": ", gsub("-Casos-confirmados.pdf", "", filename_minsal) 
                ))),
            HTML(paste0("Github repo: ", a(" github.com/gorkang/2020-corona ", href="https://github.com/gorkang/2020-corona"))),
            hr(),
           plotOutput("distPlot", height = "700px", width = "100%"),
           
           hr(),
           
           HTML(
               paste(
                   h3("Data shown in plot"),
                   
                   a("Johns Hopkins Data", href="https://github.com/CSSEGISandData/COVID-19"), 
                        " updated on: ", as.character(last_commit_time), " GMT",
                   "<BR>Final big point from ", a("worldometers.info", href="https://www.worldometers.info/coronavirus/#countries"), ": ", 
                        as.POSIXct(time_worldometer, format = "%B %d, %Y, %H:%M", tz = "GMT"), "GMT",
                   "<BR>Chilean latest data: ", a("minsal.cl", href="https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/")
                   )
           ),
           hr(),
           
           div(
               DT::dataTableOutput("mytable", width = "100%"), 
               align = "center"
           )
           
        )
    )
)
}

# Server ------------------------------------------------------------------

server <- function(input, output) {

    final_df = reactive({ 
        
        dta %>%
            # selection
            filter(country %in% !! input$countries_plot) %>% 
            # filter
            filter(value >= input$min_n) %>% 
            
            # If repeated values the same day, keep higher
            group_by(country, time) %>% 
            distinct(KEY = paste0(country, time, value), .keep_all = TRUE) %>% 
            select(-KEY) %>% 
            # top_n(n = 1, wt = value) %>% 
            ungroup() %>% 
        
            # re-adjust after filtering
            group_by(country) %>%
            mutate(days_after_100 = 0:(length(country) - 1)) %>% 
            group_by(country) %>%
            mutate(
                name_end =
                    case_when(
                        days_after_100 == max(days_after_100) ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
                        TRUE ~ ""))

            
    })
    
    growth_line = reactive({
        
        # tibble(value = cumprod(c(input$min_n, rep(paste0(1, ".", sprintf("%02d", as.numeric(input$growth))), max(final_df()$days_after_100)))),
        tibble(value = cumprod(c(input$min_n, rep((100 + input$growth)/100, max(final_df()$days_after_100)))),
               days_after_100 = 0:max(final_df()$days_after_100))
    })
    
    PLOT = reactive({
        
        p_temp = ggplot(data = final_df(), aes(x = days_after_100, y = value, color = country)) +
            geom_line() + 
            geom_point(aes(size = source)) + 
            ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .3) + #, segment.linetype = 5 
            scale_x_continuous(breaks = seq(0, max(final_df()$value), 2)) +
            labs(
                title = paste0("Confirmed cases after first ",  input$min_n ," cases"),
                subtitle = paste0("Arranged by number of days since ",  input$min_n ," or more cases"),
                x = paste0("Days after ",  input$min_n ," confirmed cases"),
                y = "Confirmed cases (log scale)", 
                caption = paste0("Source: Johns Hopkins CSSE\nFinal big point: worldometers.info\nChilean latest: minsal.cl ")
            ) +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none")
        
        if (input$log_scale == TRUE) {
            p_temp2 = p_temp +
                scale_y_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) 
                
        } else {
            p_temp2 = p_temp +
                scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
                labs(y = "Confirmed cases")
        }
            
        p_temp2 + 
            geom_line(data = growth_line(),
                      aes(days_after_100, value),
                      linetype = "dotted", inherit.aes = FALSE) +
            annotate(geom = "text",
                     x = max(final_df()$days_after_100) - 1, 
                     y = max(growth_line()$value), 
                     vjust = 1, 
                     hjust = 1, 
                     label = paste0(input$growth, "% growth"))
        
    })
    
    # Show plot
    output$distPlot <- renderPlot({
            PLOT()
    })

        
    # Show table
    output$mytable = DT::renderDataTable({
        DT::datatable(final_df() %>%
                          arrange(desc(time), country) %>% 
                          select(-name_end) %>% 
                          rename_(.dots=setNames("days_after_100", paste0("days_after_", input$min_n))), 
                      filter = 'top',
                      rownames = FALSE, 
                      options = list(pageLength = 10, 
                                     dom = 'ltipr',
                                     autoWidth = FALSE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server")
