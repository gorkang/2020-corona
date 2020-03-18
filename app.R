
# Libraries ---------------------------------------------------------------

# library(curl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(httr)
library(readr)
library(rvest)
library(tidyr)
library(scales)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tabulizer)



# Data preparation --------------------------------------------------------

cases_deaths = "cases" #cases deaths

source(here::here("R/download_or_load.R"))
source(here::here("R/fetch_worldometers_safely.R"))
source(here::here("R/data-download.R"))
source(here::here("R/data-preparation.R"))

source(here::here("R/fetch_last_update_date.R"))



data_download()
data_preparation()

# Time last commit of source file
last_commit_time = fetch_last_update_date()$result


# UI ----------------------------------------------------------------------

ui <- 
    function(request) {
        fluidPage(

    titlePanel(windowTitle = "Coronavirus tracker",
               title = HTML("<a href=\"https://gorkang.shinyapps.io/2020-corona/\">Coronavirus tracker</a>")),
    theme = shinytheme("flatly"),
    
    sidebarLayout(
        sidebarPanel(
            width = 2,

    selectInput(inputId = 'countries_plot', 
                label = 'country',
                choices = V1_alternatives,
                multiple = TRUE, 
                selectize = TRUE, 
                width = 200, 
                selected = c(top_countries, "United Kingdom", "Denmark")),
    
    uiOutput('highlight2'),
    
    
    selectInput(
        inputId = "cases_deaths", label = "Cases or deaths", selected = "cases", 
        choices = c("cases", "deaths")),
    
    
    sliderInput("min_n", paste0("Minimum cases/deaths:"),
                min = 1, max = 200, value = 100),
    
    sliderInput("growth", "Daily growth (%):",
                min = 0, max = 100, value = 30),
    
    shinyWidgets::switchInput(inputId = "log_scale", label = "Log scale", 
                              value = TRUE, size = "mini", width = '100%'),
    
    hr(),
    # HTML("<BR>"),
    bookmarkButton(label = "Get URL"),
    hr(),
    
    HTML(paste0("Simple visualization using the ", a(" @JHUSystems Coronavirus", href="https://github.com/CSSEGISandData/COVID-19", target = "_blank"), 
                " and ", a("worldometers.info", href="https://www.worldometers.info/coronavirus/#countries", target = "_blank"), " data.",
                "<BR><BR>Using code and ideas from ",  
                a(" @JonMinton", href="https://github.com/JonMinton/COVID-19", target = "_blank"), ", ", 
                a(" @christoph_sax", href="https://gist.github.com/christophsax/dec0a57bcbc9d7517b852dd44eb8b20b", target = "_blank"), ", ",
                a(" @nicebread303", href="https://github.com/nicebread/corona", target = "_blank"), ", ",
                a("@jburnmurdoch", href="https://twitter.com/jburnmurdoch", target = "_blank"), " and ", a(" @sdbernard", href="https://twitter.com/sdbernard", target = "_blank"))),
    

    HTML("<BR><BR>"),
    HTML(paste0("By ", a(" @gorkang", href="https://twitter.com/gorkang", target = "_blank")))
    
    ), 

                
        # SHOW PLOT
        mainPanel(
            p(HTML(
                paste0(
                    a("Johns Hopkins Data", href="https://github.com/CSSEGISandData/COVID-19", target = "_blank"), " updated on: ", as.character(last_commit_time), " GMT",
                    "<BR>Final big point from ", a("worldometers.info", href="https://www.worldometers.info/coronavirus/#countries", target = "_blank"), ": ", as.POSIXct(time_worldometer, format = "%B %d, %Y, %H:%M", tz = "GMT"), "GMT")
                # "<BR>Chilean latest data: ", a("minsal.cl", href="https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/"), ": ", gsub("-Casos-confirmados.pdf", "", "filename_minsal")
                )
              ),
            HTML(paste0("Github repo: ", a(" github.com/gorkang/2020-corona ", href="https://github.com/gorkang/2020-corona", target = "_blank"))),
            hr(),
           plotOutput("distPlot", height = "700px", width = "100%"),
           
           hr(),
           
           HTML(
               paste(
                   h3("Data shown in plot"),
                   
                   a("Johns Hopkins Data", href="https://github.com/CSSEGISandData/COVID-19"), 
                        " updated on: ", as.character(last_commit_time), " GMT",
                   "<BR>Final big point from ", a("worldometers.info", href="https://www.worldometers.info/coronavirus/#countries"), ": ", 
                        as.POSIXct(time_worldometer, format = "%B %d, %Y, %H:%M", tz = "GMT"), "GMT"
                   # "<BR>Chilean latest data: ", a("minsal.cl", href="https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/")
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

    setBookmarkExclude(
        c('mytable_rows_current',
          'mytable_rows_all',
          'mytable_state',
          'mytable_search_columns',
          'mytable_search',
          'mytable_cell_clicked',
          'mytable_rows_selected'))
    
    # Dinamically set highlight choices bases on input$countries_plot
    outVar = reactive({ c(input$countries_plot, "None") })
    output$highlight2 = renderUI({
        selectInput('highlight', 'highlight country', choices = outVar(),
                    selected = "None")
    })
    
    final_df = reactive({ 

        withProgress(message = 'Preparing data', value = 0, {
            
            req(input$highlight)
            
            # Run data preparation foe either cases or deaths
            if (input$cases_deaths == "deaths") {
                data_preparation(cases_deaths = input$cases_deaths)
            } else if (input$cases_deaths == "cases") {
                data_preparation(cases_deaths = input$cases_deaths)
            }
            
            if (!is.null(input$countries_plot)) {
                
                dta_temp = dta %>%
                    # selection
                    filter(country %in% input$countries_plot) %>% 
                    
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
                    mutate(days_after_100 = as.numeric(0:(length(country) - 1)),
                           days_after_100 = 
                               case_when(
                                   source == "worldometers" ~ lag(days_after_100) + round(as.POSIXlt(as.POSIXct(time_worldometer, format = "%B %d, %Y, %H:%M", tz = "GMT"))$hour/24, 2), #.1
                                   TRUE ~ days_after_100
                               )) %>% 
                    group_by(country) %>%
                    mutate(
                        name_end =
                            case_when(
                                days_after_100 == max(days_after_100) ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
                                TRUE ~ ""))
                    
                if (input$highlight != "None") {
                    dta_temp %>% 
                        mutate(highlight = 
                                   case_when(
                                       country == input$highlight ~ "red",
                                       TRUE ~ "grey"
                                   ))
                } else {
                    dta_temp %>% mutate(highlight = country)
                }
                
                
            } else {
                tibble(value = 0, 
                       days_after_100 = 0,
                       country = "",
                       source = "",
                       name_end = "")
                
            }
        })
    })
    

    growth_line = reactive({
        # We use 1.1 * to avoid overlaping
        tibble(
            value = cumprod(c(input$min_n, rep((100 + input$growth) / 100, 1.1 * max(final_df()$days_after_100, na.rm = TRUE)))),
            days_after_100 = 0:(1.1 * max(final_df()$days_after_100, na.rm = TRUE))
        )
    })
    
    PLOT = reactive({
        
        withProgress(message = 'Loading plot', value = 0, {
            
            p_temp = ggplot(data = final_df(), aes(x = days_after_100, y = value, group = country, color = highlight)) +
                geom_line(data = growth_line(),
                          aes(days_after_100, value),
                          linetype = "dotted", inherit.aes = FALSE) +
                geom_line() + 
                geom_point(aes(size = as.integer(final_df()$name_end != ""))) + 
                ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .3) + 
                scale_x_continuous(breaks = seq(0, max(final_df()$value), 2)) +
                labs(
                    title = paste0("Confirmed ", input$cases_deaths ,""),
                    subtitle = paste0("Arranged by number of days since ",  input$min_n ," or more ", input$cases_deaths),
                    x = paste0("Days after ",  input$min_n ," confirmed ", input$cases_deaths),
                    y = paste0("Confirmed ", input$cases_deaths, " (log scale)"), 
                    caption = paste0("Source: Johns Hopkins CSSE\nFinal big point: worldometers.info")
                ) +
                theme_minimal(base_size = 14) +
                theme(legend.position = "none")
            
            if (input$highlight != "None") {p_temp =  p_temp + scale_color_identity()}
            
            if (input$log_scale == TRUE) {
                p_temp2 = p_temp +
                    scale_y_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) 
                    
            } else {
                p_temp2 = p_temp +
                    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
                    labs(y = paste0("Confirmed ", input$cases_deaths))
            }
                
            p_temp2 + 
                annotate(geom = "text",
                         x = max(growth_line()$days_after_100) - .5, 
                         y = max(growth_line()$value), 
                         vjust = 1, 
                         hjust = 1, 
                         label = paste0(input$growth, "% growth"))
        })
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
shinyApp(ui = ui, server = server, enableBookmarking = "url") # Not woking yet in shinyapps.io :( Error bookmarking state: This server is not configured for saving sessions to disk.
