# options(shiny.reactlog = TRUE) 

# Libraries ---------------------------------------------------------------

library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(httr)
library(jsonlite)
library(readr)
library(rvest)
library(tidyr)
library(scales)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(vroom)



# Data preparation --------------------------------------------------------

cases_deaths = "deaths" #cases deaths

source(here::here("R/download_or_load.R"))
source(here::here("R/download_or_load_JH_API.R"))

source(here::here("R/fetch_worldometers_safely.R"))
source(here::here("R/data-download.R"))
source(here::here("R/data-preparation.R"))

source(here::here("R/data-preparation-menu.R"))

source(here::here("R/fetch_last_update_date.R"))


# Launch data_download 
minutes_to_check_downloads = 30 # Every x minutes
auto_invalide <- reactiveTimer(minutes_to_check_downloads * 60 * 1000) 

file_info_JHU <- file.info("outputs/raw_JH.csv")$mtime
time_worldometer <<- stringr::str_extract(string = html_text(read_html(here::here("outputs/temp_worldometers.html"))),
                                          pattern = '\\w+\\s\\d+(st)?(nd)?(rd)?(th)?,\\s+\\d+, \\d+:\\d+ GMT')


# UI ----------------------------------------------------------------------

ui <- 
    function(request) {
        fluidPage(
            tags$head(includeHTML(("google-analytics.html"))),
            useShinyjs(),
            
        titlePanel(
            windowTitle = "Coronavirus Tracker - Facultad de Psicología - UAI",
            fluidRow(
                column(9, HTML("<a href=\"https://gorkang.shinyapps.io/2020-corona/\">Coronavirus tracker</a>")), 
                column(1, HTML("<a href=\"http://psicologia.uai.cl/\", target = \"_blank\"><img src=\"UAI_mini.png\", alt ='Universidad Adolfo Ibáñez'></a>"))
            )
        ),
    theme = shinytheme("flatly"),
    
    sidebarLayout(
        sidebarPanel(
            width = 2,

            div(
             
            HTML(paste0(
                a(img(src = "github_small.png", title = "Github repository"), href="https://github.com/gorkang/2020-corona", target = "_blank"), "&nbsp;&nbsp;",
                a(img(src = "issue_small.png", title = "Report an issue!"), href="https://github.com/gorkang/2020-corona/issues", target = "_blank"), "&nbsp;&nbsp;",
                a(img(src = "twitter_small.png", title = "@gorkang"), href="https://twitter.com/gorkang", target = "_blank"), "&nbsp;&nbsp;", 
                a(img(src = "https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg", title = "Buy me a coffee", height = "26px"), href="https://www.buymeacoffee.com/I0rkAbM", target = "_blank"), "&nbsp;", 
                "<BR><BR>")),
            align = "center"
            ),

    selectInput(inputId = 'countries_plot', 
                label = 'Country',
                choices = V1_alternatives,
                multiple = TRUE, 
                selectize = TRUE, 
                width = "100%", 
                selected = c(top_countries, "United Kingdom", "Denmark", "Chile")),
    
    selectInput(inputId = 'highlight', 
                label = 'Highlight countries',
                choices = c(top_countries, "United Kingdom", "Denmark", "Chile"),
                multiple = TRUE, 
                selectize = TRUE, 
                width = "100%"),
    
    selectInput(inputId = "cases_deaths", label = "Cases or deaths", selected = "deaths", 
                 choices = c("cases", "deaths", "CFR")),

    radioButtons(inputId = "accumulated_daily_pct", label = "Accumulated, daily or %", selected = "accumulated", 
                 choices = c("accumulated", "daily", "%"), inline = TRUE),
    
    # Dynamically change with cases_deaths
    sliderInput('min_n_cases', paste0("Day 0 after ___ accumulated cases"), min = 1, max = 1000, value = 100), 
    sliderInput('min_n_deaths', paste0("Day 0 after ___ accumulated deaths"), min = 1, max = 500, value = 10),
    sliderInput('min_n_CFR', paste0("Day 0 after ___ accumulated deaths"), min = 1, max = 500, value = 10),
    
    # Dynamically change with accumulated_daily_pct
    sliderInput("growth_accumulated", "Daily growth (%):", min = 0, max = 100, value = 30),
    sliderInput("growth_daily", "Daily growth (%):", min = 0, max = 100, value = 20),
    sliderInput("growth_pct", "Daily growth (%):", min = -50, max = 0, value = -10),
    
    HTML("<BR>"),
    
    div(style="display:inline-block;width;45%;text-align: center;",
        shinyWidgets::switchInput(inputId = "log_scale", label = "Log", value = TRUE, size = "mini", labelWidth = "45%")
        ), 
    HTML("&nbsp;&nbsp;"),
    div(style="display:inline-block;45%;text-align: center;",
        shinyWidgets::switchInput(inputId = "smooth", label = "Smooth", value = FALSE, size = "mini", labelWidth = "45%")
        ),
    
    # RELATIVE
    div(style="display:inline-block;45%;text-align: center;",
        HTML("&nbsp;&nbsp;"),
        
    shinyWidgets::switchInput(inputId = "relative", label = "Relative/million", value = FALSE, size = "mini", labelWidth = "80%")
    ),
    HTML("<BR><BR>"),
    
    div( HTML("&nbsp;&nbsp;"), style="display:inline-block;65%;text-align: center;",
        bookmarkButton(label = "URL")
    ), 
    HTML("&nbsp;&nbsp;"),
    div(style="display:inline-block;30%;text-align: center;",
        downloadButton('downloadPlot', 'Plot')
    ),
    
    HTML("<BR><BR>"),
    
    uiOutput("WARNING"),
    
    hr(),
    
    HTML(paste0("Using code and ideas from ",  
            a("@JonMinton", href="https://github.com/JonMinton/COVID-19", target = "_blank"), ", ", 
            a("@christoph_sax", href="https://gist.github.com/christophsax/dec0a57bcbc9d7517b852dd44eb8b20b", target = "_blank"), ", ",
            a("@nicebread303", href="https://github.com/nicebread/corona", target = "_blank"), ", ",
            a("@rubenivangaalen", href="https://twitter.com/rubenivangaalen", target = "_blank"), ", ",
            a("@jburnmurdoch", href="https://twitter.com/jburnmurdoch", target = "_blank"), " and ", a(" @sdbernard", href="https://twitter.com/sdbernard", target = "_blank"))),
    
    ), 

                
        # SHOW PLOT
        mainPanel(
            p(HTML(
                paste0(
                    a("Johns Hopkins Data", href="https://covid19api.com/", target = "_blank"), " updated on: ", as.character(file_info_JHU), " GMT",
                    "<BR>", a("worldometers.info", href="https://www.worldometers.info/coronavirus/#countries", target = "_blank"), " (last point) updated on: ", as.POSIXct(time_worldometer, format = "%B %d, %Y, %H:%M", tz = "GMT"), "GMT"
                    )
                )
              ),
            
            hr(),
            
            plotOutput("distPlot", height = "800px", width = "100%"),
            
            hr(),
           
            h3("Data shown in plot ", downloadButton('downloadData', '')),
            
            hr(),
            
            div(
                DT::dataTableOutput("mytable", width = "100%"), 
                align = "center"
                ),
            
            hr(),
            span(
                div(
                    HTML(paste0("Please always check oficial sources (e.g. ", a("WHO", href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019", target = "_blank"), "), and be careful when using this or other information to create predictive models. ",
                                 "By ", a("@gorkang", href="https://twitter.com/gorkang", target = "_blank"))),
                    align = "center", 
                    style = "color:darkgrey")),
            hr()
            )
        )
    )
}


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

    setBookmarkExclude(
        c('mytable_rows_current',
          'mytable_rows_all',
          'mytable_state',
          'mytable_search_columns',
          'mytable_search',
          'mytable_cell_clicked',
          'mytable_rows_selected'))
    
    

    # WARNING -----------------------------------------------------------------
    output$WARNING <- renderUI({
        if (input$cases_deaths == "cases") {
            span(h6("REMEMBER, number os cases is not a trusworthy measure: ", br(), br(), "Number of cases is not equivalent to infections. It is limited by number of tests, and most countries are not testing enough. ", br(), br(), "Number of cases are not directly comparable (countries employ different testing strategies)."),
                 style = "color:darkred")
        } else if (input$cases_deaths == "deaths") {
            span(h6("REMEMBER: ", br(), br(), "Countries count deaths in different ways (e.g. Some count deaths at hospitals but not at nursing homes and other count both)."),
                 style = "color:darkred")
        } else if (input$cases_deaths == "CFR") {
            span(h6("REMEMBER: ", br(), br(), "Case Fatality Rate (CFR) = deaths / cases.", br(), br(), "Given the number of cases is not trustworthy (e.g. not enough tests), CFR is generally not a precise measure (usually an overestimation)."),
                 style = "color:darkred")
        }
    })

    
    # Launch data downloading -------------------------------------------------

    observe({
        withProgress(message = 'Downloding or loading data', value = 1, min = 0, max = 4, {
            
        auto_invalide()
        message("\n\n* CHECKING IF WE HAVE TO DOWNLOAD DATA ---------------------- \n")
        data_download()
        
        })
    })
    
    

    # Debounce critical vars --------------------------------------------------

    INPUT_countries_plot = reactive({ input$countries_plot })
    INPUT_countries_plot_debounced <- debounce(INPUT_countries_plot, 500)

    INPUT_highlight = reactive({ input$highlight })
    INPUT_highlight_debounced <- debounce(INPUT_highlight, 500)
    
    
    
    # Dynamic menus -----------------------------------------------------------
    
    observeEvent(input$cases_deaths,{
        if (input$cases_deaths == "cases") {
            hide("min_n_CFR")
            hide("min_n_deaths")
            show("min_n_cases")
        } else if (input$cases_deaths == "deaths") {
            hide("min_n_cases")
            hide("min_n_CFR")
            show("min_n_deaths")
        } else if (input$cases_deaths == "CFR") {
            hide("min_n_cases")
            show("min_n_CFR")
            hide("min_n_deaths")
        }
    })
    
    VAR_min_n = reactive({
        if (input$cases_deaths == "cases") {
            input$min_n_cases
        } else if (input$cases_deaths == "deaths") {
            input$min_n_deaths
        } else if (input$cases_deaths == "CFR") {
            input$min_n_CFR
        }
    })
    
    
    observeEvent(input$accumulated_daily_pct,{
        if (input$accumulated_daily_pct == "accumulated") {
            hide("growth_daily")
            hide("growth_pct")
            show("growth_accumulated")
        } else if (input$accumulated_daily_pct == "daily") {
            hide("growth_accumulated")
            hide("growth_pct")
            show("growth_daily")
        } else {
            hide("growth_daily")
            hide("growth_accumulated")
            show("growth_pct")
        }
    })

    
    VAR_growth = reactive({
        if (input$accumulated_daily_pct == "accumulated") {
            input$growth_accumulated
        } else if (input$accumulated_daily_pct == "daily") {
            input$growth_daily
        } else {
            input$growth_pct
        }
    })
    
    

    # Dynamic selection in highlight ------------------------------------------

    observeEvent(
        list(INPUT_countries_plot_debounced()), {
        
        # If set of highlights NOT the initial one
        # if (! is.null(INPUT_highlight_debounced())) {

            updateSelectInput(session = session, 
                              inputId = "highlight", 
                              choices = c(INPUT_countries_plot_debounced()),
                              selected = myReactives$highlight)
        # }
        
        })
    
    myReactives <- reactiveValues()  
    observe(
        myReactives$highlight <- INPUT_highlight_debounced()
    )
    
    

    # final_df() creation -----------------------------------------------------
    
    final_df = reactive({ 

        withProgress(message = 'Preparing data', value = 2, min = 0, max = 4, {
            
            req(VAR_min_n())
            req(input$cases_deaths)
            req(INPUT_countries_plot_debounced())
            
            # VARS
            INPUT_highlight = myReactives$highlight 
            INPUT_min_n = VAR_min_n()
            INPUT_cases_deaths = input$cases_deaths
            INPUT_countries_plot = INPUT_countries_plot_debounced()
            INPUT_relative = input$relative
            INPUT_accumulated_daily_pct = input$accumulated_daily_pct
            
            # message("INPUT_highlight = ", INPUT_highlight, "\nINPUT_min_n = ", INPUT_min_n, "\nINPUT_cases_deaths = ", INPUT_cases_deaths, "\nINPUT_countries_plot = ", INPUT_countries_plot, "\nINPUT_relative = ", INPUT_relative, "\nINPUT_accumulated_daily_pct = ", INPUT_accumulated_daily_pct, "\n")
            
            # Launch data preparation
            data_preparation(cases_deaths = INPUT_cases_deaths, countries_plot = INPUT_countries_plot, min_n = INPUT_min_n, relative = INPUT_relative)

            if (!is.null(INPUT_countries_plot)) {
                
                dta_temp = dta %>%
                    
                    # If repeated values the same day, keep higher
                    group_by(country, time) %>%
                    distinct(KEY = paste0(country, time, value), .keep_all = TRUE) %>%
                    select(-KEY) %>%
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

                    # Get rid of the latest data if it either 0 or negative
                    filter( !(days_after_100 == max(days_after_100, na.rm = TRUE) & diff <= 0)) %>% 
                    # 
                    # Create name_end labels
                    mutate(
                        name_end =
                            case_when(
                                days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
                                what == "lockdown" ~ "*",
                                TRUE ~ ""))

                
                # Highlight
                 if (any(' ' != myReactives$highlight )) {
                     
                     # Create colors diccionary
                     DF_colors_temp =
                         dta_temp %>% 
                         filter(country %in% myReactives$highlight ) %>% 
                         distinct(country)
                         
                     # If the selected country does not have observations over the threshold
                     if (nrow(DF_colors_temp) > 0) { 
                         DF_colors = DF_colors_temp %>% 
                             bind_cols(highlight = hue_pal(l = 50)(nrow(.)))
                     } else {
                         DF_colors = DF_colors_temp %>% 
                             mutate(highlight = "")
                     }
                     
                     
                    dta_temp %>% 
                         left_join(DF_colors, by = "country") %>% 
                         mutate(highlight = 
                                case_when(
                                    is.na(highlight) ~ "grey",
                                    TRUE ~ highlight
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
        
        # LIMITS of DATA
        # if (input$accumulated_daily_pct == "daily") {
        #     MIN_y = min(final_df()$diff, na.rm = TRUE)
        #     MAX_y = max(final_df()$diff, na.rm = TRUE) * 1.1
        # } else if (input$accumulated_daily_pct == "%") {
        #     MIN_y = min(final_df()$diff_pct, na.rm = TRUE) * 100
        #     MAX_y = max(final_df()$diff_pct, na.rm = TRUE) * 100
        # } else {
            MIN_y = min(final_df()$value, na.rm = TRUE)
            MAX_y = max(final_df()$value, na.rm = TRUE) * 1.1
        # }
        
        # To avoid error
        if (is.infinite(max(final_df()$days_after_100, na.rm = TRUE))) {
            max_finaldf_days_after_100 = 10 
        } else {
            max_finaldf_days_after_100 = max(final_df()$days_after_100, na.rm = TRUE)
        }
        
        # If we use 1.1 * to avoid overlaping y axis goes up a lot
        line_factor = 1
        if (input$accumulated_daily_pct == "%") {
            tibble(
                value = cumprod(c(100, rep((100 + VAR_growth()) / 100, line_factor * max_finaldf_days_after_100))),
                days_after_100 = 0:(line_factor * max_finaldf_days_after_100)) %>% 
                filter(value <= MAX_y)
        } else {
            tibble(
                value = cumprod(c(MIN_y, rep((100 + VAR_growth()) / 100, line_factor * max_finaldf_days_after_100))),
                days_after_100 = 0:(line_factor * max_finaldf_days_after_100)) %>% 
                filter(value <= MAX_y)
        }
        
    })
    

    # Prepare plot
    final_plot <- reactive({

        withProgress(message = 'Preparing plot', value = 3, min = 0, max = 4, {
                
            
            # Show accumulated or daily plot
            if (input$accumulated_daily_pct == "daily") {
                DF_plot = final_df() %>% 
                    rename(value_temp = value,
                    value = diff)
            } else if (input$accumulated_daily_pct == "%") {
                DF_plot = final_df() %>% 
                    rename(value_temp = value,
                           value = diff_pct) %>% 
                    mutate(value = value * 100)
            } else {
                DF_plot = final_df()
            }
            
            # Define which countries get a smooth (have enough points)
            VALUE_span = 3
            counts_filter = DF_plot %>% count(country) %>% filter(n > VALUE_span)

            
            # Draw plot ---------------------------------------------
            
            p_temp = ggplot(data = DF_plot, 
                            aes(x = days_after_100, y = value, group = as.factor(country), color = highlight)) +
                scale_color_hue(l = 50) +
                
                # Trend line
                geom_line(data = growth_line(), aes(days_after_100, value), linetype = "dotted", inherit.aes = FALSE) +

                # Country points (last one bigger)
                geom_point(aes(size = 1 + as.integer(final_df()$name_end != "" & final_df()$name_end != "*") - .5), alpha = .7) +
                
                scale_x_continuous(breaks = seq(0, max(final_df()$days_after_100, na.rm = TRUE), 2)) +
                labs(
                    title = paste0("Coronavirus confirmed ", input$cases_deaths , if (input$relative == TRUE) " / million people"),
                    subtitle = paste0("Arranged by number of days since ",  VAR_min_n() ," or more ", input$cases_deaths),
                    x = paste0("Days after ",  VAR_min_n() ," accumulated ", input$cases_deaths),
                    y = paste0("Confirmed ", input$accumulated_daily_pct, " ", input$cases_deaths, " (log scale)",  if (input$relative == TRUE) " / million people"), 
                    caption = paste0("[*]: Lockdown\nSources: Johns Hopkins CSSE and worldometers.info\n gorkang.shinyapps.io/2020-corona/")) +
                theme_minimal(base_size = 14) +
                theme(legend.position = "none")
            
            # Smooth or not
            if(input$smooth == FALSE) {
                p_temp = p_temp + geom_line(alpha = .7) 
            } else {
                p_temp = p_temp +  
                    geom_smooth(data = DF_plot %>% filter(country %in% counts_filter$country),
                                method = "loess", span = 1.5, se = FALSE, size = .8, alpha = .6, na.rm = TRUE)
            }

            # If any value is not " ", scale_color_identity()
            if (any(' ' != myReactives$highlight )) { p_temp =  p_temp + scale_color_identity() }
            
            
            # LIMITS
            if (input$accumulated_daily_pct == "daily") {
                MAX_y = max(final_df()$diff, na.rm = TRUE, na.rm = TRUE) * 1.1
                MIN_y = min(final_df()$diff, na.rm = TRUE, na.rm = TRUE) * 0.1 # In Log scale can't use 0
            } else if (input$accumulated_daily_pct == "%") {
                MAX_y = max(final_df()$diff_pct, na.rm = TRUE, na.rm = TRUE) * 100
                MIN_y = min(final_df()$diff_pct, na.rm = TRUE, na.rm = TRUE) * 1 # In Log scale can't use 0
            } else {
                MAX_y = max(final_df()$value, na.rm = TRUE, na.rm = TRUE) * 1.1
                MIN_y = min(final_df()$value, na.rm = TRUE, na.rm = TRUE) * 0.95
            }
            
            if (MIN_y == 0) MIN_y = 1
            # message("MIN_y: ", MIN_y, " MAX_y: ", MAX_y)
            
            # Scale, log or not
            if (input$log_scale == TRUE) {
                p_temp = p_temp +
                    scale_y_log10(breaks = scales::log_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) 
            } else {
                p_temp = p_temp +
                    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) +
                    labs(y = paste0("Confirmed ", input$accumulated_daily_pct, " ", input$cases_deaths,  if (input$relative == TRUE) " / million people"))
                    # labs(y = paste0("Confirmed ", input$accumulated_daily_pct, " ", input$cases_deaths))
            }

                     
            # Annotation trend line
            if (input$accumulated_daily_pct == "%") {
                x_axis = max(growth_line()$days_after_100, na.rm = TRUE) - .5
                y_axis = min(growth_line()$value, na.rm = TRUE) # MIN
            } else {
                x_axis = max(growth_line()$days_after_100, na.rm = TRUE) - 1.5
                y_axis = max(growth_line()$value, na.rm = TRUE) + 1 # MAX 
                # message("X: ", x_axis, " Y: ", y_axis)
            }
            
            p_final <- p_temp + 
                annotate(geom = "text",
                         x = x_axis, 
                         y = y_axis,
                         label = paste0(VAR_growth(), "% growth")) +
                
                # Country label
                ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .1, alpha = .75)  
                
                
            p_final
            
        })
    })
    
    # Show plot
    output$distPlot <- renderCachedPlot({
        
        withProgress(message = 'Showing plot', value = 4, min = 0, max = 4, {
            
            final_plot()
            
        })
    }, cacheKeyExpr = list(final_df(), growth_line(), myReactives$highlight , VAR_min_n(), input$accumulated_daily_pct, input$cases_deaths, input$log_scale, input$smooth))
    
        
    # Show table
    output$mytable = DT::renderDataTable({
        DT::datatable(final_df() %>%
                          arrange(desc(time), country) %>% 
                          select( -highlight, -name_end) %>%
                          rename_(.dots=setNames("value", paste0(input$cases_deaths, "_sum"))) %>% 
                          rename_(.dots=setNames("diff", paste0(input$cases_deaths, "_diff"))) %>% 
                          rename_(.dots=setNames("days_after_100", paste0("days_after_", VAR_min_n()))),
                          filter = 'top',
                      rownames = FALSE, 
                      options = list(pageLength = 10, 
                                     dom = 'ltipr',
                                     autoWidth = FALSE)) %>% 
            DT::formatPercentage(c("diff_pct"), 2)
    })
    
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(Sys.Date(), "_corona.png", sep = "") },
        content = function(file) { ggsave(file, plot = final_plot(), device = "png", width = 14, height = 10) }
    )

    output$downloadData <- downloadHandler(
        filename = function() { 
            paste("dataset-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(final_df() %>%
                          arrange(desc(time), country) %>% 
                          select(-name_end, -highlight) %>%
                          rename_(.dots=setNames("value", ifelse(input$cases_deaths == "cases", "cases_sum", "deaths_sum"))) %>% 
                          rename_(.dots=setNames("diff", ifelse(input$cases_deaths == "cases", "cases_diff", "deaths_diff"))) %>% 
                          rename_(.dots=setNames("days_after_100", paste0("days_after_", VAR_min_n()))), file)
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url") #, options = "test.mode"
