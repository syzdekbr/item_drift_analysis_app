###*** APP to examine item drift. Features 1. Sidebar to custom select data, 2. Plot of
###* items over time, 3. Analysis of item change over time and table of items with change
###* over that time, 4. Customizable plots of items by time

# Libraries and Set-up ----------------------------------------------------

library(shiny)
library(tidyverse)
library(shinyBS)


# Sample Data -------------------------------------------------------------

###*** Create sample data with person, date, person groups, and 0,1 scores for 5 items. 
## item_1 is increasingly easy over time, item_2 is constantly easy over time, 3-5
## are average over time
# Function to sample data according to specifications
  sample_func <- function(person_start, person_finish, date_start, date_finish, 
                          item_1_fail_prob){
  tibble(
  person = as_factor(person_start:person_finish), # 1 person per row
  date = sample(seq(as.Date(date_start), as.Date(date_finish), by="day"), 
                100, replace = TRUE), # Select date range
  # Location is a potential grouping variable
  location = sample(c("domestic", "international"), 100, replace = TRUE, 
                    prob = c(item_1_fail_prob, (1-item_1_fail_prob))),
  item_1 = sample(c(0,1), 100, replace = TRUE, prob = c(item_1_fail_prob, (1-item_1_fail_prob)))
) %>% 
  bind_cols(.,
            purrr::map2(
              .x = 2:5,
              # Probabilities of item failure- 0
              .y = c(.2, rep(.5, 3)),
              .f = ~ {
                ## Dichotomous items
                # Tibble with columns 'item_*'; 1000 rows, random 0 (fail), 1 (success)
                tibble(!!sym(paste0("item_", .x)) :=
                         sample(c(0,1), 100, replace = TRUE, prob = c(.y, (1-.y)))
                )
              })
            ) %>% 
      rowwise() %>% 
      ungroup()
  }

## Apply function with given persons, dates, and item_1 probabilities
  sample_dat <- purrr::pmap_dfr(
    .l = list(
      person_start = c(1,101, 201, 301, 401),
      person_finish = c(100, 200, 300, 400, 500),
      date_start = c("2022/01/01", "2022/03/01", "2022/05/01", "2022/07/01", "2022/09/01"),
      date_finish = c("2022/02/28", "2022/04/30", "2022/06/30", "2022/08/31", "2022/10/31"),
      item_1_fail_prob = c(.8, .6, .4, .2, .1)
    ),
    .f = sample_func
  )

# UI ----------------------------------------------------------------------

# Define UI as sidebar with options, mainpage as report
ui <- fluidPage(
  
  ## Busy indicator
  shinysky::busyIndicator(text = "Processing ... ", wait = 1000),
  tags$head(
    tags$style(HTML(".shinysky-busy-indicator {z-index: 1000;}"))
  ),

    # Application title
    titlePanel("NCSBN Item Drift Analysis"),

    # Sidebar with selection options 
    sidebarLayout(
        sidebarPanel(
          # These selections do nothing in example, can be customized for production
          # Will subset data by selections
            selectInput("program", "Select Program", "Program 1"),
            selectInput("test_plan", "Select Test Plan", "Test Plan 1"),
            selectInput("item_pool", "Select Item Pool", "Item Pool 1"),
            # Date defined in server, pre-selected start and end date by data range
            h4("Select options for data"),
            tipify(uiOutput("date_input_ui"),
                   title = "Select date range of data, prepopulated and restricted to 
                   min and max of data", placement = "right"),
            tipify(selectInput("grouping_variable", "Grouping Variable", 
                        c("None" = "none", "Location" = "location")),
                   title = "Optionally, select variable to control for", placement = "right"),
            h4("Time Span to Group Data in Separate Item Plots and Analysis"),
            fluidRow(
              column(6, numericInput("frequency", "Frequency", 1)),
              column(6, tipify(selectInput("time_group", "Time Interval", 
                                    c("Day(s)" = "day", "Week(s)" = "week", 
                                      "Month(s)" = "month", "Year(s)" = "year"),
                                    selected = "week"), 
                               title = "Select time interval to group data 
                               (e.g., 2 months groups into 2-month intervals)",
                               placement = "right"))
            ),
            uiOutput("select_items"),
            ## Choose output format
            uiOutput("rmarkdown_selectors")
        ),

        # Show results
        mainPanel(
           plotOutput("item_time_plot"), # Plot with all items
           # DT::dataTableOutput("contrast_table_item_time"), # Analysis table
           plotOutput("item_plot") # Separate item plots
        )
    )
)

# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Helper functions to format output
  source("helper_functions.R")
  
###*** Date selections- Initialized with start and end points of data
    output$date_input_ui <- renderUI({
      fluidRow(
        column(6, dateInput("start_date", "Select Start Date", value = min(sample_dat$date),
                            min = min(sample_dat$date), max = max(sample_dat$date))),
        column(6, dateInput("end_date", "Select End Date", value = max(sample_dat$date),
                            min = min(sample_dat$date), max = max(sample_dat$date)))
      )
    })
    

# Item/Time Interaction ---------------------------------------------------

###*** Plot with all items
## Function for plotting
  item_time_plot_func <- function(start_date, end_date){
    req(start_date) # Wait for dates to load
    # Transform data to long form
    long_dat <- sample_dat %>% 
      # Filter data between selected dates
      filter(between(date, as.Date(start_date), as.Date(end_date))) %>% 
      # Remove person grouping
      dplyr::select(-c(person)) %>% 
      pivot_longer(., cols = -c(date, location), 
                   names_to = "items", values_to = "values")
    # Plot, if "none" as grouping, no separation by group
    p <- if(input$grouping_variable == "none"){
      ggplot(long_dat, aes(x = date, y = values, color = items)) + 
        geom_smooth(se = FALSE)
    } else if(input$grouping_variable != "none"){
      ggplot(long_dat, aes(x = date, y = values, 
                           linetype = !! sym(input$grouping_variable), 
                           color = items)) + 
        geom_smooth(se = FALSE) 
    } 
    p +
      ggtitle("Item Difficulties Over Time") +
      ylab("Percent Correct") +
      xlab("Dates")
  }
    output$item_time_plot <- renderPlot({
      item_time_plot_func(input$start_date, input$end_date)
    })
 
###*** Analysis of change in item difficulty over time- Construct models with items as 
## fixed effect and value as outcome, with person as random effect-- Rasch model, allow
## for person to be nested within grouping_variable. Identify best model and then 
## significant slopes and output in table
    item_time_overall_interaction_func <- function(dat, optional_rand_effect, 
                                                   start_date, end_date, frequency, time_group){
      req(start_date) # Wait for dates to load
      # Pass grouping variable if chosen to formula, or none
      optional_rand_effect <- case_when(
        optional_rand_effect == "none" ~ "",
        optional_rand_effect == "location" ~ "/location"
      )
      # Create long data
      long_dat <- dat %>% 
        # Filter data between selected dates
        filter(between(date, as.Date(start_date), as.Date(end_date))) %>% 
        # Long data
        pivot_longer(., cols = -c(person, date, location), 
                     names_to = "items", values_to = "values") %>% 
        mutate(items = as_factor(items)) %>% 
        mutate(date_sequence = cut.Date(date, labels = FALSE, 
                                        breaks = paste(frequency, time_group)))   # Needed for emtrends

    ## Construct quadratic and linear model, identify best, then use
      # Quadratic
      full_mod_squared <- lme4::glmer(as.formula(paste(
        "values ~ items*poly(date_sequence,2) + (1|person", optional_rand_effect, ") - 1"
      )), 
                              family = "binomial", dat = long_dat)
      # Linear
      full_mod <- lme4::glmer(as.formula(paste(
        "values ~ items*date_sequence + (1|person", optional_rand_effect, ") - 1"
        )), 
                                      family = "binomial", dat = long_dat)
      poly_test <- lmtest::lrtest(full_mod_squared, full_mod)
      # Choose either quadratic, if lrtest significant, or linear
      poly_term <- ifelse(poly_test$`Pr(>Chisq)`[-1] < .05, 2, 1)
      # Assign quadratic or linear as "full_mod"
      full_mod <- if(poly_test$`Pr(>Chisq)`[-1] < .05){
        full_mod_squared
        } else{
          full_mod
        }
      ## Reduced model with interaction removed
      reduced_mod <- lme4::glmer(as.formula(paste(
        "values ~ items + poly(date_sequence,", poly_term, ") + (1|person", optional_rand_effect, ") - 1"
      )), 
                              family = "binomial", dat = long_dat)
      ## Compare full and reduced model. If significant identify significant slopes
      ratio_test_p_value <- lmtest::lrtest(full_mod, reduced_mod)
      if(ratio_test_p_value$`Pr(>Chisq)`[-1] < .05){
        emmeans::emtrends(full_mod, ~ items, var="date_sequence", 
                          infer = TRUE, adjust = "sidak") %>% 
          as.data.frame() %>%
          dplyr::select(-df) %>% 
          filter(p.value < .05) %>%
          mutate(across(where(is.numeric), round, 2)) %>% 
          purrr::set_names(., pretty_columns_func(names(.)))
      } else{
        tibble(Results = "No significant Interaction Between Time and Items")
      }
    }
    
    ###*** Don't include in app, just pass to report
    # output$contrast_table_item_time <- DT::renderDT(
    #   item_time_overall_interaction_func(sample_dat, input$grouping_variable,
    #                                      input$start_date, input$end_date)
    # )

 
# Separate Item Plots -----------------------------------------------------
    
  # Selection Box to Choose Items to Plot
    output$select_items <- renderUI({
      # Get all item names in data
      checkbox_choices <- sample_dat %>% 
        dplyr::select(starts_with("item_")) %>% 
        colnames()
      checkboxGroupInput("item_select", "Select Items for Plot", 
                         choices = checkbox_choices, selected = checkbox_choices,
                         inline = TRUE)
    })
    
    separate_item_plots_data_func <- function(dat, frequency, time_group, 
                                              start_date, end_date, 
                                              additional_grouping_var = NULL){
      req(start_date) # Wait for dates to load
      dat %>% 
        # Filter data between selected dates
        filter(between(date, as.Date(start_date), as.Date(end_date))) %>% 
        # Group data by selected intervals
        mutate(
          date_sequence = cut.Date(date, labels = FALSE,
                                   breaks = paste(frequency, time_group))
        ) %>% 
        pivot_longer(., cols = starts_with("item_"), # Selects all items
                     names_to = "items", values_to = "values") %>% 
        # Filter only selected items
        inner_join(.,
                   tibble(items = input$item_select),
                   by = "items") %>%
        # No grouping, all data, if input$grouping_variable == "none"
        mutate(none = "all_data") %>% 
        { ggplot(., aes(x = date_sequence, y = values, color = !! sym(input$grouping_variable))) +
            stat_summary(fun=mean, geom="line") +
            facet_wrap(facets = ~items) +
            ggtitle(paste("Separate Items Difficulty Over Time Grouped by", 
                          frequency, time_group, "Intervals")) +
            ylab("Difficultes") +
            xlab("Selected Date Grouping")
        }
    }
    output$item_plot <- renderPlot({
      separate_item_plots_data_func(sample_dat, input$frequency, input$time_group, 
                                    input$start_date, input$end_date, input$grouping_variable)
    })   


# Rmarkdown Report --------------------------------------------------------
 output$rmarkdown_selectors <- renderUI({
   tagList(
 tipify(radioButtons("report_format", "Choose format for report", choices =
                       c("HTML" = "html",
                         "PDF" = "pdf",
                         "Word/DOCX" = "docx"), inline = TRUE),
        title = "HTML- nice interactive format. PDF- nice static format. Word/DOCX- editable doc.",
        placement = "below"),
 tipify(downloadButton("generate_report", "Generate Report", icon = shiny::icon("file-contract")),
        title = "Choose desired report format and click to download a report", placement = "right")
   )
 })
    output$generate_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function() {
        paste0('item_drift_report_', gsub(" ", "_", input$item_pool),"_", Sys.Date(), ".", input$report_format)
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "item_drift_report.Rmd")
        file.copy("item_drift_report.Rmd", tempReport, overwrite = TRUE)

        # Can perform computations here
        # input_type = input$input_type
        params <- list(report_format = input$report_format,
                       all_item_plot = item_time_plot_func(input$start_date, input$end_date),
                       separate_item_plot = separate_item_plots_data_func(sample_dat, input$frequency, input$time_group, 
                                                                          input$start_date, input$end_date, input$grouping_variable),
                      item_time_analysis = item_time_overall_interaction_func(sample_dat, input$grouping_variable,
                                                         input$start_date, input$end_date, input$frequency,
                                                         input$time_group)

        )
        # # Knit the document, passing in the `params` list, and eval it in a
        # # child of the global environment (this isolates the code in the document
        # # from the code in this app).
        rmarkdown::render(tempReport,
                          output_file = file,
                          output_format = switch(input$report_format, pdf = 'pdf_document',
                                               html = 'html_document', docx = 'word_document'),
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    # output$generate_report <- downloadHandler(
    #   # For PDF output, change this to "report.pdf"
    #   filename = "report.pdf",
    #   content <- function(file) {
    #     tempReport <- file.path(tempdir(), "practice.Rmd")
    #     file.copy("practice.Rmd", tempReport, overwrite = TRUE)
    #     rmarkdown::render(tempReport, params = list(), output_format = 'pdf_document', envir = new.env(parent = globalenv()))
    #     file.copy(file.path(tempdir(), 'report.pdf'), file)
    # 
    #   },
    #   contentType = "application.pdf"
    # )
} # End Server section

# Run the application 
shinyApp(ui = ui, server = server)
