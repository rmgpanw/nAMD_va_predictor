#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(broom)
library(eyedata)

# reformat data
amd_reformatted <- amd %>% 
  mutate(time_from_1yr = time - 365) %>% 
  # remove patients without 1 year +/- 30 days VA
  # closest to 1 year
  group_by(patID) %>% 
  filter(time == 0 |
           between(time_from_1yr, left = -60, right = 60)) %>% 
  
  # take just baseline and 1yr VAs
  filter(time == 0 |
           min(time_from_1yr)) %>% 
  
  # remove patients without both baseline and 1 year VA
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  ungroup() %>% 
  
  # remove missing va
  filter(!is.na(va)) %>% 
  
  # relabel `time`
  mutate(time = case_when(
    time == 0 ~ "Baseline",
    TRUE ~ "1 year"
  )) %>% 
  
  # add baseline VA vategory
  mutate(va_category = case_when(
    va > 70 ~ ">70", 
    va > 65 & va <=70 ~ "66-70", 
    va > 60 & va <=65 ~ "61-65",
    va > 55 & va <=60 ~ "56-60",
    va > 50 & va <=55 ~ "51-55",
    va > 45 & va <=50 ~ "46-50",
    va > 40 & va <=45 ~ "41-45",
    va > 35 & va <=40 ~ "36-40",
    va > 30 & va <=35 ~ "31-35",
    va > 25 & va <=30 ~ "26-30",
    va <=25 ~ "<=25"
  ))

# variables
baseline_va_categories <- sort(unique(amd_reformatted$va_category))
################################

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wet AMD one year visual outcomes predictor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                "baseline_va_category",
                "Visual acuity at baseline (ETDRS letters)",
                baseline_va_categories),
            actionButton("replot", "Show outcomes")
        ),

        # Show a plot of the generated distribution and report probability of <6/60 and >=6/12 VA at one year
        mainPanel(
            fluidRow(
                column(12, plotOutput("one_year_va_plot")),
            ),
            fluidRow(
                column(
                    12, 
                    verbatimTextOutput("prop_6_12_6_60")
                    )
            )
        
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # reactive computation
    filter_data <- eventReactive(input$replot, {
      baseline_va_subset <- amd_reformatted %>% 
        filter(time == "Baseline") %>% 
        filter(va_category == input$baseline_va_category)
      
      amd_reformatted %>% 
        filter(patID %in% baseline_va_subset$patID) %>% 
        filter(time == "1 year")
    })
    
    output$one_year_va_plot <- renderPlot({
        # plot density plot for input baseline VA category
        filter_data() %>% 
            ggplot(aes(x = va)) + geom_density(alpha = 0.3, bw = 5, fill = "blue") + theme_classic() + 
            geom_vline(mapping = aes(xintercept = 35), colour = "red") + 
            geom_vline(mapping = aes(xintercept = 55), colour = "purple") + 
            geom_vline(mapping = aes(xintercept = 70), colour = "blue") + 
            geom_text(aes(x=35, label="6/60\n", y=0.02), colour="red", angle=90) +
            geom_text(aes(x=55, label="6/24\n", y=0.02), colour="purple", angle=90) +
            geom_text(aes(x=70, label="\n6/12", y=0.02), colour="blue", angle=90)
    })
    
    # ...print these probabilities
    output$prop_6_12_6_60 <- renderText({
      probs <- list(
        better_6_12 = tibble(
          total_n = nrow(filter_data()),
          n = sum(filter_data()$va >= 70)
        ),
        worse_6_60 = tibble(
          total_n = nrow(filter_data()),
          n = sum(filter_data()$va <= 35)
        )
      ) %>%
        purrr::map(~ broom::tidy(
          binom.test(
            x = .x$n,
            n = .x$total_n,
            p = 0.5,
            alternative = "two.sided"
          )
        ))
        
        paste0(
            "Chance of vision 6/12 or better: ", 
            round(probs$better_6_12$estimate, digits = 2) * 100,
            "% (95% CI ",
            round(probs$better_6_12$conf.low, digits = 2) * 100,
            "-",
            round(probs$better_6_12$conf.high, digits = 2) * 100,
            ")\n\n",
            
            "Chance of vision worse than 6/60: ",
            round(probs$worse_6_60$estimate, digits = 2) * 100,
            "% (95% CI ",
            round(probs$worse_6_60$conf.low, digits = 2) * 100,
            "-",
            round(probs$worse_6_60$conf.high, digits = 2) * 100,
            ")\n\n"
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
