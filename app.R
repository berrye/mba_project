library(shiny)
library(dplyr)
library(ggvis)
library(RColorBrewer)
library(rprojroot)

# read in the file
dir_root <- find_rstudio_root_file()
file_name <- file.path(dir_root, "data", "mbateam.csv")
data <- read.csv(file = file_name, header = TRUE)

# color blind friendly color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# function to convert categorial variables to numeric (yes = 1, no or blank = 0)
cat2num <- function(x){
  ifelse(x== "Yes", 1, 0)
}

data <- 
  data %>% 
  # convert categorical variables to numeric
  mutate_each(funs(cat2num), X401K, FMLA, Payroll, Call.Center, Consolidated.Billing, Easy.Enroll) %>% 
  mutate(New.Hire.Orientation.Frequency = ifelse(New.Hire.Orientation.Frequency== "", 0, 1),
         # combine 401k + FMLA + Payroll + New Hire Orientation = additional services variable
         add.serv = X401K+FMLA+Payroll+New.Hire.Orientation.Frequency,
         # combine call center + CBA + Easy Enroll = additional departments variable
         add.dep = Call.Center+Consolidated.Billing+Easy.Enroll,
         # multiply transactions per person times number of employees to get total transactions
         total.trans = Transactions.Per.Employee*Number.of.Employees,
         # add together all phone calls, emails, and transactions to get total effort variable
         total.effort = Annual.Emails.Inbound+Annual.Phone.Calls.Call.Tracking+total.trans)

# put all these columns together into dataframe and give them nice names
data2 <-
  data %>% 
  select(company = Company,
         num.emp = Number.of.Employees,
         age = DOB.Average,
         num.benefits = Number.of.Benefits,
         add.serv = add.serv,
         add.dep = add.dep,
         emails = Annual.Emails.Inbound,
         calls = Annual.Phone.Calls.Call.Tracking,
         trans = total.trans,
         total.effort = total.effort,
         services = Services) %>% 
  mutate(
    # sigfigs
    age = signif(age, 3),
    total.effort = signif(total.effort, 5),
    trans = signif(trans, 5)
  )

##################################################################################################################
size_step <- 50 # size of step for slider input

ui <- fluidPage(
  titlePanel(title = "CWV Project"),

  sidebarLayout(

    sidebarPanel(
      sliderInput(inputId = "num",
                  label = "Company Size",
                  value = c(range(data2$num.emp)), 
                  min = size_step*floor(min(data2$num.emp)/size_step), 
                  max = size_step*ceiling(max(data2$num.emp)/size_step), 
                  step = size_step),
      br(),
      selectInput(inputId = "var", 
                  label = "Explanatory Variable:",
                  choices = c(
                    "Additional Departments" = "add.dep",
                    "Additional Services" = "add.serv",
                    "Age" = "age",
                    "Benefits" = "num.benefits",
                    "Number of Employees" = "num.emp"),
                  selected = "num.benefits"),
      br(),
      radioButtons(inputId = "radio", 
                   label = "Color plot by:",
                   choices = list("Services" = "services",
                                  "Additional Departments" = "add.dep",
                                  "Additional Services" = "add.serv"))
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel(title = "Emails",
                           value = "emails",
                           ggvisOutput("emails"),
                           tags$b("Correlation:"),
                           verbatimTextOutput("statse")
                  ),
                  tabPanel(title = "Phone Calls",
                           value = "calls",
                           ggvisOutput("calls"),
                           tags$b("Correlation:"),
                           verbatimTextOutput("statsc")
                  ),
                  tabPanel(title = "Transactions",
                           value = "trans",
                           ggvisOutput("trans"),
                           tags$b("Correlation:"),
                           verbatimTextOutput("statst")
                  ),
                  tabPanel(title = "Total Effort",
                           value = "total.effort",
                           ggvisOutput("total"),
                           tags$b("Correlation:"),
                           verbatimTextOutput("statstotal")
                  )
      )))
)

server <- function(input, output) {

  fdata <- reactive({
    data2 %>%
      filter(
        between(num.emp, input$num[1], input$num[2])
      )
  })
  
  all_plot <- reactive({
    df <- fdata() %>% 
      mutate(
        xvar = fdata()[,input$var],
        yvar = fdata()[,input$tabs],
        radio = as.factor(fdata()[,input$radio])
      )
    df %>% ggvis(~xvar, ~yvar) %>%
      layer_points(key:= ~company, size := 200, fill = ~df$radio, opacity := .6) %>%
      add_tooltip(function(df2){
        paste0(df2$company, "<br>", input$var, ": ", df2$xvar, "<br>", input$tabs, ": ", df2$yvar)
      }, "hover") %>%
      add_axis("x", title = input$var) %>%
      add_axis("y", title = input$tabs) %>%
      scale_ordinal("fill", range = cbPalette) %>%
      layer_model_predictions(model = "lm", formula = yvar~xvar)
  })

    all_plot %>% bind_shiny("emails")
  all_plot %>% bind_shiny("calls")
  all_plot %>% bind_shiny("trans")
  all_plot %>% bind_shiny("total")

  all_stats <- reactive({
      cor(fdata()[,input$var], fdata()[,input$tabs], use = "complete")
  })

  output$statse <- all_stats
  output$statsc <- all_stats
  output$statst <- all_stats
  output$statstotal <- all_stats

}

shinyApp(ui = ui, server = server)
