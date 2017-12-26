###########################################################
# Title: Conditional Panels with fiticious stat 133 grades data set
#
# Description:
#   This shiny app contains three tabs; see tabsetPanel()
#   Each tab shows a different sidebar panel.
#   The sibar bar panels are handled with 'conditionalPanel()'
#
# Details:
#   The graphics in each tab are obtained with ggvis
#
# Author: Tony Hsu
###########################################################

# required packages
library(shiny)
library(ggvis)
library(dplyr)
library(readr)
source("../code/functions.R")

#
cleanScore = read_csv("../data/cleandata/cleanscores.csv")
cleanScore = mutate(cleanScore, Grade = factor(
  Grade,
  levels = c("A+",
             "A", "A-", "B+", "B", "B-",
             "C+", "C", "C-", "D", "F")
))
Grades = cleanScore$Grade
Grade = as.data.frame(table(Grades))[, 1]
Freq = as.data.frame(table(Grades))[, 2]
Prop = as.data.frame(prop.table(table(Grades)))[, 2]
gradeTable = data.frame(Grade, Freq, Prop)


# Variable names for histograms
assignment <-
  c(
    'HW1',
    'HW2',
    'HW3',
    'HW4',
    'HW5',
    'HW6',
    'HW7',
    'HW8',
    'HW9',
    "Lab",
    "QZ1",
    "QZ2",
    "QZ3",
    "QZ4",
    "Test1",
    "Test2",
    "Overall"
  )



# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Grade Visualizer"),
  
  # Sidebar with different widgets depending on the selected tab
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       h3("Panel of 1st tab"),
                       tableOutput("table1")),
      conditionalPanel(
        condition = "input.tabselected==2",
        h3("Panel of 2nd tab"),
        selectInput("var2", "X-axis variable", assignment,
                    selected = "HW1"),
        sliderInput(
          "width",
          "Bin Width",
          min = 1,
          max = 10,
          value = 10
        )
      ),
      conditionalPanel(
        condition = "input.tabselected==3",
        selectInput("var3", "X-axis variable", assignment,
                    selected = "HW1"),
        selectInput("var4", "Y-axis variable", assignment,
                    selected = "HW2"),
        sliderInput(
          "opacity",
          "Opacity",
          min = 0,
          max = 1,
          value = 0.5
        ),
        radioButtons(
          "show_line",
          "Show line",
          list(
            "none" = "n",
            "lm" = "l",
            "loess" = "lo"
          )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Barchart", value = 1,
                 ggvisOutput("barchart")),
        tabPanel(
          "Histogram",
          value = 2,
          ggvisOutput("histogram"),
          h1("Summary Statistics"),
          verbatimTextOutput("summary")
        ),
        tabPanel(
          "Scatterplot",
          value = 3,
          ggvisOutput("scatterplot"),
          h2("Correlation"),
          verbatimTextOutput("helper")
        ),
        id = "tabselected"
      )
    )
  ))


# Define server logic
server <- function(input, output) {
  output$table1 = renderTable(gradeTable)
  
  # Barchart (for 1st tab)
  vis_barchart <- reactive({
    # Normally we could do something like ggvis(x = ~mpg),
    # but since the inputs are strings, we need to do a little more work.
    #var1 <- prop("x", as.symbol(input$var1))
    cleanScore %>%
      ggvis(x = ~ Grade, fill := "#ef623b") %>%
      layer_bars(stroke := '#ef623b',
                 fillOpacity := 0.8,
                 fillOpacity.hover := 1) %>%
      add_axis("y", title = "frequency")
  })
  
  vis_barchart %>% bind_shiny("barchart")
  
  # Histogram (for 2nd tab)
  vis_histogram <- reactive({
    # Normally we could do something like ggvis(x = ~mpg),
    # but since the inputs are strings, we need to do a little more work.
    var2 <- prop("x", as.symbol(input$var2))
    
    cleanScore %>%
      ggvis(x = var2, fill := "#abafb5") %>%
      layer_histograms(stroke := 'white',
                       width = input$width)
  })
  vis_histogram %>% bind_shiny("histogram")
  
  output$summary <- renderPrint({
    #dataset <- datasetInput()
    #summary(dataset)
    var2 <- prop("x", as.symbol(input$var2))
    result = as.vector(cleanScore[, as.character(var2)])
    result = summary_stats(result[[1]])
    print_stats(result)
  })
  
  vis_scatterplot <- reactive({
    # Normally we could do something like ggvis(x = ~mpg),
    # but since the inputs are strings, we need to do a little more work.
    var3 <- prop("x", as.symbol(input$var3))
    var4 <- prop("y", as.symbol(input$var4))
    
    #displaying scatterplot
    if (input$show_line == "n") {
      cleanScore %>%
        ggvis(x = var3,
              y = var4,
              fill := "#abafb5",
              opacity := input$opacity) %>%
        layer_points(size := 50, stroke := 'white')
    } else if (input$show_line == "lo") {
      cleanScore %>%
        ggvis(x = var3,
              y = var4,
              fill := "#abafb5",
              opacity := input$opacity) %>%
        layer_points(size := 50, stroke := 'white') %>%
        layer_smooths(stroke := "blue")
    } else {
      cleanScore %>%
        ggvis(x = var3,
              y = var4,
              fill := "#abafb5",
              opacity := input$opacity) %>%
        layer_points(size := 50, stroke := 'white') %>%
        layer_model_predictions(model = "lm")
    }

    
    
    
  })
  
  vis_scatterplot %>% bind_shiny("scatterplot")
  
  #printing correlation
  output$helper <- renderPrint({
    var3 <- prop("x", as.symbol(input$var3))
    var4 <- prop("y", as.symbol(input$var4))
    result1 = as.vector(cleanScore[, as.character(var3)])
    result2 = as.vector(cleanScore[, as.character(var4)])
    a = cor(result1, result2)
    cat(a[1])
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)