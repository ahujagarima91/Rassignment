# app.R

# Load required packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# ---

## Step 2: Define the UI (User Interface)

# Define the UI for the dashboard
ui <- dashboardPage(
    
    # 2.1 Layout Template: Header
    dashboardHeader(title = "mtcars Dashboard: Reactive Filters in R"),
    
    # 2.2 Sidebar Filters
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Raw Data", tabName = "rawdata", icon = icon("table"))
        ),
        
        # MPG Slider Filter
        sliderInput("mpgFilter", "Minimum MPG:", 
                    min = min(mtcars$mpg), 
                    max = max(mtcars$mpg), 
                    value = min(mtcars$mpg), 
                    step = 1),
        
        # Cylinders Select Filter
        selectInput("cylFilter", "Filter by Cylinders:", 
                    choices = c("All", sort(unique(mtcars$cyl))), 
                    selected = "All"),
        
        # Transmission Select Filter (0=Automatic, 1=Manual)
        selectInput("amFilter", "Filter by Transmission:", 
                    choices = c("All", "Automatic" = 0, "Manual" = 1), 
                    selected = "All"),
        
        # Gear Select Filter
        selectInput("gearFilter", "Filter by Gears:", 
                    choices = c("All", sort(unique(mtcars$gear))), 
                    selected = "All")
    ),
    
    # 2.3 Dashboard Body Layout
    dashboardBody(
        tabItems(
            # Dashboard Tab Content
            tabItem(tabName = "dashboard",
                    # Info Boxes Row
                    fluidRow(
                        infoBoxOutput("avgMPGBox", width = 4),
                        infoBoxOutput("totalCarsBox", width = 4),
                        infoBoxOutput("commonCylBox", width = 4)
                    ),
                    # First Row of Plots
                    fluidRow(
                        box(title = "MPG by Cylinders (Boxplot)", status = "primary", solidHeader = TRUE, plotOutput("mpgBox"), width = 4),
                        box(title = "Weight Distribution (Histogram)", status = "info", solidHeader = TRUE, plotOutput("weightHist"), width = 4),
                        box(title = "Scatter: MPG vs HP", status = "success", solidHeader = TRUE, plotOutput("scatterMPGHP"), width = 4)
                    ),
                    # Second Row of Plots
                    fluidRow(
                        box(title = "Cylinder Distribution (Pie Chart)", status = "danger", solidHeader = TRUE, plotOutput("pieCyl"), width = 4),
                        box(title = "Gear Count (Bar Chart)", status = "warning", solidHeader = TRUE, plotOutput("gearBar"), width = 4),
                        box(title = "MPG Density Plot", status = "info", solidHeader = TRUE, plotOutput("densityMPG"), width = 4)
                    )
            ),
            
            # Raw Data Tab Content
            tabItem(tabName = "rawdata",
                    DT::dataTableOutput("table")
            )
        )
    )
)

# ---

## Step 3: Build the Server Logic

# Define the server logic
server <- function(input, output) {
    
    # 3.1 Filtered Dataset (Reactive)
    # This reactive expression filters the mtcars data based on all sidebar inputs.
    filteredData <- reactive({
        data <- mtcars
        
        # Filter by Minimum MPG
        data <- data[data$mpg >= input$mpgFilter, ]
        
        # Filter by Cylinders (must convert input to numeric)
        if (input$cylFilter != "All") {
            data <- data[data$cyl == as.numeric(input$cylFilter), ]
        }
        
        # Filter by Transmission (must convert input to numeric)
        if (input$amFilter != "All") {
            data <- data[data$am == as.numeric(input$amFilter), ]
        }
        
        # Filter by Gears (must convert input to numeric)
        if (input$gearFilter != "All") {
            data <- data[data$gear == as.numeric(input$gearFilter), ]
        }
        
        # Return the filtered data frame
        return(data)
    })
    
    # ---
    
    # 3.2 Summary Info Boxes
    
    # Average MPG Box
    output$avgMPGBox <- renderInfoBox({
        # Calculate average MPG from the reactive dataset
        avg <- round(mean(filteredData()$mpg), 1)
        infoBox("Average MPG", avg, icon = icon("tachometer-alt"), color = "aqua")
    })
    
    # Total Cars Box
    output$totalCarsBox <- renderInfoBox({
        # Count rows in the reactive dataset
        count <- nrow(filteredData())
        infoBox("Total Cars", count, icon = icon("car"), color = "green")
    })
    
    # Most Common Cylinders Box
    output$commonCylBox <- renderInfoBox({
        # Find the mode (most frequent value) of 'cyl' in the reactive dataset
        cyl_mode <- filteredData() %>%
            count(cyl) %>%
            arrange(desc(n)) %>%
            slice(1) %>%
            pull(cyl)
        
        # Handle case where no data is filtered (optional: if you want a cleaner look)
        if (length(cyl_mode) == 0) {
            cyl_mode <- "N/A"
        }
        
        infoBox("Most Common Cylinders", cyl_mode, icon = icon("cogs"), color = "yellow")
    })
    
    # ---
    
    # 3.3 Create the 6 Graphs (All plots use the reactive filteredData())
    
    # MPG Boxplot by Cylinders
    output$mpgBox <- renderPlot({
        ggplot(filteredData(), aes(x = factor(cyl), y = mpg)) +
            geom_boxplot(fill = "skyblue") +
            labs(title = "MPG by Cylinder", x = "Cylinders", y = "MPG") +
            theme_minimal()
    })
    
    # Weight Histogram
    output$weightHist <- renderPlot({
        ggplot(filteredData(), aes(x = wt)) +
            geom_histogram(binwidth = 0.5, fill = "orange", color = "white") +
            labs(title = "Weight Distribution", x = "Weight (1000 lbs)", y = "Count") +
            theme_minimal()
    })
    
    # MPG vs Horsepower Scatter Plot
    output$scatterMPGHP <- renderPlot({
        ggplot(filteredData(), aes(x = hp, y = mpg)) +
            geom_point(color = "darkgreen", size = 3) +
            geom_smooth(method = "lm", se = FALSE, color = "red") + # Added a trend line for better insight
            labs(title = "MPG vs Horsepower", x = "Horsepower (hp)", y = "MPG") +
            theme_minimal()
    })
    
    # Pie Chart: Cylinder Distribution
    output$pieCyl <- renderPlot({
        cyl_data <- filteredData() %>%
            count(cyl) %>%
            mutate(percent = round(n / sum(n) * 100, 1),
                   label = paste0(cyl, " cyl (", percent, "%)"))
        
        # Plotting the pie chart (using geom_bar with coord_polar)
        ggplot(cyl_data, aes(x = "", y = n, fill = factor(cyl))) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0) +
            theme_void() + # Removes axes, gridlines, and background
            labs(title = "Cylinder Distribution", fill = "Cylinders") +
            geom_text(aes(label = label), position = position_stack(vjust = 0.5))
    })
    
    # Gear Count Bar Chart
    output$gearBar <- renderPlot({
        ggplot(filteredData(), aes(x = factor(gear))) +
            geom_bar(fill = "steelblue") +
            labs(title = "Number of Cars by Gears", x = "Gears", y = "Count") +
            theme_minimal()
    })
    
    # MPG Density Plot
    output$densityMPG <- renderPlot({
        ggplot(filteredData(), aes(x = mpg)) +
            geom_density(fill = "purple", alpha = 0.5) +
            labs(title = "MPG Density", x = "MPG", y = "Density") +
            theme_minimal()
    })
    
    # ---
    
    # 3.4 Data Table
    
    # Raw Data Table (Filtered and paginated)
    output$table <- DT::renderDataTable({
        DT::datatable(filteredData(), options = list(pageLength = 10))
    })
}

# ---

## Step 4: Run the App
shinyApp(ui = ui, server = server)
