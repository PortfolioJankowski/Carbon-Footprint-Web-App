#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(readxl)
library(dplyr) #Creating new column (mutate function)
library(DT) #Data tables
library(ggplot2) #Chart


# Define UI for application
ui <- fluidPage(
  # Adding general theme
 theme = shinytheme("simplex"),
 
 # CSS Styling -> navbar
 tags$head(
   tags$style(HTML("
     .navbar {
              background-color: #ff3232; 
              color: white;
              font-weight: bold;
              padding: 15px 20px; 
              border-radius: 0; 
              box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1); 
            }


    .navbar-default .navbar-nav > li > a {
              color: white;
              transition: color 0.3s ease-in-out;
            }

    .navbar-default .navbar-nav > li > a:hover {
          color: #2c3e50; /* Darker color on hover */
    }


    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:hover,
    .navbar-default .navbar-nav > .active > a:focus {
            background-color: #ff6666; 
            color: white;
          }
    
    .navbar-brand {
            color: white !important;
            font-weight: bold;
            text-transform: uppercase;"))
 ),


    # NavBar -> navigation for my web app
  navbarPage(
    title = "Company Carbon Footprint",
    
    # Creating my first Tab in navBar
    # Firstly i want to show CF, then Decarbonisation forecast, then raw data
    tabPanel(
      "Introduction",
      sidebarLayout(
        sidebarPanel(
          width =1,
          tags$a(href = "https://www.wri.org/", class = "btn btn-primary d-block", style = "display: block; width:100%; margin-bottom: 10px;", "WRI"),
          tags$a(href = "https://ghgprotocol.org/", class = "btn btn-primary d-block", style = "display: block;width:100%;margin-bottom: 10px;","GHG"),
          tags$a(href = "https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2022", class = "btn btn-primary d-block", style = "display: block;width:100%; margin-bottom: 10px;","DEFRA"),
        ),
        mainPanel(
          fluidRow(
            column(width = 7,  uiOutput("CalcDescription")),
            column(width=5,img(src='img.png', align = "right", offset = 2, width =400, height=350)),
          )
        )
      )
    ),
    tabPanel(
      "Calculations",
      mainPanel(
        uiOutput("summary_Text"),
        uiOutput("CatDescription"),
        fluidRow(
          column(width = 6, plotOutput(outputId = "EmissionsByCategory")),
          column(width = 6, plotOutput(outputId = "EmissionsByCountry"))
        ),
        fluidRow(uiOutput("line")),
        fluidRow(
          column(width = 12, plotOutput(outputId = "EmissionsBySource"))
        )
      )
    ),
    tabPanel(
      "Decarbonisation",
      sidebarLayout(
        sidebarPanel(
          uiOutput("DecarbonisationInit"),
          sliderInput(inputId = "nr", label = "Choose year", min = 2023, max = 2050, value = 2023,step = 1, sep = ""),
          selectInput(inputId = "xAxis", label = "Filter", 
                      choices = c("Location" = "Location", "Category" = "Emission.Category", "Source" = "Source"), 
                      selected = "Location")
        ),
        mainPanel(
          uiOutput("DecarbonisationText"),
          uiOutput("Decarbo_summary"),
          plotOutput(outputId = "Decarbonisation")
        )
      )
    ),
    tabPanel(
      "Data",
      sidebarLayout(
        sidebarPanel(
          uiOutput("DataDescription")
        ),
        mainPanel(
          dataTableOutput("table")
        )
      )
    )
  )
)


# SERVER
server <- function(input, output) {
  # CALC TEXT
    output$CatDescription <- renderText({
      text <- "
      <style>
          body {
            font-family: Arial, sans-serif;
          }
      
          h1 {
            color: #333;
            text-align: center;
            padding-top: 20px;
          }
      
          hr {
            border: 1px solid #ccc;
            margin: 20px 0;
          }
      
          table {
            width: 100%;
            border-collapse: collapse;
            margin-bottom: 20px;
          }
      
          th, td {
            border: 1px solid #ddd;
            padding: 12px;
            text-align: left;
          }
      
          th {
            background-color: #f2f2f2;
          }
      
          tbody td {
            vertical-align: top;
          }
        </style>
        <h1>Carbon Footprint Categories</h1>
            <hr></hr>
      
        <table>
          <thead>
            <tr>
              <th>Fuels</th>
              <th>Refrigerants</th>
              <th>Energy</th>
              <th>Raw Materials</th>
              <th>Transportation</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td>
                The fuels category includes emissions associated with the extraction, transportation, refining, and combustion of various types of fuels. Common fuel categories include coal, crude oil, and natural gas.
              </td>
              <td>
                Refrigerants, such as freons, can be a significant source of greenhouse gas emissions. Emissions related to the production, use, and disposal of refrigerants are considered in this category.
              </td>
              <td>
                The energy category encompasses emissions associated with the production and consumption of energy. It is essential to consider emissions from the production of electricity and heat, both from conventional and renewable sources.
              </td>
              <td>
                Raw materials include emissions associated with the extraction, production, and transportation of materials used in manufacturing processes. Common raw materials include steel, aluminum, cement, paper, etc.
              </td>
              <td>
                The transportation category covers emissions associated with the movement of people, goods, and products. It is important to consider emissions from road, air, and maritime transportation.
              </td>
            </tr>
          </tbody>
        </table>
      
      </body>"
      HTML(text)
    })
  # DATA TEXT
    output$DataDescription <- renderText({
      text <- "<style>
                  body {
                    font-family: Arial, sans-serif;
                    line-height: 1.6;
                    color: #333;
                    margin: 20px;
                        }
  
                    h1, h2 {
                      color: #008080;
                    }
                
                    hr {
                      border: 1px solid #ccc;
                      margin: 20px 0;
                    }
                
                    p {
                      margin-bottom: 20px;
                    }
                
                    ol {
                      list-style-type: none;
                      padding: 0;
                      margin: 0;
                    }
                
                    li {
                      margin-bottom: 10px;
                    }
                </style>
                <h1>Data from the organization</h1>
                <hr></hr>
                <p>The table on the right side contains data received from the organization reporting its carbon footprint. Emissions from individual sources are categorized into appropriate groups and locations</p>
                <h2>The organization has headquarters in the following locations:</h2>
                <ol>
                  <li>United States</li>
                  <li>Germany</li>
                  <li>England</li>
                  <li>Poland</li>
                  <li>Norway</li>
                </ol>
                <h2>The organization uses the following emission sources:</h2>
                <ol>
                  <li>Fuels</li>
                  <li>Refrigerants</li>
                  <li>Energy</li>
                  <li>Raw materials</li>
                  <li>Transport</li>
                </ol>"
      HTML(text)
    })
  # DECARBONISATION TEXT
    output$DecarbonisationInit <- renderText({
      text <- "<style>
                p {
                font-family: 'Arial', sans-serif;
                font-weight: bold;
                font-size: 16px; 
                color: #333; 
                text-align: center; 
                margin: 20px;
                } 
              </style>
              <p>Select a year and an appropriate filter to discover future results:</p>
              <hr></hr>"
      
      HTML(text)
    })
    output$DecarbonisationText <- renderText({
      text <- " <h1>Decarbonisation</h1>
                <hr></hr>
                <p2>Decarbonisation is the process of reducing or eliminating carbon dioxide emissions, particularly in the context of addressing climate change. It involves transitioning to low-carbon or carbon-neutral technologies and practices.</p2>
                <p2>In this case, the assumption was made of <b>reducing the carbon footprint by 2% each year.</b></p2>
                <p2>The calculation indicates that, with a 2% reduction in the carbon footprint each year from 2023 to 2050, the overall reduction by the year 2050 would be approximately <b>45.56%.</b></p2>"
      HTML(text)
    })
  # LINE in calculations
    output$line <- renderText({
      text <- "<hr></hr>"
      HTML(text)
    })
  # INTRODUCTION TEXT
    output$CalcDescription <- renderText({
      text <- " <style>
                    body {
                      font-family: Arial, sans-serif;
                      line-height: 1.6;
                      color: #333;
                      margin: 20px;
                    }
                
                    h1, h2 {
                      color: #008080;
                    }
                
                    hr {
                      border: 1px solid #ccc;
                      margin: 20px 0;
                    }
                
                    p {
                      margin-bottom: 20px;
                    }
                
                    ol {
                      list-style: none;
                      padding: 0;
                      margin: 0;
                    }
                
                    li {
                      margin-bottom: 10px;
                    }
                
                    strong {
                      font-weight: bold;
                    }
              </style>
              <h1>Carbon Footprint Calculation</h1>
              <hr></hr>
              <p>The carbon footprint is a measure of the total amount of greenhouse gases, primarily carbon dioxide, that are directly or indirectly produced by human activities.</p>
              <h2>Steps to Calculate Carbon Footprint:</h2>
              <ol>
                  <li><strong>Identify Sources:</strong> Determine the sources of greenhouse gas emissions, including transportation, energy consumption, and waste production.</li>
                  <li><strong>Collect Data:</strong> Gather relevant data, such as fuel consumption, electricity usage, and waste generated.</li>
                  <li><strong>Conversion Factors:</strong> Convert the collected data into equivalent carbon dioxide emissions using appropriate conversion factors.</li>
                  <li><strong>Calculate Total Emissions:</strong> Sum up the converted emissions from all sources to get the total carbon footprint.</li>
                  <li><strong>Offset and Reduce:</strong> Explore ways to offset or reduce the carbon footprint through initiatives like tree planting, renewable energy use, and energy efficiency improvements.</li>
              </ol>
              <p>It's important to regularly assess and update the carbon footprint calculation to track progress in reducing environmental impact.</p>
                <h2>In the computations displayed on this website, we employ</h2>
                <ol>
                  <li><strong>GHG Protocol:</strong> The GHG Protocol is a widely recognized set of standards for accounting and reporting greenhouse gas emissions. Developed by the World Resources Institute (WRI) and the World Business Council for Sustainable Development (WBCSD), it provides guidelines and tools to help organizations measure and manage their carbon footprints.</li>
                  <li><strong>DEFRA Emission Factors:</strong> DEFRA Emission Factors refer to the emission factors provided by the Department for Environment, Food & Rural Affairs (DEFRA), offering standardized values used to estimate greenhouse gas emissions associated with various activities, such as energy production, transportation, and industrial processes.</li>
              </ol>"
      HTML(text)
    })
    # DATA LOADING AND JOINING
    companyData <- reactive({
      cData <- read.csv("Data.csv", header=TRUE, sep = ";")
      return(cData)
    })
     
    categories <- reactive({
      catData <- read.csv("Categories.csv", header = TRUE, sep = ";")
      return(catData)
    })
    
    locations <- reactive({
      read.csv("Locations.csv", header = TRUE, sep = ";")
    })
    
    factors <- reactive({
      read.csv("Factors.csv", header = TRUE, sep = ";")
    })
    
    
    #JOINING TABLES -> DATA tab
    mergedData <- reactive({
      company <- companyData()
      categories <- categories()
      locations <- locations()
      factors <- factors()
     
      joined <- left_join(company, categories, by = c("IdCategory" = "Id"))
      joined2 <- left_join(joined, locations, by = c("IdLocation" = "Id"))
      joined3 <- left_join(joined2, factors, by =c("IdSource" = "Id"))
      
      # Choosing columns
      selectedCols <- c("Id", "Emission.Category", "Source","Usage", "Factor.value", "Location")
      # Sorting table by Id
      selectedData <- joined3 %>% select(all_of(selectedCols)) %>% arrange(Id)
      return(selectedData)
    })
    
    # OUTPUT => TOTAL EMISSIONS
    output$summary_Text <- renderText({
      mainData <- mergedData()
      mainData <- mainData %>% mutate(Result = Factor.value * Usage)
      sum_result <- sum(mainData$Result)
      sum_result_rounded <- format(round(sum_result, 2), big.mark = " ")
      summary_Text <- paste0("<h1>Total Carbon Footprint of company <b>", sum_result_rounded, "</b> kg of CO2e</h1>")
      HTML(summary_Text)
    })
    
    #OUTPUT => CHART EMISSIONS BY COUNTRIES
    output$EmissionsByCountry <- renderPlot({
      mainData <- mergedData()
      mainData <- mainData %>% mutate(Result = Factor.value * Usage)
      
      ggplot(mainData, aes(x = Location, y = Result)) + 
        geom_bar(stat ="identity", fill ="#FAA0A0") +
        labs(title = "Emissions by Countries [kg CO2e]",
             x = "Country",
             y = "Emissions") +
        theme_minimal() +  
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent", color = NA),
              plot.background = element_rect(fill = "transparent", color = NA),
              text = element_text(size = 12)) +  # font size (all text)
        theme(axis.title = element_text(size = 14),  # font size (axis title)
              plot.title = element_text(size = 16, face = "bold")) +  # font size and style (chart title)
        scale_y_continuous(labels = scales::comma)  # 1k separator
    })
    
    #OUTPUT => CHART EMISSIONS BY SOURCES
    output$EmissionsBySource <- renderPlot({
      mainData <- mergedData()
      mainData <- mainData %>% mutate(Result = Factor.value * Usage)
      
      ggplot(mainData, aes(x = Source, y = Result)) + 
        geom_bar(stat ="identity", fill ="#6da153") +
        labs(title = "Emissions by Sources [kg CO2e]",
             x = "Source",
             y = "Emissions") +
        theme_minimal() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent", color = NA),
              plot.background = element_rect(fill = "transparent", color = NA),
              text = element_text(size = 12)) +  
        theme(axis.title = element_text(size = 14),  
              plot.title = element_text(size = 16, face = "bold")) + 
        scale_y_continuous(labels = scales::comma)  
    })
    
    #OUTPUT => CHART EMISSIONS BY CATEGORIES
    output$EmissionsByCategory <- renderPlot({
      mainData <- mergedData()
      mainData <- mainData %>% mutate(Result = Factor.value * Usage)
      
      ggplot(mainData, aes(x = Result, y = Emission.Category)) + 
        geom_bar(stat ="identity", fill ="gray") +
        labs(title = "Emissions by Categories [kg CO2e]",
             x = "Emissions",
             y = "Category") +
        theme_minimal() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent", color = NA),
              plot.background = element_rect(fill = "transparent", color = NA),
              text = element_text(size = 12)) +  
        theme(axis.title = element_text(size = 14),  
              plot.title = element_text(size = 16, face = "bold")) +  
        scale_x_continuous(labels = scales::comma)  
    })
    
      
    #OUTPUT => DATA TABLE
    output$table <- renderDataTable({
      mergedData()
    })
    
    
    # DECARBONISATION => SELECTED YEAR AND CHART
    selectedYear <- reactive({
      input$nr
    })
    
    selectedYear <- reactive({
      input$nr
    })
    
    # DECARBONISATION => SELECTED YEAR AND CHART
    # Im taking the input form slider on UI
    selectedYear <- reactive({
      input$nr
    })
    
    # Plot making
    output$Decarbonisation <- renderPlot({
      mainData <- mergedData()
      #Decarbonisation factor => 1 year = -2% of emissions
      adjustmentFactor <- 1 - (selectedYear() - 2023) * 0.02
      mainData <- mainData %>% 
        mutate(Result = Factor.value * Usage * adjustmentFactor)
      
      ggplot(mainData, aes_string(x = input$xAxis, y = "Result")) + 
        geom_bar(stat = "identity", fill = "#1e7d4f") +
        labs(
          title = "Emissions [kg CO2e]",
          x = input$xAxis,
          y = "Emissions"
        ) +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold")
        ) +
        scale_y_continuous(labels = scales::comma)
    })
    
    # DECARBONISATION TOTAL EMISSIONS
    output$Decarbo_summary <- renderText({
      mainData <- mergedData()
      adjustmentFactor <- 1 - (selectedYear() - 2023) * 0.02
      mainData <- mainData %>% mutate(Result = Factor.value * Usage *adjustmentFactor)
      sum_result <- sum(mainData$Result)
      sum_result_rounded <- format(round(sum_result, 2), big.mark = " ")
      summary_Text <- paste0("<h1>Total emissions in choosen year: <b>", sum_result_rounded, "</b> kg of CO2e</h1>")
      HTML(summary_Text)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
