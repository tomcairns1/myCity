# SanFranciscoTrees.R
# An R shiny app to display the trees of San Francisco

# Import the libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)

# Load the data into the file
trees_filename <- '../data/Street_Tree_List.csv'
trees <- read_csv(trees_filename)

# Modify the columns
trees <- trees %>%
    mutate(genus = str_extract(qSpecies, '[A-Z|a-z]+(?=\\s)'),
           species = str_extract(qSpecies, '(?<=\\s)[A-Z|a-z]+(?=\\s::)'),
           date = mdy(str_extract(PlantDate, '[0-9|/]+(?=\\s)')),
           day = day(date),
           wday = wday(date, label = T),
           month = month(date, label = T),
           year = year(date)) 

# Create a list of the genuses
list_of_genuses <- sort(list(unique(filter(trees, !is.na(genus))$genus))[[1]])

################################################################################

# Define the UI for Tree app
ui <- pageWithSidebar(
    
    # App Title
    headerPanel('Trees of San Francisco'),
    
    # Sidebar panel for inputs
    sidebarPanel(
        
        # Input: Selector for genus of interest
        selectInput('genus', 'Genus:', list_of_genuses)
        
        # Maybe in the future I can add a slider to show the year
    ),
    
    # Main panel for displaying outputs
    mainPanel(
        
        # Output: formatted text for Bar Chart Caption
        h3(textOutput('barCaption')),
        
        # Output: bar chart of the number of species within selected genus
        h3(textOutput('genusBarChart')),
        
        # Output: formatted text for the map caption
        h3(textOutput('mapCaption')),
        tableOutput('table')
        
        # Output: Map of the selected genus
        # plotOutput('genusPlot')
    )
)


################################################################################

# Define the server logic
server <- function(input, output) {
    
    ##############
    # Filter Data
    ##############
    # Filter the data so that it only shows the input genus
    trees.filtered <- reactive({
        trees %>%
            filter(genus == input$genus) %>%
            select(genus, species, Longitude, Latitude) %>%
            head()
    })
    
    # Table to display (for now)
    output$table <- renderTable({
        trees.filtered()
    })
    
    
    ###########
    # Captions
    ###########
    # Caption for the bar chart
    barCaptionText <- reactive({
        paste('The Species of Genus', input$genus)
    })
    
    output$barCaption <- renderText({
        barCaptionText()
    })
    
    # Caption for map
    captionText <- reactive({
        paste('Displaying Trees of Genus:', input$genus, 'in San Francisco')
    })
    
    output$mapCaption <- renderText({
        captionText()
    })
    
    
    #################
    # Visualizations
    #################
    # Create the bar chart of the genus
    output$genusBarChart <- renderPlot(
        trees.filtered() %>%
            ggplot(aes(x = species)) +
            geom_bar()
    )
    
    # # Create the map showing the plots
    # output$genusPlot <- renderPlot({
    #     leaflet(data = trees.filtered) %>% 
    #         addProviderTiles(providers$CartoDB.Positron) %>%
    #         addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 1)
    # })
    
    
}

shinyApp(ui, server)