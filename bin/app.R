# SanFranciscoTrees.R
# An R shiny app to display the trees of San Francisco

# Import the libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(scales)

# Load the data into the file
# trees_filename <- 'https://data.sfgov.org/api/views/tkzw-k3nq/rows.csv?accessType=DOWNLOAD'
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
        plotOutput('genusBarChart'),
        
        # Output: formatted text for the map caption
        h3(textOutput('mapCaption')),
        
        # Output: Map of the selected genus
        leafletOutput('genusPlot'),
        
        # Output: formatted text for line chart caption
        h3(textOutput('lineChartCaption')),
        
        # Output: Plot of the planting history
        plotOutput('linePlot')
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
            filter(genus == input$genus, !is.na(species)) %>%
            select(genus, species, Longitude, Latitude, year)
    })
    
    # Create a color palette based on the number of species    
    col_palette <- reactive({
        hue_pal()(length(unique(trees.filtered()$species)))
    })
    
    # Set the color palette for leaflet
    pal <- reactive({
        colorFactor(
            palette = col_palette(),
            domain = trees.filtered()$species
        )
    })
    
    # Count and order by species
    ordered_species <- reactive({
        trees.filtered() %>%
            filter(!is.na(species)) %>%
            group_by(species) %>%
            summarize(count = n()) %>%
            arrange(count) %>%
            mutate(color = pal()(species)) # if I can get this to work I think I'm good
    })
    
    # Create a new df containing the color of each tree
    full_trees <- reactive({
        trees.filtered() %>%
            left_join(ordered_species(), by = 'species')
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
    
    # Caption for line chart
    lineChartText <- reactive({
        paste('History of', input$genus, 'Trees Planted')
    })
    
    output$lineChartCaption <- renderText({
        lineChartText()
    })
    
    
    #################
    # Visualizations
    #################
    # Create the bar chart of the genus
    output$genusBarChart <- renderPlot({
        if (nrow(trees.filtered() > 0)) {
            full_trees() %>%
                
                # Visualization
                ggplot(aes(x = species, fill = color)) +
                geom_bar() +
                theme(panel.background = element_blank(), 
                      axis.line = element_line(), legend.position = 'none',
                      axis.text = element_text(size = 17),
                      axis.title = element_text(size = 20)) +
                labs(x = 'Species Name', y = 'Count', fill = '') +
                scale_x_discrete(expand = c(0, 0)) +
                scale_y_continuous(expand = c(0, 0)) +
                coord_flip()
        }
    })
    
    # Create the map showing the plots
    output$genusPlot <- renderLeaflet({
        leaflet(data = full_trees()) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 2,
                             popup = ~species, color = ~pal(species))
    })
    
    # Create a line chart showing time 
    output$linePlot <- renderPlot({
        trees.filtered() %>%
            filter(!is.na(year)) %>%
            group_by(year) %>%
            summarize(count = n()) %>%
            ggplot(aes(x = year, y = count)) +
            geom_line(color = 'steelblue') +
            theme(panel.background = element_blank(), 
                  axis.line = element_line()) +
            scale_x_continuous(expand = c(0, 0)) +
            scale_y_continuous(expand = c(0, 0)) +
            labs(x = 'Year', y = 'Number of Trees Planted')
    })
}

shinyApp(ui, server)

