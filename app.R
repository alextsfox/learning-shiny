library(tidyverse)
library(shiny)
library(shinythemes)
library(palmerpenguins)

#### Initialize objects to use before running anything ####
# any additional data should be loaded in here

# load in the 'penguins' dataset
dat <- penguins


#### User Interface: User clicks on stuff #####

# here we define things for the user to click on or text they see, like dropdown bars and the like.
# the ui doesn't do any computations on R objects, but it makes calls to the backend and tells the backend what to compute
ui <- fluidPage(
  
  titlePanel("Penguin Shiny App"),  # webpage title
  
  # sidebarLayout defines a webpage that has a sidebar and a main area
  sidebarLayout(  
    
    # what's in the sidebar? In our case, widgets
    # widgets are things that users interact with: multiple choice buttons, sliders, dropdown menus, etc
    sidebarPanel(
      # the title displayed to the user
      "Widgets to play with.",
      
      # our first widget - dropdown menu (shiny calls this a "selectInput" widget)
      # user make a selection. 
      # No input is processed, but a selection is made and that selection is then interpreted by the backend
      selectInput(
        # the user input sets the value of input$island_select
        inputId = "island_select", 
        # what the user sees at the menu label
        label = "Select an island:",
        # what the user sees as possible choices: a dropdown bar of island names in the penguins dataset
        choices = unique(dat$island)
        ),  
     
      # our second widget - multiple choice selection (shiny calls this a "radioButtons" widget)
      radioButtons(
        # the user input sets the value of input$species_select
        inputId = "species_select",  
        label = "Select a species",
        choices = unique(dat$species)
        )
      
      ),  # end of the sidebarPanel section of sidebarLayout
    
    
    # what's in the main panel? In our case, the widget output
    mainPanel(
      # the title displayed to the user
      "Outputs go here.",
      
      # new paragraph with "p()"
      p('Penguin Plot:'),
      # display the contents of the object output$penguin_plot
      plotOutput(outputId = "penguin_plot"),  # needs a name 
      
      # new paragraph
      p('Bill Table:'),
      # display the contents of the object output$penguin_bills
      tableOutput(outputId = 'penguin_bills')
      )  # end of the mainPanel object
  
  )  # end of the layout type: sidebarLayout
  
)  # end of the ui object


#### Server/Backend: manipulates things and does R behind the scenes ####

# initialize a "server" to carry out command from the interface. Takes an input and an output
server <- function(input, output){
  
  # the user needs to be able to manipulate R objects in real time
  # do this by creating a reactive object, which can...react...to user actions.
  
  # this object takes our dataframe and filters it based on the 'input$island_select' dropdown menu selection
  penguin_island <- reactive({
    dat %>%
      filter(island == input$island_select)
  })
  
  # this object takes our dataframe and filters it based on the 'input$species_select' multiple choice selection
  penguin_species <- reactive({
    dat %>% 
      filter(species == input$species_select) %>% 
      summarize(Length = mean(bill_length_mm, na.rm=T),
                Depth = mean(bill_depth_mm, na.rm=T))
  })
  
  
  # now we can do things with these reactive objects: create a new output that is used above by the mainPanel
  
  # this one is a ggplot object
  # send the plot we created to the output$penguin_plot object used in mainPanel above
  output$penguin_plot <- renderPlot({
    ggplot(penguin_island(),  # penguin_island is a reactive dataset, so we need the ()
           aes(species, body_mass_g, fill=sex)) +
      geom_boxplot() +
      labs(x = 'Species', y = 'Body Mass (g)', fill = 'Sex') +
      theme_minimal()
  })
  
  # this one is a table
  # send the plot we created to the output$penguin_bills object used in mainPanel above
  output$penguin_bills <- renderTable({
    penguin_species()
  })
  
}  # end of the server function

# finally, call the shiny app using the ui and server we created.
shinyApp(ui=ui, server=server)
