#### Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)

#### Load data ----
nutrient_data <- read_csv("Data/NTL-LTER_Lake_Nutrients_PeterPaul_Processed.csv")
nutrient_data$sampledate <- as.Date(nutrient_data$sampledate, format = "%Y-%m-%d")
nutrient_data <- nutrient_data %>%
  filter(depth_id > 0) %>%
  select(lakename, sampledate:po4)

#### Define UI ----
ui <- fluidPage(theme = shinytheme("spacelab"),
                #I changed the shinytheme to cyborg, but it didn't look very good. Cerulean looked good. Spacelab also looks good.
  titlePanel("Nutrients in Peter Lake and Paul Lake"),
  sidebarLayout(
    sidebarPanel(
    #I changed the layout to split layout. sidebar layout did look better
      
      # Select nutrient to plot
      selectInput(inputId = "y", 
                  label = "Nutrient",
                  choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), 
                  selected = "tp_ug"),
      
      # Select depth
      checkboxGroupInput(inputId = "fill",
                         label = "Depth ID",
                         choices = unique(nutrient_data$depth_id),
                         selected = c(1, 7)),
      
      # Select lake
      checkboxGroupInput(inputId = "shape",
                         label = "Lake",
                         choices = c("Peter Lake", "Paul Lake"),
                         selected = "Peter Lake"),

      # Select date range to be plotted
      sliderInput(inputId = "x",
                  label = "Date",
                  min = as.Date("1991-05-01"),
                  max = as.Date("2016-12-31"),
                  value = c(as.Date("1995-01-01"), as.Date("1999-12-31")))),

    # Output
    mainPanel(
      plotOutput("scatterplot", brush = brushOpts(id = "scatterplot_brush")), 
      tableOutput("mytable")
    )))
#the main panel is the window that holds our plot!

#### Define server  ----
server <- function(input, output) {
  
    # Define reactive formatting for filtering within columns
     filtered_nutrient_data <- reactive({
       nutrient_data %>%
         filter(sampledate >= input$x[1] & sampledate <= input$x[2]) %>%
         filter(depth_id %in% input$fill) %>%
         filter(lakename %in% input$shape) 
     })
    
    # Create a ggplot object for the type of plot you have defined in the UI  
       output$scatterplot <- renderPlot({
        ggplot(filtered_nutrient_data(), 
               aes_string(x = "sampledate", y = input$y, 
                          fill = "depth_id", shape = "lakename")) +
          geom_point(alpha = 2, size = 3) +
           #I didnt really see any change when I changed the alpha from 0.8 to 2, although I know this should be affecting the transparency of something
          #next I changed the size from 2 to 5: It made the points of the scatter plot a lot bigger. Perhaps too big. I think 2 or 3 are good sizes for the points
          theme_classic(base_size = 14) +
           #changing the base size from 14 to 20 made the titles and labels too big, because it made them big enough that the plot itself appeared to get smaller. 
          scale_shape_manual(values = c(21, 24)) +
         #changing the scale_shape_manual values from c(21, 24) to c(25,24) caused the shapes to change from triangkles and circles to triangles and upside down triangles
          #changing the second value to 30 made it where all the shapes were circles of the same size... not helpful
           #changing the second value to 10 caused strange bullseyes that I didn't like
          labs(x = "Date", y = expression(Concentration ~ (mu*g / L)), shape = "Lake", fill = "Depth ID") +
          scale_fill_distiller(palette = "YlOrBr", guide = "colorbar", direction = 1)
          #scale_fill_viridis_c(option = "viridis", begin = 0, end = 0.8, direction = -1)
      })
       
    # Create a table that generates data for each point selected on the graph  
       output$mytable <- renderTable({
         brush_out <- brushedPoints(filtered_nutrient_data(), input$scatterplot_brush)
       })
       
  }


#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)

#### Questions for coding challenge ----
#1. Play with changing the options on the sidebar. 
    # Choose a shinytheme that you like. The default here is "yeti"
          #I chose the spacelab shiny theme. I also tried out cyborg and cerulean.
    # How do you change the default settings? 
         #I googled R Shiny settings. I changed the argument of the shinytheme function from yeti the other themes. 
#2. How is the mainPanel component of the UI structured?
        #The mainPanel component is the window that contains the output of our code. In thise case, the mainPanel contains our plot. The mainPanel contains a widget, which is a web component that users can interact with.
    # How does the output appear based on this code?
        #The output appeared as a plot. 
#3. Explore the reactive formatting within the server.
    # Which variables need to have reactive formatting? 
        #the output and input variables need to have reactive formatting so that the server can respond to changes put into the UI
    # How does this relate to selecting rows vs. columns from the original data frame?
        #Having the output and input variables as reactive formatting allowed us to change the columns and rows we selected to create the plot. This allowed us to change the variables we used for the plot.
#4. Analyze the similarities and differences between ggplot code for a rendered vs. static plot.
    # Why are the aesthetics for x, y, fill, and shape formatted the way they are?
        #The y input of the render plot is formatted as 'y = input$y'. It is formatted this way so that the y variable of the plot can be reactive and change as a result of changes in the UI. This allows users to change the variables in the plot by clicking on a different variable in the UI. 
        #X and fill are formatted as the would be for a static ggplot. X and fill don't need to be different for the rendered plot, because the plots for all of the variable options use the same lake depths and time scale.     
# Note: the data frame has a "()" after it. This is necessary for reactive formatting.
    # Adjust the aesthetics, playing with different shapes, colors, fills, sizes, transparencies, etc.
#5. Analyze the code used for the renderTable function. 
    # Notice where each bit of code comes from in the UI and server.
      #the y variable of the plot comes from the UI
      #the output which contains the scatter plot we modify with the render plot code also comes from the UI
    # Note: renderTable doesn't work well with dates. "sampledate" appears as # of days since 1970.
