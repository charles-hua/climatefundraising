library(shiny) # you may need to install.packages() this
library(tidyverse)
library(janitor)
library(ggthemes)
library(fec16)

# This is just a normal object

# Make change to your dataset

# Note: I only selected for donations of less than or equal to $10,000
# to effectively "remove outliers." 

climatedata <- read_csv("ClimateSuperPACs.csv") %>%
    filter(Donor_Amount <= 10000)

######################################################################################
######################################################################################
#
# 1. Shiny Apps have two basic parts to them
#
#   - The user interface (UI) defines how the app should look.
#
#     -- For example, the text on the page, the placement of the elements, etc.
#
#   - The server defines how the app should behave.
#
#     -- This is how live elements are updated - like selecting a state from a list.
#
#   - Those two pieces are combined by running shinyApp(ui, server) to create the app.
#
#      -- You can also click the green "Run App" button on the top right or
#         run runApp() in the console

ui <- fluidPage(navbarPage(
    "Charles Hua Gov 50",
    
    tabPanel(
        "Main",
        
        # - UIs are built from "panel" functions, which specify areas of your page.
        #
        #   -- There is a "main panel," a "sidebar," a "title," etc.
        
        # Here is a sidebar!
        
        #sidebarPanel(
            
        #    sliderInput(
        #        inputId = "selected_size",                  # a name for the value you choose here
        #        label = "Choose a number as the bin width:", # the label to display above the slider
        #        min = 0,                                    # the min, max, and initial values
        #        max = 30,
        #        value = 2 
        #    )
            
        #),
        
        # Application title
        titlePanel("Climate Fundraising Data"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                sliderInput("bins",
                            "Number of bins:",
                            min = 1,
                            max = 30,
                            value = 2)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distPlot")
            )
        ),
        
        # And here is your "main panel" for the page.
        
        mainPanel(
            # - You can also make your UI more complicated with UI elements.
            #
            #   -- In general, these are defined by functions that you give arguments to 
            #      (e.g. min and max values).
            #
            # - These include:
            #
            #   -- selectInput() to choose from multiple options.
            #
            #   -- sliderInput() lets you choose a value from a slider of values you define.
            #
            #   -- radioButtons() let you choose a button from a number of options
            #
            #   -- textInput() lets you enter whatever text you want.
            #
            #   -- Lots of other options, like entering a date. Look at the resources for 
            #      other choices!
            #
            # - You then assign these inputs to a value and use those values in other places, 
            #   like in plots!
            #
            # - All of these functions have their own arguments. For example:
            
            
            #radioButtons(
            #    inputId = "selected_color",             # a name for the value you choose here
            #    label = "Choose a color!",              # the label to display above the buttons
            #    choices = c("red", "blue", "green")     # the button values to choose from
            #),
            
            #textInput(
            #    inputId = "entered_text",               # a name for the value you choose here
            #    label = "Place your title text here:",  # a label above the text box
            #    value = "Example Title"                 # an initial value for the box
            #),
            
            #textOutput("state_message"),              # load a text object called "state_message"
            #textOutput("size_message"),
            #textOutput("color_message"),
            #textOutput("text_message"),
            #plotOutput("state_plot")
        )
    ),
    tabPanel("About", h2("About Me!"), 
             h3("This page is about me and my project! My name is Charles Hua,
                and I am a junior at Harvard College studying Statistics and 
                Math with a minor in Energy and Environment. I'm deeply passionate
                about politics and climate change, so I was interested in looking
                at the landscape of climate fundraising and donations as it relates
                to federal and state political campaigns. Enjoy!"), 
                h3("While I'm still in the process of assembling my final dataset, I'm
                   current planning on leveraging data from the Federal Elections 
                   Commission and from individual state election commissions, 
                   before wrangling together a dataset of donations to climate-related
                   PACs and climate-oriented candidates."),  
             h4("URL to Repo: https://github.com/charles-hua/gov50finalproject.git")),
    tabPanel("Graph", titlePanel("Climate Fundraising Data"), 
             h2("We are displaying the Climate Fundraising Data from the main page
                to test the process of adding a new tab and showing another visualization in Shiny."), 
        sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 30,
                        value = 2)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot2")
        )
    ),
    )
)
)

server <- function(input, output, session) {
    # - Then, you use these named objects to update the data on your site via the input object.
    #
    #   -- render() functions are what show content that will change live on your site.
    #
    #   -- so here, renderText() is updating live text based on your choice.

    
    # This line makes our dataset reactive.
    # That is, we can update it based on the values of input that define above.
    
    # Just like renderText(), we can renderPlot()!
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- climatedata$Donor_Amount 
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        ggplot(mapping = aes(x)) + 
            geom_histogram(breaks = bins, fill = "dodgerblue", border = "black") + 
            labs(title = "Climate Fundraising Data", x = "Donation Amount", 
                 y = "Count")
        
        # hist(x, breaks = bins, col = "dodgerblue", border = "white") 
    })
    
    output$distPlot2 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- climatedata$Donor_Amount 
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        ggplot(mapping = aes(x)) + 
            geom_histogram(breaks = bins, fill = "dodgerblue", border = "black") + 
            labs(title = "Climate Fundraising Data", x = "Donation Amount", 
                 y = "Count")
        
        # hist(x, breaks = bins, col = "dodgerblue", border = "white") 
    })
   
}

shinyApp(ui, server)