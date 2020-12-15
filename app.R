
# I include the relevant libraries for my project.

library(shiny) 
library(tidyverse)
library(janitor)
library(ggthemes)
require(scales)
library(shinythemes)
library(ggpattern)
library(ggplot2)
library(urbnmapr)
library(usmap)
library(rstanarm)
library(gtsummary)
library(gt)
library(broom.mixed)

# I load in my data through calls to the read_csv function. I also make sure to 
# properly include the column type for all of the variables.

climatefederal <- read_csv("data/Climate Fundraising Federal.csv",
                           col_type = cols(ID = col_character(),
                                           Jurisdiction = col_character(),
                                           State = col_character(),
                                           District_Type = col_character(),
                                           District_Number = col_character(),
                                           Candidate_Name = col_character(),
                                           Party = col_character(),
                                           Total_Amount = col_double(),
                                           Total_For = col_double(),
                                           Total_Against = col_double(),
                                           PAC_Name = col_character(),
                                           PAC_Type = col_character(),
                                           PAC_ID = col_character(),
                                           ID_Type = col_character(),
                                           Citation_URL = col_character()))

climatestate <- read_csv("data/Climate Fundraising State.csv", 
                        col_type = cols(ID = col_character(),
                                        Jurisdiction = col_character(),
                                        State = col_character(),
                                        District_Type = col_character(),
                                        District_Number = col_character(),
                                        Candidate_Name = col_character(),
                                        Party = col_character(),
                                        Total_Amount = col_double(),
                                        Total_For = col_double(),
                                        Total_Against = col_double(),
                                        PAC_Name = col_character(),
                                        PAC_Type = col_character(),
                                        PAC_ID = col_character(),
                                        ID_Type = col_character(),
                                        Citation_URL = col_character()))

# I bind my federal and state climate fundraising datasets together. 

climate <- rbind(climatefederal, climatestate)

# I perform some data wrangling and cleaning by removing rows with non-values,
# filter to only include Democrats and Republicans, filter to only include
# individuals who have received non-negative contribution amounts (since some
# candidates evidently had negative values reported for some contributions).
# I also change the Party initial to be spelled out in full.

climate <- climate %>%
    select(-District_Number, -PAC_ID, -ID_Type, -Citation_URL, -PAC_Type) %>%
    drop_na() %>%
    filter(Party %in% c("D", "R")) %>%
    filter(Total_Amount >= 0) %>%
    mutate(Party = case_when(Party == "D" ~ "Democrat",
                            Party == "R" ~ "Republican",
                            TRUE ~ "Independent"))

# Here, I create separate tibbles corresponding to the individual's party.

climateDem <- climate %>%
    filter(Party == "Democrat")

climateRep <- climate %>%
    filter(Party == "Republican")

climateInd <- climate %>%
    filter(Party == "Independent")

# Here, I create separate tibbles corresponding to the individual's district
# type.

climateUSHouse <- climate %>%
    filter(District_Type == "U.S. House")

climateUSSenate <- climate %>%
    filter(District_Type == "U.S. Senate")

climateUSPresident <- climate %>%
    filter(District_Type == "President")

# Here, I manipulate the tibble to group by the district type and summarize
# the total amounts of climate fundraising by district type..

climateOffice <- climate %>%
    group_by(District_Type) %>%
    summarize(Total = sum(Total_Amount), .groups = "drop")

# Here, I manipulate the tibble to group by the party and summarize the total
# amounts of climate fundraising by party.

climateParty <- climate %>%
    group_by(Party) %>%
    summarize(Total = sum(Total_Amount), .groups = "drop")

# Here, I filter the climate dataset to include observations that are not 
# unaffiliated with any state (for example, Presidential races do not have a 
# state associated with them).

climateState <- climate %>%
    filter(!(State == "All"))

# Here, I generate a list of all 50 U.S. states for later use.

state_list <- sort(unique(climateState$State))

# Here, I generate a linear regression model using stan_glm, with climate as
# my dataset, and the response variable Total_Amount with predictor variables
# of Party and District Type.

model <- stan_glm(data = climate, Total_Amount ~ Party + District_Type, 
                  refresh = 0, family = gaussian())

################################################################################
################################################################################

# Here, I design the ui (user interface) component of my Shiny app. I have a 
# series of tabs and, within each tab, I include visualizations, interactive 
# features, descriptions of relevant analysis and insights, etc. I establish
# several calls to the plots I generate later on in my project. 

ui <- fluidPage(navbarPage(theme = shinytheme("flatly"), "Climate Fundraising",
                           
        # I create a "By Party" tab that showcases climate fundraising insights
        # and analysis by political party for the political donations.
         
        tabPanel("By Party", titlePanel("Climate Fundraising by Party"), 
             sidebarLayout(sidebarPanel(selectInput("Party", h3("Select Party"), 
                             choices = list("Democrat" = 1, "Republican" = 2), 
                             selected = 1)), 
                           mainPanel(plotOutput("distPlot10"))), 
             h3("Key Insights"), h4("Based on the histogram plots generated 
             above, we observe that Republican candidates tend to attract 
             greater fundraising amounts than Democratic candidates. However, 
             we also observe that there are significantly more Democratic 
             candidates that receive fundraising than Republican candidates. We 
             summarize these trends in the following boxplot."), 
             plotOutput("distPlot11"), 
             h4("Summarizing these results, we observe that more amounts of 
                climate fundraising went to Republican campaigns than 
                Democratic campaigns."), plotOutput("distPlot12")), 
        
        # I create a "By Office" tab that showcases climate fundraising insights
        # and analysis by office.
        
        tabPanel("By Office", titlePanel("Climate Fundraising by Office"), 
             sidebarLayout(sidebarPanel(selectInput("Office", 
                                                    h3("Select Office"), 
                             choices = list("President" = 1, "U.S. Senate" = 2, 
                                            "U.S. House" = 3), selected = 1)), 
                           mainPanel(plotOutput("distPlot8"))), 
             h3("Key Insights"), h4("Based on the histogram plots generated 
             above, we observe that Presidential races tend to attract 
             significantly greater fundraising amounts than Congressional races.
             Moreover, even within the U.S. Congress, U.S. Senate races tend to 
             attract more funding than U.S. House races. We see that, while 
             some Presidential and even U.S. Senate races do attract significant 
             amounts of funding, most U.S. House campaigns consist of fairly 
             small donation amounts. We summarize these trends in the following 
             bar graph."), plotOutput("distPlot9")), 
        
        # I create a "By State" tab that showcases climate fundraising insights
        # and analysis by state. In this tab, I include a map of climate 
        # donations.
        
        tabPanel("By State", titlePanel("Climate Fundraising by State"), 
        sidebarLayout(sidebarPanel(h5("Note: Some states may be missing, since 
        not all states received climate donations."), 
                                   selectInput("State", h3("Select State"), 
                                               choices = state_list, 
                                               selected = 1)), 
                      mainPanel(plotOutput("distPlot13"))), h3("Key Insights"), 
               h4("We notice right away that many states have very few 
                  candidates receiving climate fundraising dollars. We now 
                  compare total climate fundraising amounts by state."), 
        plotOutput("distPlot14"), h4("We now plot a map of total climate 
                                     fundraising across states."), 
             plotOutput("distPlot15")),
        
        # I create a Model tab that describes the linear regression model that
        # I generated using stan_glm. I include the model output, a table of
        # linear regression coefficients, and the interpretation of my model.
        
        tabPanel("Model", titlePanel("Climate Fundraising Predictive Model"), 
                 h3("Overview of Model"), h4("We seek to generate a linear 
                 regression model that is capable of predicting the amount a 
                 candidate is able to receive in fundraising, based on various 
                 predictor variables, including Party, Office, and State. In our 
                 particular model, we choose a stan_glm regression model with 
                 the total climate fundraising amount for a given candidate as 
                 the response variable, and Party and Office as the predictor 
                 variables, along with an intercept term."), h3("Model Output"), 
                 gt_output("distText16"), h3("Interpretation of Model"), 
                 h4("We interpret the model as follows. Our intercept term 
                 indicates that, for a candidate who is a Democrat running for 
                 President, they are predicted to receive $1,847,278 in 
                 donations. For a Republican candidate, they are predicted to 
                 receive $340,329 more in climate donations than a Democratic 
                 candidate (running for the same position). Meanwhile, a 
                 Presidential candidate is predicted to receive $1,853,297 more
                 in climate donations than a comparable U.S. House candidate and 
                 $1,746,177 more in climate donations than a comparable U.S. 
                 Senate candidate. Our table above also includes 95% confidence
                 intervals for the parameters previously mentioned.")),
        
        # I create an About page that describes myself, my project, and my 
        # data sources for this project, including a link to my GitHub
        # repository.
        
        tabPanel("About", h2("About Me"), 
             h3("My name is Charles Hua, and I am a junior at Harvard College
             studying Statistics and Mathematics with a minor in Energy and 
             Environment. I'm deeply passionate about climate and politics and 
             decided to pursue a project exploring the landscape of climate
             fundraising for federal and state political campaigns."), 
             h2("About My Project"), h3("Climate and environmental issues are 
             playing an increasingly important role in political campaigns, 
             driving increased voter turnout and greater prominence of climate 
             change in political messaging. A key consequence of this trend has 
             been in climate political donations, where we've begun to see 
             several climate- and environment-oriented political action 
             committees (or PACs) making contributions to federal and state 
             candidates. My project aims to understand the current landscape of 
             climate political fundraising in the United States, at both the 
             federal and state level, to understand where money is coming from 
             and where money is going. Through this work, I've realized that 
             there remain many inefficiencies in the way climate-oriented 
             donations are made, with over-emphasis on Presidential and federal 
             races, and less emphasis on Congressional and state races, 
             presenting opportunities for revamped strategy in terms of climate 
             donations."), h2("About My Data"), h3("I leveraged publicly 
             accessible data from the Federal Election Commission and from 
             individual state election commissions to generate a 
             one-of-its-kind, comprehensive dataset exploring donations to and
             expenditures of climate-oriented political action committees (PACs)
             and campaigns. I hope you enjoy the project!"),  
             h4("Link to GitHub Repo: 
                https://github.com/charles-hua/climatefundraising"))
)
)

server <- function(input, output, session) {
    
    # Here, I generate a histogram to display general climate fundraising data.

    output$distPlot <- renderPlot({
        
        # I generate bins based on input$bins from ui.R.
        
        x    <- climate$Total_Amount
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # I plot the histogram with the specified number of bins. 
        
        ggplot(mapping = aes(x)) + 
            geom_histogram(breaks = bins, fill = "dodgerblue", 
                           border = "black") + 
            labs(title = "Climate Fundraising Data", x = "Donation Amount", 
                 y = "Count") + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a histogram to display general climate fundraising data.
    
    output$distPlot2 <- renderPlot({
        
        # I generate bins based on input$bins from ui.R.
        
        x    <- climate$Total_Amount
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # I plot the histogram with the specified number of bins. 
        
        ggplot(mapping = aes(x)) + 
            geom_histogram(breaks = bins, fill = "dodgerblue", 
                           border = "black") + 
            labs(title = "Climate Fundraising Data", x = "Donation Amount", 
                 y = "Count") + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a boxplot to display general climate fundraising data by
    # party.
    
    output$distPlot3 <- renderPlot({
        
        # I generate bins based on input$bins from ui.R.
        
        x <- climate$Total_Amount 
        ggplot(mapping = aes(climate$Party, x)) + 
            geom_boxplot() + 
            labs(title = "Climate Fundraising Data", x = "Donation Amount", 
                 y = "Count") + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a histogram to display general climate fundraising data
    # for Democrats. I include a log-scale transformation.
    
    output$distPlot4 <- renderPlot({
        xDem <- climateDem$Total_Amount
        ggplot(mapping = aes(xDem)) + 
            geom_histogram(bins = 30, fill = "dodgerblue", alpha = 0.7) + 
            labs(title = "Climate Fundraising for Democrats", 
                 x = "Donation Amount", y = "Candidates") + 
            scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a histogram to display general climate fundraising data
    # for Republicans. I include a log-scale transformation.
    
    output$distPlot5 <- renderPlot({
        xRep <- climateRep$Total_Amount
        ggplot(mapping = aes(xRep)) + 
            geom_histogram(bins = 30, fill = "red", alpha = 0.7) + 
            labs(title = "Climate Fundraising for Republicans", 
                 x = "Donation Amount", y = "Candidates") + 
            scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a histogram to display general climate fundraising data
    # for U.S. House candidates. I include a log-scale transformation.
    
    output$distPlot6 <- renderPlot({
        xUSHouse <- climateUSHouse$Total_Amount
        ggplot(mapping = aes(xUSHouse)) + 
            geom_histogram(bins = 30, fill = "forestgreen", alpha = 0.7) + 
            labs(title = "Climate Fundraising for U.S. House Candidates", 
                 x = "Donation Amount", y = "Candidates") + 
            scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a histogram to display general climate fundraising data
    # for U.S. Senate candidates. I include a log-scale transformation.
    
    output$distPlot7 <- renderPlot({
        xUSSenate <- climateUSSenate$Total_Amount
        ggplot(mapping = aes(xUSSenate)) + 
            geom_histogram(bins = 30, fill = "forestgreen", alpha = 0.7) + 
            labs(title = "Climate Fundraising for U.S. Senate Candidates", 
                 x = "Donation Amount", y = "Candidates") + 
            scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a histogram to display general climate fundraising data
    # for U.S. President candidates. I include a log-scale transformation.
    
    output$distPlot8 <- renderPlot({
        if(input$Office == 1) {
            xUSPresident <- climateUSPresident$Total_Amount
            ggplot(mapping = aes(xUSPresident)) + 
                geom_histogram(bins = 20, fill = "red", alpha = 0.7) + 
                labs(title = "Climate Fundraising for U.S. Presidential 
                     Candidates", x = "Donation Amount", y = "Candidates") + 
                scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", 
                                                    math_format(10^.x))) + 
                scale_x_continuous(labels = scales::dollar) + 
                scale_y_continuous(label = scales::number_format(accuracy = 1)) + 
                theme_fivethirtyeight()
        }
        
        else if(input$Office == 2) {
            xUSSenate <- climateUSSenate$Total_Amount
            ggplot(mapping = aes(xUSSenate)) + 
                geom_histogram(bins = 20, fill = "dodgerblue", alpha = 0.7) + 
                labs(title = "Climate Fundraising for U.S. Senate Candidates", 
                     x = "Donation Amount", y = "Candidates") + 
                scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", 
                                                    math_format(10^.x))) + 
                scale_x_continuous(labels = scales::dollar) + 
                scale_y_continuous(label = scales::number_format(accuracy = 1)) + 
                theme_fivethirtyeight()
        }
        
        else if(input$Office == 3) {
            xUSHouse <- climateUSHouse$Total_Amount
            ggplot(mapping = aes(xUSHouse)) + 
                geom_histogram(bins = 10, fill = "white", alpha = 0.7) + 
                labs(title = "Climate Fundraising for U.S. House Candidates", 
                     x = "Donation Amount", y = "Candidates") + 
                scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", 
                                                    math_format(10^.x))) + 
                scale_x_continuous(labels = scales::dollar) + 
                scale_y_continuous(label = scales::number_format(accuracy = 1)) + 
                theme_fivethirtyeight() 
        }
    })
    
    # Here, I generate a bar plot summarizing climate fundraising data by 
    # office or district type. I also change the labels on the scales. 
    
    output$distPlot9 <- renderPlot({
        ggplot(mapping = aes(x = fct_relevel(climateOffice$District_Type, 
                                             "President", "U.S. Senate", 
                                             "U.S. House"), 
                             y = climateOffice$Total, 
                             fill = climateOffice$District_Type)) + 
            geom_col(stat = "identity", position = "dodge", alpha = 0.7) + 
            labs(title = "Climate Fundraising by Office", x = "Office", 
                 y = "Total Climate Fundraising Amount", 
                 fill = "Office") + 
            scale_fill_manual(values = c("red", "white", "dodgerblue")) + 
            theme(legend.position = "none") + 
            scale_y_continuous(label = scales::number_format(accuracy = 1)) + 
            scale_y_continuous(labels=scales::dollar_format()) + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a histogram to display general climate fundraising data
    # for Democrats. I also change the labels on my scales.
    
    output$distPlot10 <- renderPlot({
        if(input$Party == 1) {
            xDem <- climateDem$Total_Amount
            ggplot(mapping = aes(xDem)) + 
                geom_histogram(bins = 20, fill = "dodgerblue", alpha = 0.7) + 
                labs(title = "Climate Fundraising for Democratic Candidates", 
                     x = "Donation Amount", y = "Candidates") + 
                scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", 
                                                    math_format(10^.x))) + 
                scale_x_continuous(labels = scales::dollar) + 
                scale_y_continuous(label = scales::number_format(accuracy = 1)) + 
                theme_fivethirtyeight()
        }
        
        else if(input$Party == 2) {
            xRep <- climateRep$Total_Amount
            ggplot(mapping = aes(xRep)) + 
                geom_histogram(bins = 20, fill = "red", alpha = 0.7) + 
                labs(title = "Climate Fundraising for Republican Candidates", 
                     x = "Donation Amount", y = "Candidates") + 
                scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", 
                                                    math_format(10^.x))) + 
                scale_x_continuous(labels = scales::dollar) + 
                scale_y_continuous(label = scales::number_format(accuracy = 1)) + 
                theme_fivethirtyeight()
        }
    })
    
    # Here, I generate a boxplot to display climate fundraising data by party.
    
    output$distPlot11 <- renderPlot({
        ggplot(mapping = aes(x = climate$Party, y = climate$Total_Amount, 
                             fill = climate$Party)) + 
            geom_boxplot(alpha = 0.7) + 
            ylim(0, 10000) + 
            scale_fill_manual(values = c("dodgerblue", "red")) + 
            labs(title = "Distribution of Climate Fundraising by Party", 
                 x = "Donation Amount", y = "Total Climate Fundraising") + 
            theme(legend.position = "none") + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a histogram to display a summary of climate fundraising
    # data by party. I also change the format of the labels on the scales.
    
    output$distPlot12 <- renderPlot({
        ggplot(mapping = aes(x = climateParty$Party, 
                             y = climateParty$Total, 
                             fill = climateParty$Party)) + 
            geom_col(stat = "identity", position = "dodge", alpha = 0.7) + 
            labs(title = "Climate Fundraising by Party", 
                 x = "Party", y = "Total Climate Fundraising Amount", 
                 fill = "Party") + 
            scale_fill_manual(values = c("dodgerblue", "red")) + 
            scale_y_continuous(labels = scales::dollar) + 
            theme(legend.position = "none") + 
            scale_y_continuous(label = scales::number_format(accuracy = 1)) + 
            scale_y_continuous(labels=scales::dollar_format()) + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a geom_point() plot to display climate fundraising
    # data by state. I also change the labels on the scales.
    
    output$distPlot13 <- renderPlot({
        state <- climateState %>%
            filter(State == input$State)
        
        xState <- state$Total_Amount
        
        ggplot(mapping = aes(x = state$State, y = xState)) + geom_point() + 
            labs(x = "", y = "Climate Fundraising Amount") + 
            scale_y_continuous(labels = scales::dollar) +
            scale_y_continuous(label = scales::number_format(accuracy = 1)) + 
            theme_fivethirtyeight()
    })
    
    # Here, I generate a scatterplot to display climate fundraising data by 
    # state. I manually included my own sets of labels.
    
    output$distPlot14 <- renderPlot({
        statefundraising <- climate %>%
            filter(!(State == "All")) %>%
            group_by(State) %>%
            summarize(StateTotal = sum(Total_Amount), .groups = "drop")
        
        statefundraising %>%
            ggplot(aes(StateTotal, State)) + geom_point() + 
            theme_fivethirtyeight() + 
            scale_x_continuous(breaks = c(0, 2000000, 4000000, 6000000), 
                               labels = c("$0 million", "$2 million", 
                                          "$4 million", "$6 million")) +
            labs(title = "Federal Climate Fundraising by State", 
                 x = "Donations", y = "State")
    })
    
    # Here, I generate a map displaying climate fundraising data by state.
    
    output$distPlot15 <- renderPlot({
        statefundraising <- climate %>%
            filter(!(State == "All")) %>%
            rename("state" = State) %>%
            group_by(state) %>%
            summarize(StateTotal = sum(Total_Amount), .groups = "drop")
        
        # Here, I read in a CSV file containing state longitude and latitude
        # values in order to generate the dataset.
    
        geography <- read_csv("data/statelatlong.csv", 
                              col_types = cols(Latitude = col_double(), 
                                               Longitude = col_double())) %>%
            rename("state" = State)
        
        # Here, I merge the statefundraising and geography datasets.
        
        statefundraising <- merge(statefundraising, geography, by = "state") %>%
            select(-City)
        
        # Here, I actually plot the map of climate fundraising for states.
        
        plot_usmap(data = statefundraising, values = "StateTotal", 
                   color = "black", labels=FALSE) + 
            scale_fill_continuous( low = "white", high = "green3", 
                                   name = "Amount Raised ($)", 
                                   label = scales::comma) + 
            theme(legend.position = "right") + 
            labs(title = "Federal Climate Fundraising by State", 
                 caption = "Source: OpenSecrets") 
    })
    
    # Here, I generate a linear regression table that includes the model 
    # coefficients and estimates.

    output$distText16 <- render_gt({
        tbl_regression(model, intercept = TRUE) %>% 
            as_gt()
    })
}

# Note: I have chosen to not include captions including data sources for my 
# visualizations, because they come from multiple sources, which are all 
# acknowledged in the About page of the project. Especially becomes the data 
# come from both federal and state sources, there would be too many sources to 
# include for a given visualization.

shinyApp(ui, server)

