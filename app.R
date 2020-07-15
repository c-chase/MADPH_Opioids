#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# There are two objects being defined..."server" and "ui".

library(shiny)
library(tidyverse)
library(devtools)
library(urbnmapr)
library(readxl)
library(viridis)
library(fastDummies)

# read in 2016-2019 data
Rx <- read_excel(
    "MADPH_PMP.xlsx",
    range = cell_cols("A:H") #cell_cols selects all rows associated w those columns
)


# read in 1st quarter of 2015
Rx.q1 <- read_excel(
    "pmp-county-data-roll-q1-2015.xlsx",
    range = "A5:F20"
)

#adding in "year" and "quarter" to "Rx.q1"
Rx.q1 <- Rx.q1 %>% 
    mutate(Year = rep(2015, nrow(Rx.q1)))  %>% #creating a new column,"Year". For every row in the column "Year" in "Rx.q1," write "2015." 
    mutate(Quarter = rep(1, nrow(Rx.q1))) 


# read in 2nd quarter of 2015
Rx.q2 <- read_excel(
    "pmp-county-data-roll-q2-2015.xlsx",
    range = "A5:F20"
)

#adding in "year" and "quarter" to "Rx.q2"
Rx.q2 <- Rx.q2 %>% 
    mutate(Year = rep(2015, nrow(Rx.q2)))  %>% #creating a new column,"Year". For every row in the column "Year" in "Rx.q2," write "2015." 
    mutate(Quarter = rep(2, nrow(Rx.q2))) 


#reading quarter 3, 2015
Rx.q3 <- read_excel(
    "pmp-county-data-roll-q3-2015.xlsx",
    range = "A5:F20"
)


Rx.q3 <- Rx.q3 %>% 
    mutate(Year = rep(2015, nrow(Rx.q3)))  %>% #creating a new column,"Year". For every row in the column "Year" in "Rx.q3," write "2015." 
    mutate(Quarter = rep(3, nrow(Rx.q3))) 


#merge all of the county data together from 2015, and then to the 2016-2019
full.rx <- Rx.q1 %>% 
    full_join(Rx.q2) %>% 
    full_join(Rx.q3) %>% 
    full_join(Rx) %>% 
    set_names( c("County", "Population", "Total Rx", "Total Rx Units", "N.People w/ Rx", "Percent of County Pop w/ Rx", "Year", "Quarter")) %>%  #renaming all of the names
    mutate(county_name = paste(County, "County")) %>%   #mutating a new column, county_name...on the other side is the expression to make a row. paste from the currently existing column, "County," and then add the string "County". It's evaluated on a row by row basis.  
    dummy_rows(select_columns = c("Year", "Quarter", "County")) #making dummy rows for NAs

#define "test"
test <- c("#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8", "#756bb1", "#54278f")


#"ma.percents" just has the year, the quarter, and the % of the population reciving a scheduled Rx for all of MA.
ma.percents <- full.rx %>%  #making a dataframe called ma.percents
    filter(County == "MA") %>%  #we only want to look at "MA" in the County data
    select(Year, Quarter, `Percent of County Pop w/ Rx`) %>%  #select picks only "year", "quarter", and that percent
    rename(ma.percent = `Percent of County Pop w/ Rx`) #renaming so that a later join will result in a new column

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    

    # Application title
    titlePanel("MA Schedule II Rx by County"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        sep = "", #gets rid of commas in year
                        min = 2015,
                        max = 2020,
                        value = 2015),
            sliderInput("quarter",
                        "Quarter:",
                        min = 1,
                        max = 4,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        
        mainPanel(plotOutput("map1"), plotOutput("map2")
           
        )
    )
)

# Define server logic required to draw a histogram
# any functional code will be on the server side of things
# if you want the output to be saved such that it can be used by the UI...you need to save it to a variable within the output object.

server <- function(input, output) {

    output$map1 <- renderCachedPlot({
        full.rx %>%
            filter("county_name" != "MA County")%>% 
            left_join(counties, by = "county_name") %>% 
            filter(state_name == "Massachusetts") %>% 
            filter(Year == input$year, Quarter == input$quarter) %>% 
            ggplot(mapping = aes(long, lat, group = group, fill = `Percent of County Pop w/ Rx`)) +
            labs(title = "Percent of Population Prescribed Schedule II Medications, by County")+
            coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
            geom_polygon(color = "#ffffff", size = .25) +
            theme_void() +
            scale_fill_viridis(colors = test, limits = c(0,10), na.value = "orange") #"limits" gives upper and lower bound 
        
    },
    cache = diskCache(),
    cacheKeyExpr = list(input$year, input$quarter) #allows you to have a unique value for each cached plot
    )
    
    output$map2 <- renderCachedPlot({
        full.rx %>% #piping in full.rx, and doing a left join with the dataframe that we just made 
            left_join(ma.percents) %>% #bc full.rx is piped in, it's the left hand dataframe
            mutate(difference = `Percent of County Pop w/ Rx` - ma.percent) %>% #making a column called "difference"...diff btwn county %age and MA %age
            # view()
            filter(County != "MA") %>% #removing MA county
            left_join(counties, by="county_name") %>% #joining it into the counties dataframe, which we need to make a map
            filter(state_name == "Massachusetts") %>% 
            filter(Year == input$year, Quarter == input$quarter) %>% 
            ggplot() +
            aes(long, lat, group = group, fill = `difference`) +
            geom_polygon(color = "gray", size = 0.25) +
            coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
            theme_void()  +
            scale_fill_gradient2(
                low = "blue",
                mid = "white",
                high = "red",
                limits = c(-2,2),
                midpoint = 0) + #manually setting the midpoint as zero 
            labs(title = "Difference from State % of Pop. Receiving Schedule 2 Rx by County")
        },
        cache = diskCache(),
        cacheKeyExpr = list(input$year, input$quarter) #allows you to have a unique value for each cached plot
        )
        
}

# Run the application 
shinyApp(ui = ui, server = server)
