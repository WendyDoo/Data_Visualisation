# read csv data sets
crime_alcohol_2019 <- read.csv("CR_AL_2019.csv")
crime_alcohol_2020 <- read.csv("CR_AL_2020.csv")
covid_data <- read.csv("COVID2020_quarterly.csv")


# import library 
library(ggplot2)
library(dplyr)
library(lubridate)
library(shiny)
library(plotly)
library(hrbrthemes)
library(shinyWidgets)
library(hablar)
library(RColorBrewer)
library(highcharter) 
library(gganimate)
library(gifski)
library(png)

# Input:
# Extract and subset data 
# Transform data for Visualization 1
year2019 <- c(rep("2019", 11))
year2020 <- c(rep("2020", 11))

crime_alcohol_2019 <- cbind(crime_alcohol_2019, year2019)
crime_alcohol_2020 <- cbind(crime_alcohol_2020, year2020)

colnames(crime_alcohol_2020)[5] <- "Total.crimes"

df2019 <- data.frame("AreaName" = crime_alcohol_2019$Area.Name, 
                     "Total_crimes" = crime_alcohol_2019$Total.crimes, 
                     "Year" = crime_alcohol_2019$year2019,
                     "AreaCode" = crime_alcohol_2019$Area.Code,
                     "AlcoholDeaths" = crime_alcohol_2019$Total.Deaths) 
df2019 <- df2019[-c(1), ]

df2020 <- data.frame("AreaName" = crime_alcohol_2020$Area.Name, 
                     "Total_crimes" = crime_alcohol_2020$Total.crimes, 
                     "Year" = crime_alcohol_2020$year2020,
                     "AreaCode" = crime_alcohol_2020$Area.Code,
                     "AlcoholDeaths" = crime_alcohol_2020$Total.Deaths) 
df2020 <- df2020[-c(1), ]

df_combined_yrs <- rbind(df2019, df2020)

# Transform data for Visualization 2 

df2019$"Colours" <- c("#d5d5f2", "#3b3bd1", "#7d7de8", "#bcbcf7", "#afaffa",
                      "#9b9bf2", "#0707ab","#5d5dc9", "#cdcdf7", "#cfcfe3")

df2020$"Colours" <- c("#d5d5f2", "#5d5dc9", "#7d7de8", "#bcbcf7", "#afaffa",
                      "#9b9bf2", "#0707ab","#3b3bd1", "#cdcdf7", "#cfcfe3")

df2019$Total_Numbers = rowSums(df2019[,c("Total_crimes", "AlcoholDeaths")])

df2020$Total_Numbers = rowSums(df2020[,c("Total_crimes", "AlcoholDeaths")])

# Transform data for Visualization 3
# Transform data to create new data frames for plotting heat maps 2019
crime1 <- c(rep("Violence against person", 10))
crime2 <- c(rep("Homicide", 10))
crime3 <- c(rep("Violence with injury", 10))
crime4 <- c(rep("Violence without injury", 10))
crime5 <- c(rep("Stalking & Harassment", 10))
crime6 <- c(rep("Sexual offences", 10))
crime7 <- c(rep("Robbery", 10))
crime8 <- c(rep("Theft offences", 10))
crime9 <- c(rep("Drug offences", 10))
crime10 <- c(rep("Shoplifting", 10))

# Data set in 2019
df3 <- crime_alcohol_2019[-c(1), ]
c1 <- df3$Violence.against.the.person
area <- df3$Area.Name

type1 <- cbind("Area" = area, "Crime type" = crime1, "Value" = c1)

c2 <- df3$Homicide
type2 <- cbind("Area" = area, "Crime type" = crime2, "Value" = c2)

c3 <- df3$Violence.with.injury
type3 <- cbind("Area" = area, "Crime type" = crime3, "Value" = c3)

c4 <- df3$Violence.without.injury
type4 <- cbind("Area" = area, "Crime type" = crime4, "Value" = c4)

c5 <- df3$Stalking.and.harassment
type5 <- cbind("Area" = area, "Crime type" = crime5, "Value" = c5)

c6 <- df3$Sexual.offences
type6 <- cbind("Area" = area, "Crime type" = crime6, "Value" = c6)

c7 <- df3$Robbery
type7 <- cbind("Area" = area, "Crime type" = crime7, "Value" = c7)

c8 <- df3$Theft.offences
type8 <- cbind("Area" = area, "Crime type" = crime8, "Value" = c8)

c9 <- df3$Drug.offences
type9 <- cbind("Area" = area, "Crime type" = crime9, "Value" = c9)

c10 <- df3$Shoplifting
type10 <- cbind("Area" = area, "Crime type" = crime10, "Value" = c10)

df_plot3 <- rbind.data.frame(type1, type2, type3, type4, type5, type6, type7,
                             type8, type9, type10)

# Data frame of 1st heat map
df_plot3 <- df_plot3 %>% 
  mutate(text = paste0("Area name: ", Area, "\n", "Year: 2019", "\n", 
                       "Crime type: ", `Crime type`, "\n", "Number: ", Value, "\n"))

df_plot3 <- df_plot3 %>% 
  convert(int(Value))



# Transform data to plot heat map in 2020
df4 <- crime_alcohol_2020[-c(1), ]
d1 <- df4$Violence.against.the.person
area <- df4$Area.Name

data1 <- cbind("Area" = area, "Crime type" = crime1, "Value" = d1)

d2 <- df4$Homicide
data2 <- cbind("Area" = area, "Crime type" = crime2, "Value" = d2)

d3 <- df4$Violence.with.injury
data3 <- cbind("Area" = area, "Crime type" = crime3, "Value" = d3)

d4 <- df4$Violence.without.injury
data4 <- cbind("Area" = area, "Crime type" = crime4, "Value" = d4)

d5 <- df4$Stalking.and.harassment
data5 <- cbind("Area" = area, "Crime type" = crime5, "Value" = d5)

d6 <- df4$Sexual.offences
data6 <- cbind("Area" = area, "Crime type" = crime6, "Value" = d6)

d7 <- df4$Robbery
data7 <- cbind("Area" = area, "Crime type" = crime7, "Value" = d7)

d8 <- df4$Theft.offences
data8 <- cbind("Area" = area, "Crime type" = crime8, "Value" = d8)

d9 <- df4$Drug.offences
data9 <- cbind("Area" = area, "Crime type" = crime9, "Value" = d9)

d10 <- df4$Shoplifting
data10 <- cbind("Area" = area, "Crime type" = crime10, "Value" = d10)

df_plot4 <- rbind.data.frame(data1, data2, data3, data4, data5, data6, data7,
                             data8, data9, data10)

# Data frame of 2nd heat map
df_plot4 <- df_plot4 %>% 
  mutate(text = paste0("Area name: ", Area, "\n", "Year: 2020", "\n", 
                       "Crime type: ", `Crime type`, "\n", "Number: ", Value, "\n"))

df_plot4 <- df_plot4 %>% 
  convert(int(Value))

df_plot4_1 <- df_plot4


#Transform data for Visualization 4
# Stacked Bar Charts 
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
crime_alcohol_2020 <- crime_alcohol_2020[-c(1),]

t1 <- crime_alcohol_2020$Theft.offences
t2 <- crime_alcohol_2020$Violence.against.the.person
t3 <- crime_alcohol_2020$Violence.without.injury
t4 <- crime_alcohol_2020$Total.Deaths
alcohol <- c(rep("Alcohol Deaths", 10))


v1 <- cbind("Area" = area, "Type" = crime8, "Value" = t1)
v2 <- cbind("Area" = area, "Type" = crime1, "Value" = t2)
v3 <- cbind("Area" = area, "Type" = crime4, "Value" = t3)
v4 <- cbind("Area" = area, "Type" = alcohol, "Value" = t4)

# Data frame to plot stacked bar chart 
df_plot4 <- rbind.data.frame(v1, v2, v3, v4)

# Transform data for Visualization 5
# Extract and create new data frame to plot animate bubble chart
q2 <- c(rep(2, 10))
q3 <- c(rep(3, 10))
q4 <- c(rep(4, 10))

case2 <- covid_data$Total.Cases.Q2
case3 <- covid_data$Total.Cases.Q3
case4 <- covid_data$Total.Cases.Q4

a1 <- cbind.data.frame("Area" = area, "Quater" = q2, 
                       "Cases" = case2, "Alcohol deaths" = t4)
a2 <- cbind.data.frame("Area" = area, "Quater" = q3, 
                       "Cases" = case3, "Alcohol deaths" = t4)
a3 <- cbind.data.frame("Area" = area, "Quater" = q4, 
                       "Cases" = case4, "Alcohol deaths" = t4)

# Data frame to plot bubble chart 
df_plot5 <- rbind.data.frame(a1, a2, a3)

# Plot bubble chart
bubble <- ggplot(df_plot5, aes(x = Cases, y = `Alcohol deaths`, 
                     size = `Alcohol deaths`, color = Area)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Quater: {frame_time}', x = 'Positive cases', 
       y = 'Alcohol deaths') +
  scale_colour_manual(values = c("#f7cac9", "#dec2cb", "#a2798f", "#bbbec9",
                                 "#92a8d1","#00c2c7", "#77aaff", "#5588ff", 
                                 "#3366ff", "#f6cf92"))+
  scale_size(range = c(0.6, 17)) +
  transition_time(Quater) +
  ease_aes('linear')
anim_save("bubble_chart.gif", bubble)


# Create UI
ui <- fluidPage(
  titlePanel("Total Recorded Crimes and Number of Alcohol Deaths in England and Wales 
             before and during COVID-19 pandemic Analysis"), br(), br(),
  
  setBackgroundColor( color = c("#ece4f2", "#fde7d6", "#91abdb"),
                      gradient = "linear",
                      direction = "bottom"),
  
  sidebarLayout(position = "left",
                sidebarPanel(h3(textOutput("summary"), align = "center"),
                             p(textOutput("des")), br(),
                             
                             h5("Question 1: "), p(textOutput("q1")), br(),
                             
                             h5("Question 2: "), p(textOutput("q2")), br(),
                             
                             h5("Question 3: "), p(textOutput("q3")), br(),
                             
                             h5("Notice:"),
                             
                             em("After the application displayed, please follow 
                             these steps: "), br(), 
                             
                             em("Step 1: Have a click on the screen to activate 
                                interactions"), br(), 
                             
                             em("Step 2: Hover your mouse over each plot to observe 
                                specific information"), br(),
                             
                             em("Step 3: Click on squares of area legends to filter
                                your choice")
                        
                ),
                
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Recorded Crimes", br(), plotlyOutput(outputId = "plot1_1"), 
                             textOutput("p1_1"), textOutput("text1_1"),
                             textOutput("text1_2"), textOutput("overall1")),
                    
                    tags$head(tags$style("#text1_1{color: #3366ff;}"),
                              tags$style("#text1_2{color: #3366ff;}"),
                              tags$style("#overall1{color: #3366ff;}"),
                              tags$style("#p1_1{color: #3366ff;}")), 
            
                    br(), 
                    
                    tabPanel("Alcohol Deaths", br(), plotlyOutput(outputId = "plot1_2"),
                             textOutput("text2_1"), textOutput("text2_2"),
                             textOutput(("overall2"))), 
                    br(),
                    
                    tags$head(tags$style("#text2_1{color: #3366ff;}"),
                              tags$style("#text2_2{color: #3366ff;}"),
                              tags$style("#overall2{color: #3366ff;}"))
                  ),
                  
                  tabsetPanel(
                    tabPanel("Year 2019", br(), plotlyOutput(outputId = "plot2_1"),
                             textOutput("text3_1"), textOutput("text3_1_1")), 
                    br(), 
                    
                    tabPanel("Year 2020", br(), plotlyOutput(outputId = "plot2_2"),
                             textOutput("text3_2")), 
                    br(),
                    
                    tags$head(tags$style("#text3_1{color: #3366ff;}"),
                              tags$style("#text3_1_1{color: #3366ff;}"),
                              tags$style("#text3_2{color: #3366ff;}"))
                  ),
                  
                  tabsetPanel(
                    tabPanel("Crime Types in 2019", br(), plotlyOutput("plot3_1"),
                             textOutput("text4_1")), 
                    br(),
                    
                    tabPanel("Crime Types in 2020", br(), plotlyOutput("plot3_2"),
                             textOutput("text4_2")),
                    
                    tags$head(tags$style("#text4_1{color: #3366ff;}"),
                              tags$style("#text4_2{color: #3366ff;}"))
                    ), br(), br(), 
                  
                  sidebarLayout(position = "left",
                                sidebarPanel(
                                  p("After plotting the data sets of
                                               total recorded crimes and alcohol deaths,
                                               the top 3 highest crime types and 
                                               total number of people died by 
                                               heavy consuming alcohol during pandemic
                                               are displayed by stacked bar chart"),
                                  
                                  p("North West tends to attain high numbers
                                               in all these sectors. This region did not
                                               reach the peak in criminal factor 
                                               but got the highest in number of
                                               alcohol deaths during Coronavirus
                                               time"),
                                  
                                  p("Wales still remained the lowest in 
                                               these factors")
                                ),
                                mainPanel(plotlyOutput("plot4"))
                                
                  ), br(), br(), 
                  
                  sidebarLayout(position = "left",
                                sidebarPanel(p("The aminated bubble chart shows how 
                                             number of positive COVID-19 cases 
                                             affects the number of alcohol deaths 
                                             during 2020, throughout the first year of
                                             the pandemic in England and Wales"),
                                             
                                             p("Looking at the bubble chart,
                                               we can observe that regions had 
                                               high number of people died by
                                               heavy drinking, also got 
                                               high number of positive cases,
                                               such as North West and South East,
                                               When positive cases increased,
                                               number of alcohol deaths still 
                                               remained highly"),
                                             
                                             p("A new assumption can be 
                                             generated: People heavily drank 
                                             alcohol, could be easily caught 
                                             by COVID-19
                                               "), br(), br(),
                                             
                                             ),
                                mainPanel(imageOutput("plot5"), br(), br(), br(),
                                          br(), h5("Data Scources: "),
                                          p("Coronavirus cases in Wales: 
                                             https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=cumCasesByPublishDate&format=csv"),
                                          p("Coronavirus cases in England:
                                            https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=cumCasesByPublishDate&format=csv"), br(),
                                          p("Alcohol Deaths data:
                                            https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/quarterlyalcoholspecificdeathsinenglandandwales"),
                                          p("Recorded Crimes data: 
                                            https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables"), br())
                  )
                ))
  
)

# Create Server
server <- function(input, output, session) {
  # First heat map
  p1 <- ggplot(df_plot3, aes(x = Area, y = `Crime type`, text=text)) + 
    geom_tile(aes(fill = Value))  + theme_ipsum() + 
    scale_fill_gradient(high = "#06487a",low = "#c7e5ff") + 
    theme(axis.text.x = element_text(angle = 45))
  
  # Second heat map 
  p2 <- ggplot(df_plot4_1, aes(x = Area, y = `Crime type`, text=text)) + 
    geom_tile(aes(fill = Value))  + theme_ipsum() + 
    scale_fill_gradient(high = "#06487a",low = "#c7e5ff") + 
    theme(axis.text.x = element_text(angle = 45))
  
  
  
  # Grouped Bar chart (Visualization 1)
  # Plot grouped bar chart of total number of crimes in 2019 and 2020
  output$plot1_1 <- renderPlotly({
    df_combined_yrs %>%
      plot_ly(x = ~Year,
              y = ~Total_crimes,
              text = ~paste("Number: ", Total_crimes, "\n",
                            "Area name: ", AreaName),
              hoverinfo = "text",
              color = ~AreaName, colors = c("#f7cac9", "#dec2cb", "#a2798f", "#a6aabd",
                                            "#92a8d1","#b4c8ea", "#77aaff", "#5588ff", "#3366ff", "#546bab")) %>%
      add_bars() %>%
      layout(title = "Total Number of Crimes in England and Wales",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Crimes"),
             barmode = "group")
  })
  
  # Plot grouped bar chart of total number of alcohol deaths in 2019 and 2020
  output$plot1_2 <- renderPlotly({
    df_combined_yrs %>%
      plot_ly(x = ~Year,
              y = ~AlcoholDeaths,
              text = ~paste("Number: ", AlcoholDeaths, "\n",
                            "Area name: ", AreaName),
              hoverinfo = "text",
              color = ~AreaName, colors = c("#f7cac9", "#dec2cb", "#a2798f", "#a6aabd",
                                            "#92a8d1","#b4c8ea", "#77aaff", "#5588ff", "#3366ff", "#546bab")) %>%
      add_bars() %>%
      layout(title = "Total Number of Alcohol Deaths in England and Wales",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Alcohol Deaths"),
             barmode = "group")
  })
  
  # Donut chart (Visualization 2)
  # Plot of data 2019
  output$plot2_1 <- renderPlotly({
    plotly::plot_ly(data = df2019, 
                    labels = ~AreaName, 
                    values = ~Total_Numbers, 
                    type = 'pie', hole = 0.4,
                    textinfo='label+percent',
                    showlegend = FALSE, 
                    marker = list(colors = ~Colours)) %>% 
      plotly::layout(title = 'Percentage of Crimes and Alcohol deaths in 2019',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  # Plot of data 2020
  output$plot2_2 <- renderPlotly({
    plotly::plot_ly(data = df2020, 
                    labels = ~AreaName, 
                    values = ~Total_Numbers, 
                    type = 'pie', hole = 0.35,
                    textinfo='label+percent',
                    showlegend = FALSE, 
                    marker = list(colors = ~Colours)) %>% 
      plotly::layout(title = 'Percentage of Crimes and Alcohol deaths in 2020',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  # Heat Map (Visualization 3)
  # Heat map of data 2019
  output$plot3_1 <- renderPlotly({
    ggplotly(p1, tooltip="text") 
  })
  
  # Heat map of data 2020
  output$plot3_2 <- renderPlotly({
    ggplotly(p2,tooltip="text")
  })
  
  # Plot stacked bar chart with interaction
  output$plot4 <- renderPlotly({
    df_plot4 %>%
      plot_ly(x = ~Type,
              y = ~Value,
              text = ~paste("Type: ", Type, "\n",
                            "Number: ", Value, "\n",
                            "Area name: ", Area),
              hoverinfo = "text",
              color = ~Area, colors = c("#cc95a5", "#f7cac9", "#c5b9cd", "#dfdfde",
                                        "#92a8d1","#b4c8ea", "#77aaff", "#5588ff", "#3366ff", "#546bab")) %>%
      add_bars() %>%
      layout(title = "Top 3 Crime Types & Total of Alcohol deaths 
         in COVID-19 pandemic",
             xaxis = list(title = "Catergory"),
             yaxis = list(title = "Number"),
             barmode = "stack")
    
  })
  
  
  
  
  # Plot animate Bubble chart 
  output$plot5 <- renderImage({
    list(src = "bubble_chart.gif", contentType = "image/gif")
  }, deleteFile = TRUE)
  
  # Text contents 
  output$summary <- renderText({"
  SUMMARY"})
  
  output$des <- renderText({"The data visualisation project is exploring three
    interesting questions about number of recorded crimes, number of alcohol deaths
    and number of COVID-19 cases in England and Wales before and during pandemic."
  })
  
  output$q1 <- renderText({" What is the relationship between the number of 
    crimes and the number of alcohol- specific deaths before and during the 
    coronavirus pandemic in England and Wales?"
  })
  
  output$q2 <- renderText({"How are the number of crimes and the number of 
    alcohol-specific deaths different by regions in England and Wales?"
  })
  
  output$q3 <- renderText({"Does the increase in coronavirus cases affect 
    the number of alcohol-specific deaths?"
  })
  
  output$p1_1 <- renderText({"Both 2019 and 2020 had same top 3 regions"
  })
  
  output$text1_1 <- renderText({" Top 3 regions having highest number 
    of recorded crimes: London, North West, North East."})
  
  output$text1_2 <- renderText({"Top 3 regions having lowest number of 
    recorded crimes: Wales, North East, South West."})
  
  output$overall1 <- renderText({"The positive meaning of crimes data set in two years is
    that the number of crimes decreased slightly in pandemic time in both 
    England and Wales. The reason could be because of national lock down rules
    and strict social distance."})
  
  output$text2_1 <- renderText({"Top 3 regions having highest number 
    of alcohol deaths in 2019: North West, South East, Yorkshire and The Humber"})
  
  output$text2_2 <- renderText({"Top 3 regions having highest number 
    of alcohol deaths in 2020: North West, South East, West Midlands."})
  
  output$overall2 <- renderText({"In general, the trend of specific-alcohol deaths
    data seems to increase in all regions during COVID-19 first year. This problem
    could significantly affect social well-being."})
  
  output$text3_1 <- renderText({"Top 3 highest percentage of both 
  criminal numbers and alcohol deaths in 2019: London, North West, South East."
  })
  
  output$text3_1_1 <- renderText({"London was mostly counted as the biggest 
    percentage in England."
  })
  
  output$text3_2 <- renderText({"The total top 3 highest number of these two topics in 2020
  was also as same as in 2019 but these numbers in London and North West slightly
  decreased, only South East increased by 0.1%"
  })
  
  output$text4_1 <- renderText({"Heat Map 2019 displays different crime types in 
    England and Wales. We can see that Violence against person and Theft offences
    are shown as darker blue, meaing these two crimial types mainly appeared before
    the pandemic. These two crime types frequently occured in London, North West,
    South East and Yorkshire of England. Additionally, these regions have large
    population, compared to other places."
  })
  
  output$text4_2 <- renderText({"Heat Map 2020 shows different crime types in 
    England and Wales. We can see that Violence against person and Theft offences
    are shown as darker blue and if we hover the mouse over each square, we can see that 
    there are several areas counted higher number of violence against person 
    during pandemic. This problem could be impacted by the national restrictions and
    lock down rules since people mostly stayed at home and were not allowed to 
    socialise, therefore, this could lead to some mental health issues or 
    behaviour disorders."
  })
  
  

  
}


# Run Shiny App
shinyApp(ui, server)


#References 
# https://plotly.com/r/pie-charts/ 
# https://r-graph-gallery.com/79-levelplot-with-ggplot2.html


