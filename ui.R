## Import About Me tab Text
source("aboutText.R")
## Import Calculations
source("communityTool.R")
#install.packages("DT")
library(DT)
######################################################################### WEBSITE ######################################

ui <- fluidPage(
  titlePanel("Community Nitrogen Footprint Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(style = "overflow-y:scroll; max-height: screen.height-100;",
                 width = 3,
                 fileInput("cex_data", "CEX Data:"),
                 fileInput("block_group_data_entry", "Block Group Data Entry"),
                 selectInput("region", "Region:", ## need to add a whole lot more, how to be more informative
                             c("ASCC Alaska Grid" = "AKGD",
                               "ASCC Miscellaneous" = "AKMS",
                               "WECC Southwest" = "AZNM",
                               "WECC California" = "CAMX",
                               "ERCOT ALL" = "ERCT",
                               "FRCC ALL" = "FRCC",
                               "HICC Miscellaneous" = "HIMS",
                               "HICC Oahu" = "HIOA",
                               "MRO East" = "MROE",
                               "MRO West" = "MROW",
                               "NPCC New England" = "NEWE",
                               "WECC Northwest" = "NWPP",
                               "NPCC NYC/Westchester" = "NYCW",
                               "NPCC Long Island" = "NYLI",
                               "NPCC Upstate NY" = "NYUP",
                               "RFC East" = "RFCE",
                               "RFC Michigan" = "RFCM",
                               "RFC West" = "RFCW",
                               "WECC Rockies" = "RMPA",
                               "SPP North" = "SPNO",
                               "SPP South" = "SPSO",
                               "SERC Mississippi Valley" = "SRMV",
                               "SERC Midwest" = "SRMW",
                               "SERC South" = "SRSO",
                               "SERC Tennessee Valley" = "SRTV",
                               "SERC Virginia/Carolina" = "SRVC")),
                 numericInput("total_treated_wastewater", "Total Treated Wastewater (gallons)", value = 0),
                 numericInput("wastewater_removal_factor", "Treatment Plant N Removal Factor", value = 0),
                 numericInput("electricity_by_residents", "Total Kilowatt Hours Used By Residents", value=0),
                 numericInput("electricity_by_businesses", "Total Kilowatt Hours Used By Businesses", value=0),
                 numericInput("total_therms_by_residents", "Total Therms Used By Residents (Natural Gas)", value=0),
                 numericInput("total_therms_by_businesses", "Total Therms Used By Businesses (Natural Gas)", value=0),
                 numericInput("avg_cats_per_person", "Average Cats Per  Person", 
                              value = round(as.numeric(read_excel("constants.xlsx", sheet = "pet")$Cats[1]), 3)),
                 numericInput("avg_dogs_per_person", "Average Dogs Per Person",
                              value = round(as.numeric(read_excel("constants.xlsx", sheet = "pet")$Dogs[1]), 3)),
                 h3("Miles Traveled by:"),
                 numericInput("motorcycle_miles_year", "Motorcycles:", value = 1),
                 numericInput("passenger_miles_year", "Passenger Cars:", value = 1),
                 numericInput("light_trucks_miles_year", "Light Duty Trucks:", value = 1),
                 numericInput("bus_miles_year", "Buses:", value = 1),
                 numericInput("heavy_trucks_miles_year", "Medium/Heavy Duty Trucks:", value = 1)
                 #   submitButton("Submit")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("About", 
                 fluidRow(aboutText) #aboutText defined in aboutText.R
        ),
        tabPanel("Summary", 
                 fluidRow(style="background-color:#dbd7d7;",
                          checkboxGroupInput("block_groups_f", "Block Groups:", choices = block_groups,
                                             selected = block_groups, inline = TRUE),
                          actionButton("select_all", "Select All"),
                          actionButton("clear_selected", "Clear Selected")
                 ),
                 fluidRow(style="padding:5%", column = 12, plotOutput("plot1")),
                 fluidRow(style="padding:5%", column = 12, plotOutput("plot2")),
                 fluidRow(column = 1, dataTableOutput('table1') )         
        )
        #    ,
        
        #  tabPanel("Map"), 
        #  tabPanel("Projections",
        #           h3("Use the slider bars to adjust certain categories. ",
        #                "Value of the sliders represent percentage of the ",
        #                "original category that is used."),
        #           fluidRow(
        #              column(4, 
        #              sliderInput("beef_consumption", h5("Beef Consumption"), 
        #                       min=75, max=125, step=5, value=100),
        #              checkboxInput("beef_beans_replacement", h5("Replace Beef Consumption with Bean Consumption"))),
        #              column(4, sliderInput("bean_consumption", h5("Bean Consumption"), 
        #                       min=75, max=125, step=5, value=100)),
        #              column(4, sliderInput("car_motorcycle_use", h5("Car and Motorcycle Transportation"), 
        #                       min=75, max=125, step=5, value=100))
        #              ),
        #           fluidRow(dataTableOutput('projectionsTable'))
        #           ),
        #  tabPanel("Check Input",
        #           fluidRow(radioButtons("totalOrPerCapita", label="Doesn't do anything yet", choices = c("Total", "Per Capita"),
        #                                 selected="Total")),
        #           fluidRow(HTML("Population and Number of Cats and Dogs Per Block Group")),
        #           fluidRow(dataTableOutput('table3')),
        #           fluidRow(HTML("Kilograms of Food Per Category Per Block Group")),
        #           fluidRow(dataTableOutput('table2')))
        
      )
    )
    
  )
)
