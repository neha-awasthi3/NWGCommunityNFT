
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


server <- function(input, output, session){
  passenger_cars_miles_year <- reactive({input$passenger_miles_year})
  motorcycles_miles_year <- reactive({input$motorcycle_miles_year})
  light_trucks_miles_year <- reactive({input$light_trucks_miles_year})
  bus_miles_year <- reactive({input$bus_miles_year})
  heavy_trucks_miles_year <- reactive({input$heavy_trucks_miles_year})
  total_treated_waterwater_input <- reactive({input$total_treated_wastewater})
  wastewater_treatment_factor_input <- reactive({input$wastewater_removal_factor})
  therms_by_business_input <- reactive({input$total_therms_by_businesses})
  therms_by_residents_input <- reactive({input$total_therms_by_residents})
  electricity_by_businesses <- reactive({input$electricity_by_businesses})
  electricity_by_residents <- reactive({input$electricity_by_residents})
  region <- reactive({input$region})
  
  avg_cats_per_person <- reactive({input$avg_cats_per_person})
  avg_dogs_per_person <- reactive({input$avg_dogs_per_person})
  
  # For projections
  beef_consumption_input <- reactive({input$beef_consumption})
  beef_beans_replacement_input <- reactive({input$beef_beans_replacement})
  car_motorcycle_use_input <- reactive(input$car_motorcycle_use)
  
  cex_data <- eventReactive(input$cex_data, {
    read.csv(input$cex_data$datapath)
  })
  general_data <- eventReactive(input$block_group_data_entry, {
    read.csv(input$block_group_data_entry$datapath)
  })
  observeEvent(input$clear_selected, {
    updateCheckboxGroupInput(session,"block_groups_f","Block Groups:",choices = cex_data()$ID,inline=TRUE, selected=NULL)
  })
  observeEvent(input$select_all,{
    updateCheckboxGroupInput(session,"block_groups_f","Block Groups:",choices = cex_data()$ID,inline=TRUE, selected=cex_data()$ID)
  })
  observeEvent(input$cex_data, {
    block_groups <- cex_data()$ID
    updateCheckboxGroupInput(session, "block_groups_f", choices = block_groups, selected = block_groups, inline = TRUE)
  })
  
  observeEvent(input$totalOrPerCapita, {
    
  })
  output$table1 <- renderDataTable({
    updateAll(cex_data(), general_data(), passenger_cars_miles_year(), motorcycles_miles_year(), light_trucks_miles_year(), bus_miles_year(), 
              heavy_trucks_miles_year(), wastewater_treatment_factor_input(), total_treated_waterwater_input(), therms_by_business_input(), 
              therms_by_residents_input(), electricity_by_businesses(), electricity_by_residents(), region(), avg_cats_per_person(), 
              avg_dogs_per_person())},
                                   options = list(dom  = '<"top">lrt<"bottom">ip',
                                                  columns = list (
                                                    list(title = "Block Group"),
                                                    list(title = "Food"),
                                                    list(title = "Pets"),
                                                    list(title = "Wastewater"),
                                                    list(title = "Transportation"),
                                                    list(title = "Electricity"),
                                                    list(title = "Natural Gas"),
                                                    list(title = "Fertilizer"),
                                                    list(title = "Total")
                                                  )))

  output$table2 <- renderDataTable({
    input_check(cex_data(), general_data())
  },
  options = list(dom  = '<"top">lrt<"bottom">ip',
                 columns = list (
                   list(title = "Block Group"),
                   list(title = "Beef"),
                   list(title = "Pork"),
                   list(title = "Chicken"),
                   list(title = "Cheese"),
                   list(title = "Eggs"),
                   list(title = "Milk"),
                   list(title = "Fish"),
                   list(title = "Liquids"),
                   list(title = "Grains"),
                   list(title = "Nuts"),
                   list(title = "Fruits"),
                   list(title = "Oils"),
                   list(title = "Beans"),
                   list(title = "Spices"),
                   list(title = "Potatoes"),
                   list(title = "Coffee and Tea"),
                   list(title = "Sugar"),
                   list(title = "Vegetables"),
                   list(title = "Food At Home"),
                   list(title = "Food Away From Home"),
                   list(title = "SNAP Food"),
                   list(title = "Total")
                 )))
  
  output$table3 <- renderDataTable({
    pet_calculations(general_data(), avg_cats_per_person(), avg_dogs_per_person(), TRUE)
  }, options = list(dom  = '<"top">lrt<"bottom">ip',
  columns = list (
    list(title = "Block Group"),
    list(title = "Population"),
    list(title = "Number of Cats"),
    list(title = "Number of Dogs")
  )))
    
  
  output$projectionsTable <- renderDataTable({
    updateAll(cex_data(), general_data(), passenger_cars_miles_year(), motorcycles_miles_year(), light_trucks_miles_year(), bus_miles_year(), 
              heavy_trucks_miles_year(), wastewater_treatment_factor_input(), total_treated_waterwater_input(), therms_by_business_input(), 
              therms_by_residents_input(), electricity_by_businesses(), electricity_by_residents(), region(), avg_cats_per_person(), 
              avg_dogs_per_person(), TRUE, beef_consumption_input(), beef_beans_replacement_input())},
    options = list(dom  = '<"top">lrt<"bottom">ip',
                   columns = list (
                     list(title = ""),
                     list(title = "Food"),
                     list(title = "Pets"),
                     list(title = "Wastewater"),
                     list(title = "Transportation"),
                     list(title = "Electricity"),
                     list(title = "Natural Gas"),
                     list(title = "Fertilizer"),
                     list(title = "Total")
                   ),
                   include.rownames = FALSE))
  
  
  
  
  output$plot1 <- renderPlot(summary_graphs_filtered(input$block_groups_f, cex_data(), general_data(), passenger_cars_miles_year(), motorcycles_miles_year(), 
                                                     light_trucks_miles_year(), bus_miles_year(), heavy_trucks_miles_year(), 
                                                     wastewater_treatment_factor_input(), total_treated_waterwater_input(), therms_by_business_input(), 
                                                     therms_by_residents_input(), electricity_by_businesses(), electricity_by_residents(), region(),
                                                     avg_cats_per_person(), avg_dogs_per_person()))
  output$plot2 <- renderPlot(stacked_graphs_filtered(input$block_groups_f, cex_data(), general_data(), passenger_cars_miles_year(), motorcycles_miles_year(), 
                                                     light_trucks_miles_year(), bus_miles_year(), heavy_trucks_miles_year(), wastewater_treatment_factor_input(), 
                                                     total_treated_waterwater_input(), therms_by_business_input(), therms_by_residents_input(), 
                                                     electricity_by_businesses(), electricity_by_residents(), region(), avg_cats_per_person(), avg_dogs_per_person()))
  
  # Download excel template for Block Group data inputs
  output$downloadBlockGroupInput <- downloadHandler(
    filename = function() { "Data_Entry.xlsx"},
    content = function(fileName) {file.copy("Data_Entry.xlsx", fileName)}
  )
}

############ Update calculations functions -- FOR TABLES ####################################################################
updateAll <- function(cex_data, general_data, passenger_cars_miles_year, motorcycle_miles_year, light_trucks_miles_year, bus_miles_year, 
                      heavy_trucks_miles_year, wastewater_removal_factor, total_treated_wastewater, therms_by_business, therms_by_residents, 
                      electricity_by_businesses, electricity_by_residents, region, avg_cats_per_person, avg_dogs_per_person,
                      isProjection=FALSE, beef_consumption_change=100, beans_replace_beef=FALSE){
  population_data <- general_data$Total.Population.of.BG
  total_food_production_totals <- food_calculations(cex_data, general_data, FALSE, beef_consumption_change, beans_replace_beef)
  
  beef_production_n <- total_food_production_totals[,1]
  pork_production_n <- total_food_production_totals[,2]
  chicken_production_n <- total_food_production_totals[,3]
  cheese_production_n <- total_food_production_totals[,4]
  eggs_production_n <- total_food_production_totals[,5]
  milk_production_n <- total_food_production_totals[,6]
  fish_production_n <- total_food_production_totals[,7]
  liquids_production_n <- total_food_production_totals[,8]
  grains_production_n <- total_food_production_totals[,9]
  nuts_production_n <- total_food_production_totals[,10]
  fruits_production_n <- total_food_production_totals[,11]
  oils_production_n <- total_food_production_totals[,12]
  beans_production_n <- total_food_production_totals[,13]
  spices_production_n <- total_food_production_totals[,14]
  potatoes_production_n<- total_food_production_totals[,15]
  coffee_tea_production_n <- total_food_production_totals[,16]
  sugar_production_n <- total_food_production_totals[,17]
  vegetables_production_n <- total_food_production_totals[,18]
  total_food_production_n <- rowSums(total_food_production_totals)
  
  pet_data <- pet_calculations(general_data, avg_cats_per_person, avg_dogs_per_person)
  pet_food_n <- pet_data[,1]
  pet_waste_n <- pet_data[,2]
  wastewater_n <- wastewater_calculations(wastewater_removal_factor, total_treated_wastewater, population_data)
  transportation_n <- transportation_calculations(cex_data, motorcycle_miles_year, passenger_cars_miles_year, light_trucks_miles_year, bus_miles_year, heavy_trucks_miles_year)
  electricity_n <- electricity_calculations(cex_data, general_data, electricity_by_residents, electricity_by_businesses, region)
  nat_gas_n <- nat_gas_calculations(cex_data, general_data, therms_by_residents, therms_by_business)
  fert_n <- nat_gas_calculations(cex_data, general_data, therms_by_residents, therms_by_business)
  
  combined_by_block_group_n <- total_food_production_n  + pet_food_n + pet_waste_n + wastewater_n +
    transportation_n + electricity_n + nat_gas_n + fert_n
  all_n <- sum(total_food_production_n, na.rm=T) + sum(pet_food_n, na.rm=T) + sum(pet_waste_n, na.rm=T) + sum(wastewater_n, na.rm=T) +
    sum(transportation_n, na.rm=T) + sum(electricity_n, na.rm=T) + sum(nat_gas_n, na.rm=T) + sum(fert_n, na.rm=T)
  combined_by_category_n_table<-cbind(cex_data$ID, total_food_production_n, pet_food_n + pet_waste_n,
                                      wastewater_n, transportation_n, electricity_n, nat_gas_n, fert_n, combined_by_block_group_n)
  # This started giving an index column and pushing over the
  # data by one while keeping the column name, so changed to cbind
  #combined_by_category_n_table <- data.frame("Block Group" <- cex_data$ID,
  #                                            "Food" <- total_food_production_n,
  #                                           "Pets" <- pet_food_n + pet_waste_n,
  #                                           "Wastewater" <- wastewater_n,
  #                                           "Transportation" <- transportation_n,
  #                                           "Electricity" <- electricity_n,
  #                                           "Natural Gas" <- nat_gas_n,
  #                                           "Total" <- combined_by_block_group_n)
  #combined_by_category_n_table["Total" ,] <- colSums(combined_by_category_n_table)
  #combined_by_block_group_n[,"Total"] <- rowSums(combined_by_category_n_table)
  if(isProjection){
    sums <- colSums(combined_by_category_n_table)
    combined_by_category_n_table <- data.frame("Food" <- sums[2],
                                               "Pets" <- sums[3],
                                               "Wastewater" <- sums[4],
                                               "Transportation" <- sums[5],
                                               "Electricity" <- sums[6],
                                               "Natural Gas" <- sums[7],
                                               "Fertilizer" <- sums[7],
                                               "Total" <- sums[8])
    # had been trying to add weird former column names as row names 
    # this wa pushing the data over so it didn't line up with the column names
    # this fixed that
    row.names(combined_by_category_n_table) <- c("")
  }
  combined_by_category_n_table <- round(combined_by_category_n_table, 3)
  return(combined_by_category_n_table)
}

################## GRAPHS #############################################################################################################################
summary_graphs_filtered <- function(blockgroups, cex_data, general_data, passenger_cars_miles_year, motorcycle_miles_year, light_trucks_miles_year, 
                                    bus_miles_year, heavy_trucks_miles_year, wastewater_removal_factor, total_treated_wastewater, therms_by_business, 
                                    therms_by_residents, electricity_by_businesses, electricity_by_residents, region, avg_cats_per_person, avg_dogs_per_person){
  population_data <- general_data$Total.Population.of.BG
  total_food_production_totals <- food_calculations(cex_data, general_data)
  beef_production_n <- total_food_production_totals[,1]
  pork_production_n <- total_food_production_totals[,2]
  chicken_production_n <- total_food_production_totals[,3]
  cheese_production_n <- total_food_production_totals[,4]
  eggs_production_n <- total_food_production_totals[,5]
  milk_production_n <- total_food_production_totals[,6]
  fish_production_n <- total_food_production_totals[,7]
  liquids_production_n <- total_food_production_totals[,8]
  grains_production_n <- total_food_production_totals[,9]
  nuts_production_n <- total_food_production_totals[,10]
  fruits_production_n <- total_food_production_totals[,11]
  oils_production_n <- total_food_production_totals[,12]
  beans_production_n <- total_food_production_totals[,13]
  spices_production_n <- total_food_production_totals[,14]
  potatoes_production_n<- total_food_production_totals[,15]
  coffee_tea_production_n <- total_food_production_totals[,16]
  sugar_production_n <- total_food_production_totals[,17]
  vegetables_production_n <- total_food_production_totals[,18]
  total_food_production_n <- rowSums(total_food_production_totals)
  pet_data <- pet_calculations(general_data, avg_cats_per_person, avg_dogs_per_person)
  pet_food_n <- pet_data[,1]
  pet_waste_n <- pet_data[,2]
  wastewater_n <- wastewater_calculations(wastewater_removal_factor, total_treated_wastewater, population_data)
  transportation_n <- transportation_calculations(cex_data, motorcycle_miles_year, passenger_cars_miles_year, light_trucks_miles_year, bus_miles_year, heavy_trucks_miles_year)
  electricity_n <- electricity_calculations(cex_data, general_data, electricity_by_residents, electricity_by_businesses, region)
  nat_gas_n <- nat_gas_calculations(cex_data, general_data, therms_by_residents, therms_by_business)
  fert_n <- nat_gas_calculations(cex_data, general_data, therms_by_residents, therms_by_business)
  
  combined_by_block_group_n <- total_food_production_n  + pet_food_n + pet_waste_n + wastewater_n +
    transportation_n + electricity_n + nat_gas_n + fert_n
  all_n <- sum(total_food_production_n, na.rm=T) + sum(pet_food_n, na.rm=T) + sum(pet_waste_n, na.rm=T) + sum(wastewater_n, na.rm=T) +
    sum(transportation_n, na.rm=T) + sum(electricity_n, na.rm=T) + sum(nat_gas_n, na.rm=T) + sum(fert_n, na.rm=T)
  a_filtered <- ggplot(combined_by_category_filtered(blockgroups, cex_data$ID, total_food_production_n, pet_food_n, pet_waste_n, wastewater_n, transportation_n, electricity_n, nat_gas_n,fert_n), aes(x="", y=value, fill=group)) + 
    geom_bar(stat="identity", width = 1, color="white") + coord_polar("y", start = 0) + theme_void() +
    scale_fill_viridis(discrete = TRUE, option="C") +
    ggtitle("Nitrogen by General Category") +
    geom_label_repel(aes(label = round(value*100, 2)), position = position_stack(vjust = .9)) 
  
  b_filtered <- ggplot(food_by_sources_filtered(blockgroups, cex_data$ID, beef_production_n, pork_production_n, chicken_production_n, cheese_production_n, eggs_production_n, milk_production_n, fish_production_n, liquids_production_n, grains_production_n, nuts_production_n, fruits_production_n, oils_production_n, beans_production_n, spices_production_n, potatoes_production_n, coffee_tea_production_n, sugar_production_n, vegetables_production_n), 
                       aes(x=group, y = value, fill=group, label=round(value, 0))) +
    geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_fill_viridis(discrete = TRUE, option="C") + xlab("Food Category") + ylab("Nitrogen (kg)") +
    ggtitle("Nitrogen by Food Category") 
  #geom_label_repel(size = 4, position = position_stack(vjust = 1))
  return({grid.arrange(a_filtered, b_filtered, ncol=2)})
}

stacked_graphs_filtered<- function(blockgroups, cex_data, general_data, passenger_cars_miles_year, motorcycle_miles_year, light_trucks_miles_year, 
                                   bus_miles_year, heavy_trucks_miles_year, wastewater_removal_factor, total_treated_wastewater, therms_by_business, 
                                   therms_by_residents, electricity_by_businesses, electricity_by_residents, region, avg_cats_per_person, avg_dogs_per_person){
  population_data <- general_data$Total.Population.of.BG
  total_food_production_totals <- food_calculations(cex_data, general_data)
  beef_production_n <- total_food_production_totals[,1]
  pork_production_n <- total_food_production_totals[,2]
  chicken_production_n <- total_food_production_totals[,3]
  cheese_production_n <- total_food_production_totals[,4]
  eggs_production_n <- total_food_production_totals[,5]
  milk_production_n <- total_food_production_totals[,6]
  fish_production_n <- total_food_production_totals[,7]
  liquids_production_n <- total_food_production_totals[,8]
  grains_production_n <- total_food_production_totals[,9]
  nuts_production_n <- total_food_production_totals[,10]
  fruits_production_n <- total_food_production_totals[,11]
  oils_production_n <- total_food_production_totals[,12]
  beans_production_n <- total_food_production_totals[,13]
  spices_production_n <- total_food_production_totals[,14]
  potatoes_production_n<- total_food_production_totals[,15]
  coffee_tea_production_n <- total_food_production_totals[,16]
  sugar_production_n <- total_food_production_totals[,17]
  vegetables_production_n <- total_food_production_totals[,18]
  total_food_production_n <- rowSums(total_food_production_totals)
  pet_data <- pet_calculations(general_data, avg_cats_per_person, avg_dogs_per_person)
  pet_food_n <- pet_data[,1]
  pet_waste_n <- pet_data[,2]
  wastewater_n <- wastewater_calculations(wastewater_removal_factor, total_treated_wastewater, population_data)
  transportation_n <- transportation_calculations(cex_data, motorcycle_miles_year, passenger_cars_miles_year, light_trucks_miles_year, bus_miles_year, heavy_trucks_miles_year)
  electricity_n <- electricity_calculations(cex_data, general_data, electricity_by_residents, electricity_by_businesses, region)
  nat_gas_n <- nat_gas_calculations(cex_data, general_data, therms_by_residents, therms_by_business)
  fert_n <- nat_gas_calculations(cex_data, general_data, therms_by_residents, therms_by_business)
  
  combined_by_block_group_n <- total_food_production_n  + pet_food_n + pet_waste_n + wastewater_n +
    transportation_n + electricity_n + nat_gas_n + fert_n
  all_n <- sum(total_food_production_n, na.rm=T) + sum(pet_food_n, na.rm=T) + sum(pet_waste_n, na.rm=T) + sum(wastewater_n, na.rm=T) +
    sum(transportation_n, na.rm=T) + sum(electricity_n, na.rm=T) + sum(nat_gas_n, na.rm=T) + sum(fert_n, na.rm=T)
  food_by_sources2 <- food_by_sources_filtered(blockgroups, cex_data$ID, beef_production_n, pork_production_n, chicken_production_n, cheese_production_n, eggs_production_n, milk_production_n, fish_production_n, liquids_production_n, grains_production_n, nuts_production_n, fruits_production_n, oils_production_n, beans_production_n, spices_production_n, potatoes_production_n, coffee_tea_production_n, sugar_production_n, vegetables_production_n)
  food_by_sources2$x <- 1
  c_filtered <- ggplot(food_by_sources2, aes(x=x, y=value, fill=group))+geom_col()+
    scale_fill_viridis(discrete = TRUE, option="C")
  combined_by_category2 <- combined_by_category_filtered(blockgroups, cex_data$ID, total_food_production_n, pet_food_n, pet_waste_n, wastewater_n, transportation_n, electricity_n, nat_gas_n, fert_n)
  combined_by_category2$x <- 1
  d_filtered <- ggplot(combined_by_category2, aes(x=x, y=value, fill=group))+geom_col()+
    scale_fill_viridis(discrete = TRUE, option="C")
  return({grid.arrange(d_filtered, c_filtered, ncol=2)})
}


input_check <- function(cex_data, general_data){
  total_food_production_totals <- food_calculations(cex_data, general_data, TRUE)
  total_food_production_totals["Total" ,] <- colSums(total_food_production_totals)
  total_food_production_totals[,"Total"] <- rowSums(total_food_production_totals)
  total_food_production_totals <- round(total_food_production_totals, 3)
 
  return(total_food_production_totals) 
 # return (total_food_production_totals %>%
 #           formatStyle(columns = colnames(input_check(cex_data(), general_data()))[2:ncol(input_check(cex_data(), general_data()))], 
#                        backgroundColor  = styleInterval(c(0, 250), c('red', 'white', 'red'))))
}

shinyApp(ui=ui, server=server)


# if want to run locally but access elsewhere
#app <- shinyApp(ui=ui, server=server)
#runApp(app,host="0.0.0.0",port=5553)

