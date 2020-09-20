
## Import About Me tab Text
source("aboutText.R")
## Import Calculations
source("communityTool.R")

######################################################################### WEBSITE ######################################

ui <- fluidPage(
  titlePanel("Community Nitrogen Footprint Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(style = "overflow-y:scroll; max-height: 600px;",
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
                 h3("Miles Traveled by:"),
                 numericInput("motorcycle_miles_year", "Motorcycles:", value = 1),
                 numericInput("passenger_miles_year", "Passenger Cars:", value = 1),
                 numericInput("light_trucks_miles_year", "Light Duty Trucks:", value = 1),
                 numericInput("bus_miles_year", "Buses:", value = 1),
                 numericInput("heavy_trucks_miles_year", "Medium/Heavy Duty Trucks:", value = 1)
              #   submitButton("Submit")
    ),
    
    mainPanel(
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
        ),
        
        tabPanel("Map"), 
        tabPanel("Reduction Strategies")
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
  
  cex_data <- eventReactive(input$cex_data, {
    read.csv(input$cex_data$datapath)
  })
  general_data <- eventReactive(input$block_group_data_entry, {
    read.csv(input$block_group_data_entry$datapath)
  })
  observeEvent(input$clear_selected, {
    print("clear")
    print(block_groups)
    updateCheckboxGroupInput(session,"block_groups_f","Block Groups:",choices = cex_data()$ID,inline=TRUE, selected=NULL)
  })
  observeEvent(input$select_all,{
    print("select")
    updateCheckboxGroupInput(session,"block_groups_f","Block Groups:",choices = cex_data()$ID,inline=TRUE, selected=cex_data()$ID)
  })
  observeEvent(input$cex_data, {
    block_groups <- cex_data()$ID
    updateCheckboxGroupInput(session, "block_groups_f", choices = block_groups, selected = block_groups, inline = TRUE)
  })
  
  
  output$table1 <- renderDataTable({
    updateAll(cex_data(), general_data(), passenger_cars_miles_year(), motorcycles_miles_year(), light_trucks_miles_year(), bus_miles_year(), heavy_trucks_miles_year(), wastewater_treatment_factor_input(), total_treated_waterwater_input(), therms_by_business_input(), therms_by_residents_input(), electricity_by_businesses(), electricity_by_residents(), region())},
                                   options = list(dom  = '<"top">lrt<"bottom">ip',
                                                  columns = list (
                                                    list(title = "Block Group"),
                                                    list(title = "Food"),
                                                    list(title = "Pets"),
                                                    list(title = "Wastewater"),
                                                    list(title = "Transportation"),
                                                    list(title = "Electricity"),
                                                    list(title = "Natural Gas"),
                                                    list(title = "Total")
                                                  )))
  
  
  output$plot1 <- renderPlot(summary_graphs_filtered(input$block_groups_f, cex_data(), general_data(), passenger_cars_miles_year(), motorcycles_miles_year(), light_trucks_miles_year(), bus_miles_year(), heavy_trucks_miles_year(), wastewater_treatment_factor_input(), total_treated_waterwater_input(), therms_by_business_input(), therms_by_residents_input(), electricity_by_businesses(), electricity_by_residents(), region()))
  output$plot2 <- renderPlot(stacked_graphs_filtered(input$block_groups_f, cex_data(), general_data(), passenger_cars_miles_year(), motorcycles_miles_year(), light_trucks_miles_year(), bus_miles_year(), heavy_trucks_miles_year(), wastewater_treatment_factor_input(), total_treated_waterwater_input(), therms_by_business_input(), therms_by_residents_input(), electricity_by_businesses(), electricity_by_residents(), region()))
  
  # Download excel template for Block Group data inputs
  output$downloadBlockGroupInput <- downloadHandler(
    filename = function() { "Data_Entry.xlsx"},
    content = function(fileName) {file.copy("Data_Entry.xlsx", fileName)}
  )
}

## Update calculations functions
updateAll <- function(cex_data, general_data, passenger_cars_miles_year, motorcycle_miles_year, light_trucks_miles_year, bus_miles_year, heavy_trucks_miles_year, wastewater_removal_factor, total_treated_wastewater, therms_by_business, therms_by_residents, electricity_by_businesses, electricity_by_residents, region){
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
  pet_data <- pet_calculations(general_data)
  pet_food_n <- pet_data[,1]
  pet_waste_n <- pet_data[,2]
  wastewater_n <- wastewater_calculations(wastewater_removal_factor, total_treated_wastewater, population_data)
  transportation_n <- transportation_calculations(cex_data, motorcycle_miles_year, passenger_cars_miles_year, light_trucks_miles_year, bus_miles_year, heavy_trucks_miles_year)
  electricity_n <- electricity_calculations(cex_data, general_data, electricity_by_residents, electricity_by_businesses, region)
  nat_gas_n <- nat_gas_calculations(cex_data, general_data, therms_by_residents, therms_by_business)
  
  combined_by_block_group_n <- total_food_production_n  + pet_food_n + pet_waste_n + wastewater_n +
    transportation_n + electricity_n + nat_gas_n
  all_n <- sum(total_food_production_n) + sum(pet_food_n) + sum(pet_waste_n) + sum(wastewater_n) +
    sum(transportation_n) + sum(electricity_n) + sum(nat_gas_n)
  combined_by_category_n_table <- data.frame("Block Group" <- cex_data$ID,
                                             "Food" <- total_food_production_n,
                                             "Pets" <- pet_food_n + pet_waste_n,
                                             "Wastewater" <- wastewater_n,
                                             "Transportation" <- transportation_n,
                                             "Electricity" <- electricity_n,
                                             "Natural Gas" <- nat_gas_n,
                                             "Total" <- combined_by_block_group_n)
  combined_by_category_n_table["Total" ,] <- colSums(combined_by_category_n_table)
  combined_by_category_n_table <- round(combined_by_category_n_table, 3)
  return(combined_by_category_n_table)
}


summary_graphs_filtered <- function(blockgroups, cex_data, general_data, passenger_cars_miles_year, motorcycle_miles_year, light_trucks_miles_year, bus_miles_year, heavy_trucks_miles_year, wastewater_removal_factor, total_treated_wastewater, therms_by_business, therms_by_residents, electricity_by_businesses, electricity_by_residents, region){
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
  pet_data <- pet_calculations(general_data)
  pet_food_n <- pet_data[,1]
  pet_waste_n <- pet_data[,2]
  wastewater_n <- wastewater_calculations(wastewater_removal_factor, total_treated_wastewater, population_data)
  transportation_n <- transportation_calculations(cex_data, motorcycle_miles_year, passenger_cars_miles_year, light_trucks_miles_year, bus_miles_year, heavy_trucks_miles_year)
  electricity_n <- electricity_calculations(cex_data, general_data, electricity_by_residents, electricity_by_businesses, region)
  nat_gas_n <- nat_gas_calculations(cex_data, general_data, therms_by_residents, therms_by_business)
  
  combined_by_block_group_n <- total_food_production_n  + pet_food_n + pet_waste_n + wastewater_n +
    transportation_n + electricity_n + nat_gas_n
  all_n <- sum(total_food_production_n) + sum(pet_food_n) + sum(pet_waste_n) + sum(wastewater_n) +
    sum(transportation_n) + sum(electricity_n) + sum(nat_gas_n)
  
  print(total_food_production_n)
  print(pet_food_n)
  print(pet_waste_n)
  print(wastewater_n)
  print(transportation_n)
  print(electricity_n)
  print(nat_gas_n)
  a_filtered <- ggplot(combined_by_category_filtered(blockgroups, cex_data$ID, total_food_production_n, pet_food_n, pet_waste_n, wastewater_n, transportation_n, electricity_n, nat_gas_n), aes(x="", y=value, fill=group)) + 
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

stacked_graphs_filtered<- function(blockgroups, cex_data, general_data, passenger_cars_miles_year, motorcycle_miles_year, light_trucks_miles_year, bus_miles_year, heavy_trucks_miles_year, wastewater_removal_factor, total_treated_wastewater, therms_by_business, therms_by_residents, electricity_by_businesses, electricity_by_residents, region){
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
  pet_data <- pet_calculations(general_data)
  pet_food_n <- pet_data[,1]
  pet_waste_n <- pet_data[,2]
  wastewater_n <- wastewater_calculations(wastewater_removal_factor, total_treated_wastewater, population_data)
  transportation_n <- transportation_calculations(cex_data, motorcycle_miles_year, passenger_cars_miles_year, light_trucks_miles_year, bus_miles_year, heavy_trucks_miles_year)
  electricity_n <- electricity_calculations(cex_data, general_data, electricity_by_residents, electricity_by_businesses, region)
  nat_gas_n <- nat_gas_calculations(cex_data, general_data, therms_by_residents, therms_by_business)
  
  combined_by_block_group_n <- total_food_production_n  + pet_food_n + pet_waste_n + wastewater_n +
    transportation_n + electricity_n + nat_gas_n
  all_n <- sum(total_food_production_n) + sum(pet_food_n) + sum(pet_waste_n) + sum(wastewater_n) +
    sum(transportation_n) + sum(electricity_n) + sum(nat_gas_n)
  food_by_sources2 <- food_by_sources_filtered(blockgroups, cex_data$ID, beef_production_n, pork_production_n, chicken_production_n, cheese_production_n, eggs_production_n, milk_production_n, fish_production_n, liquids_production_n, grains_production_n, nuts_production_n, fruits_production_n, oils_production_n, beans_production_n, spices_production_n, potatoes_production_n, coffee_tea_production_n, sugar_production_n, vegetables_production_n)
  food_by_sources2$x <- 1
  c_filtered <- ggplot(food_by_sources2, aes(x=x, y=value, fill=group))+geom_col()+
    scale_fill_viridis(discrete = TRUE, option="C")
  combined_by_category2 <- combined_by_category_filtered(blockgroups, cex_data$ID, total_food_production_n, pet_food_n, pet_waste_n, wastewater_n, transportation_n, electricity_n, nat_gas_n)
  combined_by_category2$x <- 1
  d_filtered <- ggplot(combined_by_category2, aes(x=x, y=value, fill=group))+geom_col()+
    scale_fill_viridis(discrete = TRUE, option="C")
  return({grid.arrange(d_filtered, c_filtered, ncol=2)})
}

shinyApp(ui=ui, server=server)


# if want to run locally but access elsewhere
#app <- shinyApp(ui=ui, server=server)
#runApp(app,host="0.0.0.0",port=5553)

