## Import About Me tab Text
source("aboutText.R")
## Import Calculations
source("communityTool.R")
## Import Page UI
source("ui.R")
## For functions
source("communityTool_website.R")
#install.packages("DT")
library(DT)

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
