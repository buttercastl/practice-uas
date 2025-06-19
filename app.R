library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)

# Mematikan scientific notation
options(scipen = 999)

# Fungsi koneksi database
db_connect <- function() {
  dbConnect(
    Postgres(),
    dbname   = Sys.getenv("DB_NAME", "railway"),
    host     = Sys.getenv("DB_HOST", "caboose.proxy.rlwy.net"),
    port     = as.integer(Sys.getenv("DB_PORT", 57184)),
    user     = Sys.getenv("DB_USER", "postgres"),
    password = Sys.getenv("DB_PASSWORD", "fCuuNIeXaHMsCSpaNyXAYMqZltzKWFvH")
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Products Dashboard", titleWidth = 280),
  dashboardSidebar(width = 280,
                   sidebarMenu(
                     menuItem("Data", tabName = "data", icon = icon("table")),
                     menuItem("Summary", tabName = "summary", icon = icon("chart-pie")),
                     menuItem("Category", tabName = "category", icon = icon("th-list")),
                     menuItem("Product Type", tabName = "type", icon = icon("box-open")),
                     menuItem("Brand", tabName = "brand", icon = icon("tags"))
                   ),
                   hr(),
                   selectInput("catSelect", "Select category:", choices = NULL),
                   selectInput("typeSelect", "Select type:", choices = NULL),
                   selectInput("brandSelect", "Select brand:", choices = NULL)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
              fluidRow(valueBoxOutput("boxRevenue"), valueBoxOutput("boxQuantity"), valueBoxOutput("boxMarkup")),
              fluidRow(
                box(title = "Average Price per Category", width = 12, plotlyOutput("plotAvgPriceCat", height = 300)),
                box(title = "Quantity Sold per Category", width = 12, plotlyOutput("plotCategoryquantity", height = 300))
              )
      ),
      tabItem(tabName = "category",
              fluidRow(
                box(title = "Product per Category", width = 12, plotlyOutput("plotProductsPerCat", height = 400))
              )
      ),
      tabItem(tabName = "type",
              fluidRow(
                box(title = "Revenue by Product Type", width = 12, plotlyOutput("plotTypeDetail", height = 400))
              )
      ),
      tabItem(tabName = "brand",
              fluidRow(
                box(title = "Top 10 Brands by Units Sold", width = 6, plotlyOutput("plotTopBrandsquantity", height = 350)),
                box(title = "Unique Product per Brand", width = 6, plotlyOutput("plotUniqueProdBrand", height = 350))
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Dataset", width = 12, DTOutput("tableData"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Data from DB
  data_all <- reactive({
    con <- db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    dbGetQuery(con, "SELECT * FROM products")
  })
  
  # Update filter choices based on DB data
  observe({
    df <- data_all()
    updateSelectInput(session, "catSelect", choices = c("All", unique(df$product_category)), selected = "All")
    updateSelectInput(session, "typeSelect", choices = c("All", unique(df$product_type)), selected = "All")
    updateSelectInput(session, "brandSelect", choices = c("All", unique(df$product_brand)), selected = "All")
  })
  
  # Reactive filtered data
  filtered <- reactive({
    df <- data_all()
    if (input$catSelect != "All") df <- filter(df, product_category == input$catSelect)
    if (input$typeSelect != "All") df <- filter(df, product_type == input$typeSelect)
    if (input$brandSelect != "All") df <- filter(df, product_brand == input$brandSelect)
    df
  })
  
  # Value Boxes
  output$boxRevenue <- renderValueBox({
    rev <- sum(filtered()$revenue, na.rm = TRUE)
    valueBox(scales::dollar(rev), "Total Revenue", icon = icon("money-bill"), color = "green")
  })
  output$boxQuantity <- renderValueBox({
    quantity <- sum(filtered()$quantity, na.rm = TRUE)
    valueBox(quantity, "Total Quantity", icon = icon("shopping-cart"), color = "blue")
  })
  output$boxMarkup <- renderValueBox({
    mpu <- mean(filtered()$markup, na.rm = TRUE)
    valueBox(round(mpu, 2), "Average Markup", icon = icon("line-chart"), color = "yellow")
  })
  
  # Base theme tanpa axis titles
  base_theme <- theme_minimal() + theme(axis.title = element_blank())
  
  # Summary Plots
  output$plotAvgPriceCat <- renderPlotly({
    dat <- filtered() %>% group_by(product_category) %>%
      summarize(average_price = sum(revenue)/sum(quantity))
    p <- ggplot(dat, aes(reorder(product_category, average_price), average_price,
                         text = paste0(product_category, ': $', round(average_price, 2)))) +
      geom_col(aes(fill = average_price)) + scale_fill_viridis() + coord_flip() + base_theme
    ggplotly(p, tooltip = "text")
  })
  output$plotCategoryquantity <- renderPlotly({
    dat <- filtered() %>% group_by(product_category) %>% summarize(quantity = sum(quantity))
    p <- ggplot(dat, aes(reorder(product_category, quantity), quantity,
                         text = paste0(product_category, ': ', quantity, ' units'))) +
      geom_col(aes(fill = quantity)) + scale_fill_viridis() + coord_flip() + base_theme
    ggplotly(p, tooltip = "text")
  })
  
  # Category Detail
  output$plotProductsPerCat <- renderPlotly({
    dat <- filtered() %>% group_by(product_category) %>% summarize(count = n_distinct(product_id))
    p <- ggplot(dat, aes(reorder(product_category, count), count,
                         text = paste0(product_category, ': ', count, ' products'))) +
      geom_col(aes(fill = count)) + scale_fill_viridis() + coord_flip() + base_theme
    ggplotly(p, tooltip = "text")
  })
  
  # Type Detail
  output$plotTypeDetail <- renderPlotly({
    dat <- filtered() %>% group_by(product_type) %>% summarize(revenue = sum(revenue))
    p <- ggplot(dat, aes(product_type, revenue,
                         text = paste0(product_type, ': $', scales::comma(revenue)))) +
      geom_col(aes(fill = revenue)) + scale_fill_viridis() + coord_flip() + base_theme
    ggplotly(p, tooltip = "text")
  })
  
  # Brand Tab: Units & Unique Products
  output$plotTopBrandsquantity <- renderPlotly({
    dat <- filtered() %>% group_by(product_brand) %>% summarize(quantity = sum(quantity)) %>% slice_max(quantity, n = 10)
    p <- ggplot(dat, aes(reorder(product_brand, quantity), quantity,
                         text = paste0(product_brand, ': ', quantity, ' units'))) +
      geom_col(aes(fill = quantity)) + scale_fill_viridis() + coord_flip() + base_theme
    ggplotly(p, tooltip = "text")
  })
  output$plotUniqueProdBrand <- renderPlotly({
    dat <- filtered() %>% group_by(product_brand) %>% summarize(unique_count = n_distinct(product_id)) %>% slice_max(unique_count, n = 10)
    p <- ggplot(dat, aes(reorder(product_brand, unique_count), unique_count,
                         text = paste0(product_brand, ': ', unique_count, ' products'))) +
      geom_col(aes(fill = unique_count)) + scale_fill_viridis() + coord_flip() + base_theme
    ggplotly(p, tooltip = "text")
  })
  
  # Data table
  output$tableData <- renderDT({
    datatable(filtered(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Jalankan aplikasi
shinyApp(ui, server)
