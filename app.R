library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(DT)
library(ggplot2)
library(dplyr)

# Fungsi koneksi database
koneksi_db <- function() {
  dbConnect(
    Postgres(),
    dbname = Sys.getenv("DB_NAME", "railway"),
    host = Sys.getenv("DB_HOST", "tramway.proxy.rlwy.net"),  
    port = as.integer(Sys.getenv("DB_PORT", "53751")),
    user = Sys.getenv("DB_USER", "postgres"),
    password = Sys.getenv("DB_PASSWORD", "XPlLZtNLtiDCbPDvhxiKqnHOWOfHdZwT")
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Produk"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tabel", tabName = "tabel", icon = icon("table")),
      menuItem("Visualisasi", tabName = "visual", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tabel",
        fluidRow(
          box(width = 12, title = "Data Produk", DTOutput("data_produk"))
        )
      ),
      tabItem(
        tabName = "visual",
        fluidRow(
          box(width = 6, title = "Jumlah Produk per Kategori", plotOutput("bar_kategori")),
          box(width = 6, title = "Distribusi Harga Produk", plotOutput("histogram_harga"))
        ),
        fluidRow(
          box(width = 6, title = "Harga Rata-rata per Kategori", plotOutput("avg_harga_kategori")),
          box(width = 6, title = "Top 10 Produk Termahal", plotOutput("top_produk"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  data_produk <- reactive({
    con <- koneksi_db()
    on.exit(dbDisconnect(con), add = TRUE)
    dbGetQuery(con, "SELECT * FROM produk")
  })
  
  output$data_produk <- renderDT({
    datatable(data_produk())
  })
  
  output$bar_kategori <- renderPlot({
    data_produk() %>%
      count(kategori) %>%
      ggplot(aes(x = reorder(kategori, -n), y = n, fill = kategori)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Kategori", y = "Jumlah Produk") +
      theme(legend.position = "none")
  })
  
  output$histogram_harga <- renderPlot({
    ggplot(data_produk(), aes(x = harga)) +
      geom_histogram(bins = 15, fill = "#2c7fb8", color = "white") +
      theme_minimal() +
      labs(x = "Harga", y = "Jumlah Produk")
  })
  
  output$avg_harga_kategori <- renderPlot({
    data_produk() %>%
      group_by(kategori) %>%
      summarise(rata_rata = mean(harga)) %>%
      ggplot(aes(x = reorder(kategori, -rata_rata), y = rata_rata, fill = kategori)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Kategori", y = "Harga Rata-rata") +
      theme(legend.position = "none")
  })
  
  output$top_produk <- renderPlot({
    data_produk() %>%
      arrange(desc(harga)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(nama, harga), y = harga, fill = harga)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      labs(x = "Produk", y = "Harga")
  })
}

# Jalankan aplikasi
shinyApp(ui, server)