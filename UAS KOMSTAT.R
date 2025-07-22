# =====================================================
# DASHBOARD ANALISIS SOVI - UJIAN STATISTIKA TERAPAN
# Politeknik Statistika STIS - LENGKAP
# =====================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(leaflet)
library(leaflet.extras)
library(viridis)
library(car)
library(nortest)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(knitr)
library(rmarkdown)
library(zip)
library(moments)
library(e1071)
library(stats)
library(sf)
library(cluster)
library(factoextra)
library(ggdendro)

# Load SOVI data dari URL yang diberikan
sovi_data <- read_csv("sovi_data.csv")

# Load distance matrix
distance_matrix <- read_csv("distance.csv")
# Convert to proper matrix format, removing first column which is row index
distance_matrix <- as.matrix(distance_matrix[, -1])

# Load shapefiles
peta_kabupaten <- st_read("Administrasi_Kabupaten.shp")

# Gabungkan kdprov dan kdkab untuk membuat DISTRICTCODE yang cocok
peta_kabupaten <- peta_kabupaten %>%
  mutate(
    kdprov = as.numeric(as.character(kdprov)),
    kdkab = as.numeric(as.character(kdkab)),
    DISTRICTCODE = as.numeric(paste0(kdprov, sprintf("%02d", kdkab)))
  )

# Gabungkan data sovi_data dengan data peta
sovi_peta <- left_join(peta_kabupaten, sovi_data, by = "DISTRICTCODE")

# Periksa struktur data
print("Struktur SOVI Data:")
print(names(sovi_data))
print("Struktur Distance Matrix:")
print(dim(distance_matrix))

# Create additional categorical variables
sovi_data$Population_Size <- ifelse(sovi_data$CHILDREN > median(sovi_data$CHILDREN, na.rm = TRUE), "Besar", "Kecil")

sovi_data$Economic_Status <- cut(sovi_data$POVERTY,
                                 breaks = quantile(sovi_data$POVERTY, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                                 labels = c("Kemiskinan_Rendah", "Kemiskinan_Sedang", "Kemiskinan_Tinggi"), include.lowest = TRUE)

sovi_data$Age_Group <- cut(sovi_data$ELDERLY,
                           breaks = quantile(sovi_data$ELDERLY, probs = c(0, 0.5, 1), na.rm = TRUE),
                           labels = c("Muda", "Tua"), include.lowest = TRUE)

sovi_data$Education_Level <- cut(sovi_data$LOWEDU,
                                 breaks = quantile(sovi_data$LOWEDU, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                                 labels = c("Pendidikan_Tinggi", "Pendidikan_Sedang", "Pendidikan_Rendah"), include.lowest = TRUE)

# Formal dashboard color palette
colors <- c("#2C3E50", "#34495E", "#ECF0F1", "#BDC3C7", "#95A5A6", "#7F8C8D", "#E74C3C", "#3498DB", "#2ECC71")

# Custom CSS dengan warna formal
custom_css <- paste0("
.content-wrapper, .right-side {
  background-color: ", colors[3], ";
}
.main-header .navbar {
  background-color: ", colors[1], " !important;
}
.main-header .logo {
  background-color: ", colors[1], " !important;
}
.sidebar {
  background-color: ", colors[2], " !important;
}
.box {
  border-radius: 8px !important;
  box-shadow: 0 4px 12px rgba(44, 62, 80, 0.1) !important;
  border-top: 3px solid ", colors[1], " !important;
}
.btn-primary {
  background-color: ", colors[1], " !important;
  border-color: ", colors[1], " !important;
}
.value-box-icon {
  background-color: rgba(44, 62, 80, 0.2) !important;
}
")

# UI
ui <- dashboardPage(
  title = "Dashboard Analisis SOVI - Ujian Statistika Terapan STIS",
  skin = "blue",
  
  dashboardHeader(
    title = "Dashboard Analisis SOVI - Ujian STIS 2025",
    titleWidth = 400
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("calculator"),
               menuSubItem("Uji Rata-rata", tabName = "uji_rata"),
               menuSubItem("Uji Proporsi & Varians", tabName = "uji_proporsi"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("line-chart"))
    ),
    
    # Download section in sidebar
    div(
      style = "background: rgba(255,255,255,0.1); margin: 10px; padding: 15px; border-radius: 8px;",
      conditionalPanel(
        condition = "input.sidebar == 'beranda'",
        h5("Download Beranda", style = "color: white;"),
        downloadButton("download_beranda_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_beranda_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_beranda_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_beranda_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'manajemen'",
        h5("Download Manajemen", style = "color: white;"),
        downloadButton("download_manajemen_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_manajemen_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_manajemen_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_manajemen_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'eksplorasi'",
        h5("Download Eksplorasi", style = "color: white;"),
        downloadButton("download_eksplorasi_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_eksplorasi_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_eksplorasi_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_eksplorasi_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'asumsi'",
        h5("Download Uji Asumsi", style = "color: white;"),
        downloadButton("download_asumsi_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_asumsi_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_asumsi_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_asumsi_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'uji_rata' || input.sidebar == 'uji_proporsi' || input.sidebar == 'anova'",
        h5("Download Inferensia", style = "color: white;"),
        downloadButton("download_inferensia_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_inferensia_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_inferensia_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_inferensia_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'regresi'",
        h5("Download Regresi", style = "color: white;"),
        downloadButton("download_regresi_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_regresi_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_regresi_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_regresi_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      # Beranda Tab
      tabItem(
        tabName = "beranda",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("Dashboard Analisis SOVI", style = "margin: 0; font-weight: 600;"),
                   p("Social Vulnerability Index Analysis - Ujian Statistika Terapan STIS 2025", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        # Enhanced Metric Cards
        fluidRow(
          column(3,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("total_observations")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Total Observasi")
                 )
          ),
          column(3,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[7], " 0%, ", colors[8], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("avg_poverty_rate")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Rata-rata Kemiskinan (%)")
                 )
          ),
          column(3,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[8], " 0%, ", colors[9], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("avg_education_rate")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Rata-rata Pendidikan Rendah (%)")
                 )
          ),
          column(3,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[9], " 0%, ", colors[1], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("avg_growth_rate")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Rata-rata Pertumbuhan (%)")
                 )
          )
        ),
        
        # Additional metrics row
        fluidRow(
          column(4,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[4], " 0%, ", colors[5], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("total_variables")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Total Variabel")
                 )
          ),
          column(4,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[5], " 0%, ", colors[6], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("data_completeness")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Kelengkapan Data")
                 )
          ),
          column(4,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[6], " 0%, ", colors[7], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("avg_elderly_rate")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Rata-rata Lansia (%)")
                 )
          )
        ),
        
        # Metadata Dashboard
        fluidRow(
          column(12,
                 box(
                   title = "Metadata Dashboard", status = "primary", solidHeader = TRUE, width = NULL,
                   div(
                     style = "padding: 15px;",
                     h4("Informasi Dataset SOVI", style = paste0("color: ", colors[1], "; margin-bottom: 15px;")),
                     fluidRow(
                       column(6,
                              tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                      tags$li(strong("Nama Dashboard:"), " Dashboard Analisis SOVI - Ujian STIS 2025"),
                                      tags$li(strong("Dataset:"), " Social Vulnerability Index"),
                                      tags$li(strong("Sumber Data:"), " https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"),
                                      tags$li(strong("Metadata:"), " https://www.sciencedirect.com/science/article/pii/S2352340921010180"),
                                      tags$li(strong("Jumlah Observasi:"), textOutput("total_observations_meta", inline = TRUE)),
                                      tags$li(strong("Jumlah Variabel:"), textOutput("total_variables_meta", inline = TRUE))
                              )
                       ),
                       column(6,
                              tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                      tags$li(strong("Platform:"), " R Shiny"),
                                      tags$li(strong("Kelengkapan Data:"), textOutput("data_completeness_meta", inline = TRUE)),
                                      tags$li(strong("Fitur Utama:"), " Analisis Statistik Komprehensif"),
                                      tags$li(strong("Download Format:"), " JPG, PDF, Word"),
                                      tags$li(strong("Ujian:"), " 23 Juli 2025, 10.30-12.30 WIB"),
                                      tags$li(strong("Fakta Integritas:"), " https://s.stis.ac.id/Fakta_integritas_KOMSTAT")
                              )
                       )
                     )
                   )
                 )
          )
        ),
        
        # Enhanced Main content with multiple visualizations
        fluidRow(
          column(6,
                 box(
                   title = "Distribusi Indikator Utama SOVI", status = "primary", solidHeader = TRUE, width = NULL,
                   plotlyOutput("sovi_distribution", height = "350px")
                 )
          ),
          column(6,
                 box(
                   title = "Korelasi Antar Variabel Kunci", status = "info", solidHeader = TRUE, width = NULL,
                   plotlyOutput("correlation_plot", height = "350px")
                 )
          )
        ),
        
        # Additional analysis row
        fluidRow(
          column(6,
                 box(
                   title = "Statistik Ringkasan Multi-Indikator", status = "info", solidHeader = TRUE, width = NULL,
                   verbatimTextOutput("summary_stats")
                 )
          ),
          column(6,
                 box(
                   title = "Distribusi Regional", status = "success", solidHeader = TRUE, width = NULL,
                   plotlyOutput("regional_distribution", height = "350px")
                 )
          )
        ),
        
        # Peta Distribusi
        fluidRow(
          column(12,
                 box(
                   title = "Peta Distribusi SOVI Indonesia", status = "primary", solidHeader = TRUE, width = NULL,
                   leafletOutput("beranda_map", height = "400px")
                 )
          )
        ),
        
        # Enhanced Interpretasi
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: ", colors[3], "; padding: 20px; border-radius: 8px; border-left: 4px solid ", colors[1], "; margin-top: 15px;"),
                   h4("Interpretasi Komprehensif Dashboard Beranda", style = paste0("color: ", colors[1], ";")),
                   uiOutput("beranda_interpretation")
                 )
          )
        )
      ),
      
      # Manajemen Data Tab
      tabItem(
        tabName = "manajemen",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("Manajemen Data", style = "margin: 0; font-weight: 600;"),
                   p("Kelola dan transformasi data SOVI untuk analisis optimal", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        # Enhanced overview cards
        fluidRow(
          column(3,
                 div(
                   style = paste0("background: ", colors[1], "; color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 20px;"),
                   h3(style = "margin: 0; font-size: 2.5em;", textOutput("data_quality_score", inline = TRUE)),
                   p("Skor Kualitas Data", style = "margin: 5px 0 0 0; font-size: 0.9em;")
                 )
          ),
          column(3,
                 div(
                   style = paste0("background: ", colors[7], "; color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 20px;"),
                   h3(style = "margin: 0; font-size: 2.5em;", textOutput("missing_percentage", inline = TRUE)),
                   p("Data Hilang (%)", style = "margin: 5px 0 0 0; font-size: 0.9em;")
                 )
          ),
          column(3,
                 div(
                   style = paste0("background: ", colors[8], "; color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 20px;"),
                   h3(style = "margin: 0; font-size: 2.5em;", textOutput("numeric_vars_count", inline = TRUE)),
                   p("Variabel Numerik", style = "margin: 5px 0 0 0; font-size: 0.9em;")
                 )
          ),
          column(3,
                 div(
                   style = paste0("background: ", colors[9], "; color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 20px;"),
                   h3(style = "margin: 0; font-size: 2.5em;", textOutput("categorical_vars_count", inline = TRUE)),
                   p("Variabel Kategorik", style = "margin: 5px 0 0 0; font-size: 0.9em;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Alat Manajemen Data Lanjutan", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Ringkasan Data",
                              br(),
                              fluidRow(
                                column(6,
                                       h4("Profil Dataset Lengkap", style = paste0("color: ", colors[1], ";")),
                                       div(
                                         style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-bottom: 15px;"),
                                         verbatimTextOutput("enhanced_data_summary")
                                       )
                                ),
                                column(6,
                                       h4("Analisis Tipe Data", style = paste0("color: ", colors[1], ";")),
                                       DT::dataTableOutput("data_types_table")
                                )
                              ),
                              
                              fluidRow(
                                column(12,
                                       h4("Deteksi Outlier dan Anomali", style = paste0("color: ", colors[1], ";")),
                                       plotlyOutput("outlier_detection_plot", height = "300px")
                                )
                              ),
                              
                              div(
                                style = paste0("background: ", colors[3], "; padding: 20px; border-radius: 8px; margin-top: 20px; border-left: 4px solid ", colors[1], ";"),
                                h5("Interpretasi Kualitas Data", style = paste0("color: ", colors[1], ";")),
                                uiOutput("enhanced_data_interpretation")
                              )
                     ),
                     
                     tabPanel("Kategorisasi Data",
                              br(),
                              div(
                                style = paste0("background: white; border-radius: 8px; padding: 20px; border: 1px solid ", colors[2], ";"),
                                h4("Mengubah Data Kontinu menjadi Kategorik", style = paste0("color: ", colors[1], ";")),
                                
                                fluidRow(
                                  column(4,
                                         selectInput("categorize_variable", "Pilih Variabel:",
                                                     choices = NULL),
                                         selectInput("categorize_method", "Metode Kategorisasi:",
                                                     choices = list(
                                                       "Kuartil (4 kategori)" = "quartile",
                                                       "Tertil (3 kategori)" = "tertile",
                                                       "Median (2 kategori)" = "median"
                                                     ))
                                  ),
                                  column(4,
                                         textInput("category_labels", "Label Kategori (pisahkan dengan koma):",
                                                   value = "Rendah, Sedang, Tinggi"),
                                         br(),
                                         actionButton("apply_categorization", "Terapkan Kategorisasi",
                                                      class = "btn-primary", style = "width: 100%;")
                                  ),
                                  column(4,
                                         h5("Preview Kategorisasi"),
                                         div(
                                           style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 6px;"),
                                           verbatimTextOutput("categorization_preview")
                                         )
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition = "input.apply_categorization > 0",
                                  br(),
                                  h4("Hasil Kategorisasi", style = paste0("color: ", colors[1], ";")),
                                  fluidRow(
                                    column(6,
                                           h5("Tabel Frekuensi"),
                                           DT::dataTableOutput("categorization_table")
                                    ),
                                    column(6,
                                           h5("Visualisasi Kategori"),
                                           plotlyOutput("categorization_plot", height = "300px")
                                    )
                                  ),
                                  
                                  div(
                                    style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                    h5("Interpretasi Kategorisasi", style = paste0("color: ", colors[1], ";")),
                                    uiOutput("categorization_interpretation")
                                  )
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Eksplorasi Data Tab
      tabItem(
        tabName = "eksplorasi",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("Eksplorasi Data", style = "margin: 0; font-weight: 600;"),
                   p("Analisis deskriptif dan visualisasi data SOVI", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Eksplorasi Data Interaktif", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Statistik Deskriptif",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("descriptive_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("descriptive_group", "Kelompokkan berdasarkan:",
                                                   choices = c("Tidak ada" = "none",
                                                               "Kategori SOVI" = "SOVI_Category",
                                                               "Ukuran Populasi" = "Population_Size",
                                                               "Status Ekonomi" = "Economic_Status"))
                                ),
                                column(4,
                                       br(),
                                       actionButton("run_descriptive", "Jalankan Analisis",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_descriptive > 0",
                                fluidRow(
                                  column(6,
                                         h4("Statistik Deskriptif", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("descriptive_stats")
                                  ),
                                  column(6,
                                         h4("Tabel Ringkasan", style = paste0("color: ", colors[1], ";")),
                                         DT::dataTableOutput("descriptive_table")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Statistik Deskriptif", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("descriptive_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Visualisasi Data",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("plot_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("plot_type", "Jenis Plot:",
                                                   choices = list(
                                                     "Histogram" = "histogram",
                                                     "Box Plot" = "boxplot",
                                                     "Density Plot" = "density"
                                                   ))
                                ),
                                column(4,
                                       checkboxInput("show_stats", "Tampilkan Statistik", value = TRUE)
                                )
                              ),
                              
                              fluidRow(
                                column(8,
                                       plotlyOutput("visualization_plot", height = "450px")
                                ),
                                column(4,
                                       conditionalPanel(
                                         condition = "input.show_stats",
                                         h4("Statistik Deskriptif", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("plot_stats")
                                       )
                                )
                              ),
                              
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                h5("Interpretasi Visualisasi", style = paste0("color: ", colors[1], ";")),
                                uiOutput("visualization_interpretation")
                              )
                     ),
                     
                     tabPanel("Peta Interaktif",
                              br(),
                              fluidRow(
                                column(3,
                                       selectInput("map_variable", "Variabel untuk Peta:",
                                                   choices = NULL),
                                       selectInput("map_type", "Jenis Peta:",
                                                   choices = list(
                                                     "Scatter Plot" = "scatter",
                                                     "Heat Map" = "heatmap"
                                                   ))
                                ),
                                column(9,
                                       leafletOutput("exploration_map", height = "500px")
                                )
                              ),
                              
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                h5("Interpretasi Peta", style = paste0("color: ", colors[1], ";")),
                                uiOutput("map_interpretation")
                              )
                     ),
                     
                     tabPanel("Analisis Cluster",
                              br(),
                              fluidRow(
                                column(4,
                                       h5("Pengaturan Clustering", style = paste0("color: ", colors[1], ";")),
                                       selectInput("cluster_variables", "Pilih Variabel untuk Clustering:",
                                                   choices = NULL, multiple = TRUE),
                                       selectInput("cluster_method", "Metode Clustering:",
                                                   choices = list(
                                                     "K-Means" = "kmeans",
                                                     "Hierarchical" = "hierarchical",
                                                     "PAM (K-Medoids)" = "pam"
                                                   )),
                                       conditionalPanel(
                                         condition = "input.cluster_method == 'kmeans' || input.cluster_method == 'pam'",
                                         numericInput("n_clusters", "Jumlah Cluster:", value = 3, min = 2, max = 10)
                                       ),
                                       conditionalPanel(
                                         condition = "input.cluster_method == 'hierarchical'",
                                         selectInput("linkage_method", "Metode Linkage:",
                                                     choices = list("ward.D2", "single", "complete", "average"))
                                       ),
                                       br(),
                                       actionButton("run_clustering", "Jalankan Analisis Cluster",
                                                    class = "btn-primary", style = "width: 100%;")
                                ),
                                column(8,
                                       conditionalPanel(
                                         condition = "input.run_clustering > 0",
                                         h5("Visualisasi Cluster", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("cluster_plot", height = "400px")
                                       )
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_clustering > 0",
                                br(),
                                fluidRow(
                                  column(6,
                                         h5("Hasil Clustering", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("cluster_summary")
                                  ),
                                  column(6,
                                         h5("Tabel Cluster", style = paste0("color: ", colors[1], ";")),
                                         DT::dataTableOutput("cluster_table")
                                  )
                                ),
                                
                                fluidRow(
                                  column(12,
                                         h5("Dendrogram (untuk Hierarchical Clustering)", style = paste0("color: ", colors[1], ";")),
                                         conditionalPanel(
                                           condition = "input.cluster_method == 'hierarchical'",
                                           plotlyOutput("dendrogram_plot", height = "300px")
                                         )
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Analisis Cluster", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("cluster_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Uji Asumsi Tab
      tabItem(
        tabName = "asumsi",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("Uji Asumsi Statistik", style = "margin: 0; font-weight: 600;"),
                   p("Verifikasi asumsi normalitas dan homogenitas untuk analisis statistik", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Pengujian Asumsi Statistik", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Uji Normalitas",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("normality_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("normality_test", "Jenis Uji:",
                                                   choices = list(
                                                     "Shapiro-Wilk" = "shapiro",
                                                     "Anderson-Darling" = "anderson",
                                                     "Kolmogorov-Smirnov" = "ks"
                                                   ))
                                ),
                                column(4,
                                       br(),
                                       actionButton("run_normality", "Jalankan Uji",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_normality > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji Normalitas", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("normality_result")
                                  ),
                                  column(6,
                                         h4("Q-Q Plot", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("qq_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Histogram dengan Kurva Normal", style = paste0("color: ", colors[1], ";")),
                                plotlyOutput("normality_histogram", height = "350px"),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Uji Normalitas", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("normality_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Uji Homogenitas",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("homogeneity_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("homogeneity_group", "Variabel Pengelompokan:",
                                                   choices = c("SOVI_Category", "Population_Size", "Economic_Status"))
                                ),
                                column(4,
                                       br(),
                                       actionButton("run_homogeneity", "Jalankan Uji",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_homogeneity > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji Levene", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("homogeneity_result")
                                  ),
                                  column(6,
                                         h4("Box Plot berdasarkan Kelompok", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("homogeneity_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Uji Homogenitas", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("homogeneity_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Uji Rata-rata Tab
      tabItem(
        tabName = "uji_rata",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("Uji Beda Rata-rata", style = "margin: 0; font-weight: 600;"),
                   p("Analisis perbedaan rata-rata menggunakan uji t", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Uji Beda Rata-rata", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Uji t Satu Sampel",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("onesample_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       numericInput("mu_hypothesis", "Nilai Hipotesis (μ₀):",
                                                    value = 0, step = 0.1)
                                ),
                                column(4,
                                       selectInput("alternative_hypothesis", "Hipotesis Alternatif:",
                                                   choices = list(
                                                     "≠ (Dua arah)" = "two.sided",
                                                     "> (Lebih besar)" = "greater",
                                                     "< (Lebih kecil)" = "less"
                                                   ))
                                )
                              ),
                              
                              actionButton("run_onesample", "Jalankan Uji",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_onesample > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji t Satu Sampel", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("onesample_result")
                                  ),
                                  column(6,
                                         h4("Visualisasi", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("onesample_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Uji t Satu Sampel", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("onesample_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Uji t Dua Sampel",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("twosample_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("twosample_group", "Variabel Pengelompokan:",
                                                   choices = c("Population_Size"))
                                ),
                                column(4,
                                       checkboxInput("equal_variances", "Asumsi Varians Sama", value = TRUE)
                                )
                              ),
                              
                              actionButton("run_twosample", "Jalankan Uji",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_twosample > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji t Dua Sampel", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("twosample_result")
                                  ),
                                  column(6,
                                         h4("Perbandingan Kelompok", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("twosample_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Uji t Dua Sampel", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("twosample_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Uji Proporsi & Varians Tab
      tabItem(
        tabName = "uji_proporsi",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("Uji Proporsi & Varians", style = "margin: 0; font-weight: 600;"),
                   p("Analisis proporsi dan varians dengan metode statistik", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Uji Proporsi & Varians", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Uji Proporsi",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("prop_variable", "Pilih Variabel Kategorik:",
                                                   choices = c("SOVI_Category", "Population_Size", "Economic_Status"))
                                ),
                                column(4,
                                       selectInput("prop_category", "Kategori yang Diuji:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       numericInput("prop_hypothesis", "Proporsi Hipotesis:",
                                                    value = 0.5, min = 0, max = 1, step = 0.01)
                                )
                              ),
                              
                              actionButton("run_prop_test", "Jalankan Uji",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_prop_test > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji Proporsi", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("prop_test_result")
                                  ),
                                  column(6,
                                         h4("Visualisasi Proporsi", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("prop_test_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Uji Proporsi", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("prop_test_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Uji Varians",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("var_test_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("var_test_group", "Variabel Pengelompokan:",
                                                   choices = c("Population_Size"))
                                ),
                                column(4,
                                       br(),
                                       actionButton("run_var_test", "Jalankan Uji F",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_var_test > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji F", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("var_test_result")
                                  ),
                                  column(6,
                                         h4("Perbandingan Varians", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("var_test_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Uji Varians", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("var_test_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # ANOVA Tab
      tabItem(
        tabName = "anova",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("Analysis of Variance (ANOVA)", style = "margin: 0; font-weight: 600;"),
                   p("Analisis varians untuk membandingkan rata-rata lebih dari dua kelompok", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Analisis ANOVA", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("ANOVA Satu Arah",
                              br(),
                              fluidRow(
                                column(6,
                                       selectInput("anova_variable", "Pilih Variabel Dependen:",
                                                   choices = NULL)
                                ),
                                column(6,
                                       selectInput("anova_group", "Pilih Variabel Pengelompokan:",
                                                   choices = c("SOVI_Category", "Economic_Status", "Education_Level"))
                                )
                              ),
                              
                              actionButton("run_anova", "Jalankan ANOVA",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_anova > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil ANOVA", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("anova_result")
                                  ),
                                  column(6,
                                         h4("Plot Rata-rata Kelompok", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("anova_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Uji Post-hoc (Tukey HSD)", style = paste0("color: ", colors[1], ";")),
                                verbatimTextOutput("posthoc_result"),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi ANOVA Satu Arah", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("anova_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("ANOVA Dua Arah",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("anova2_variable", "Pilih Variabel Dependen:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("anova2_factor1", "Faktor 1:",
                                                   choices = c("SOVI_Category", "Population_Size", "Economic_Status"))
                                ),
                                column(4,
                                       selectInput("anova2_factor2", "Faktor 2:",
                                                   choices = c("Age_Group", "Education_Level"))
                                )
                              ),
                              
                              checkboxInput("include_interaction", "Sertakan Interaksi", value = TRUE),
                              
                              actionButton("run_anova2", "Jalankan ANOVA Dua Arah",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_anova2 > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil ANOVA Dua Arah", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("anova2_result")
                                  ),
                                  column(6,
                                         h4("Plot Interaksi", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("anova2_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Uji Post-hoc untuk Faktor Utama", style = paste0("color: ", colors[1], ";")),
                                verbatimTextOutput("posthoc2_result"),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi ANOVA Dua Arah", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("anova2_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Regresi Linear Berganda Tab
      tabItem(
        tabName = "regresi",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("Regresi Linear Berganda", style = "margin: 0; font-weight: 600;"),
                   p("Analisis regresi dengan diagnostik dan validasi asumsi", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Regresi Linear Berganda", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Pembangunan Model",
                              br(),
                              fluidRow(
                                column(6,
                                       selectInput("regression_response", "Variabel Respons (Y):",
                                                   choices = NULL)
                                ),
                                column(6,
                                       selectInput("regression_predictors", "Variabel Prediktor (X):",
                                                   choices = NULL, multiple = TRUE)
                                )
                              ),
                              
                              actionButton("run_regression", "Jalankan Regresi",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_regression > 0",
                                h4("Hasil Regresi Linear Berganda", style = paste0("color: ", colors[1], ";")),
                                verbatimTextOutput("regression_result"),
                                
                                fluidRow(
                                  column(6,
                                         h4("Ringkasan Model", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("model_summary")
                                  ),
                                  column(6,
                                         h4("Fitted vs Actual", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("fitted_actual_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Model", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("regression_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Plot Diagnostik",
                              br(),
                              conditionalPanel(
                                condition = "input.run_regression > 0",
                                h4("Plot Diagnostik Regresi", style = paste0("color: ", colors[1], "; margin-bottom: 20px;")),
                                
                                fluidRow(
                                  column(6,
                                         h5("Residual vs Fitted"),
                                         plotlyOutput("residuals_fitted", height = "300px")
                                  ),
                                  column(6,
                                         h5("Q-Q Plot Residual"),
                                         plotlyOutput("qq_residuals", height = "300px")
                                  )
                                ),
                                
                                fluidRow(
                                  column(6,
                                         h5("Scale-Location Plot"),
                                         plotlyOutput("scale_location_plot", height = "300px")
                                  ),
                                  column(6,
                                         h5("Residual vs Leverage"),
                                         plotlyOutput("leverage_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Plot Diagnostik", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("diagnostic_interpretation")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_regression == 0",
                                div(style = "text-align: center; padding: 50px; color: #999;",
                                    p("Silakan jalankan model regresi terlebih dahulu di tab 'Pembangunan Model'."))
                              )
                     ),
                     
                     tabPanel("Uji Asumsi",
                              br(),
                              conditionalPanel(
                                condition = "input.run_regression > 0",
                                h4("Uji Asumsi Regresi", style = paste0("color: ", colors[1], ";")),
                                
                                fluidRow(
                                  column(6,
                                         h5("Uji Multikolinearitas (VIF)"),
                                         verbatimTextOutput("multicollinearity_test")
                                  ),
                                  column(6,
                                         h5("Uji Durbin-Watson"),
                                         verbatimTextOutput("durbin_watson_test")
                                  )
                                ),
                                
                                fluidRow(
                                  column(6,
                                         h5("Uji Normalitas Residual"),
                                         verbatimTextOutput("residual_normality_test")
                                  ),
                                  column(6,
                                         h5("Uji Homoskedastisitas"),
                                         verbatimTextOutput("homoscedasticity_test")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Uji Asumsi", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("assumption_interpretation")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_regression == 0",
                                div(style = "text-align: center; padding: 50px; color: #999;",
                                    p("Silakan jalankan model regresi terlebih dahulu di tab 'Pembangunan Model'."))
                              )
                     )
                   )
                 )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    regression_model = NULL,
    categorized_variable = NULL
  )
  
  # Update choices when app starts
  observe({
    numeric_vars <- names(select_if(sovi_data, is.numeric))
    
    # Update all select inputs
    updateSelectInput(session, "categorize_variable", choices = numeric_vars)
    updateSelectInput(session, "descriptive_variable", choices = numeric_vars)
    updateSelectInput(session, "plot_variable", choices = numeric_vars)
    updateSelectInput(session, "map_variable", choices = numeric_vars)
    updateSelectInput(session, "normality_variable", choices = numeric_vars)
    updateSelectInput(session, "homogeneity_variable", choices = numeric_vars)
    updateSelectInput(session, "onesample_variable", choices = numeric_vars)
    updateSelectInput(session, "twosample_variable", choices = numeric_vars)
    updateSelectInput(session, "var_test_variable", choices = numeric_vars)
    updateSelectInput(session, "anova_variable", choices = numeric_vars)
    updateSelectInput(session, "anova2_variable", choices = numeric_vars)
    updateSelectInput(session, "regression_response", choices = numeric_vars)
    updateSelectInput(session, "regression_predictors", choices = numeric_vars)
    
    # Update cluster analysis choices
    updateSelectInput(session, "cluster_variables", choices = numeric_vars)
  })
  
  # Update proportion category choices
  observe({
    req(input$prop_variable)
    categories <- unique(sovi_data[[input$prop_variable]])
    categories <- categories[!is.na(categories)]
    updateSelectInput(session, "prop_category", choices = categories)
  })
  
  # Enhanced Home tab outputs
  output$total_observations <- renderText({
    format(nrow(sovi_data), big.mark = ",")
  })
  
  output$total_observations_meta <- renderText({
    format(nrow(sovi_data), big.mark = ",")
  })
  
  output$total_variables <- renderText({
    ncol(sovi_data)
  })
  
  output$total_variables_meta <- renderText({
    ncol(sovi_data)
  })
  
  output$avg_poverty_rate <- renderText({
    round(mean(sovi_data$POVERTY, na.rm = TRUE), 1)
  })
  
  output$avg_education_rate <- renderText({
    round(mean(sovi_data$LOWEDU, na.rm = TRUE), 1)
  })
  
  output$avg_growth_rate <- renderText({
    round(mean(sovi_data$GROWTH, na.rm = TRUE), 1)
  })
  
  output$avg_elderly_rate <- renderText({
    round(mean(sovi_data$ELDERLY, na.rm = TRUE), 1)
  })
  
  output$data_completeness <- renderText({
    complete_rows <- sum(complete.cases(sovi_data))
    total_rows <- nrow(sovi_data)
    paste0(round(complete_rows/total_rows * 100, 1), "%")
  })
  
  output$data_completeness_meta <- renderText({
    complete_rows <- sum(complete.cases(sovi_data))
    total_rows <- nrow(sovi_data)
    paste0(round(complete_rows/total_rows * 100, 1), "%")
  })
  
  # Enhanced summary stats for multiple indicators
  output$summary_stats <- renderPrint({
    key_vars <- sovi_data[, c("POVERTY", "LOWEDU", "ELDERLY", "GROWTH", "CHILDREN")]
    summary(key_vars)
  })
  
  # New correlation plot
  output$correlation_plot <- renderPlotly({
    numeric_vars <- sovi_data[, c("POVERTY", "LOWEDU", "ELDERLY", "GROWTH", "CHILDREN")]
    cor_matrix <- cor(numeric_vars, use = "complete.obs")
    
    # Create heatmap
    p <- plot_ly(
      x = colnames(cor_matrix),
      y = colnames(cor_matrix),
      z = cor_matrix,
      type = "heatmap",
      colorscale = list(c(0, colors[7]), c(0.5, colors[3]), c(1, colors[8])),
      showscale = TRUE
    ) %>%
      layout(
        title = "Matriks Korelasi Variabel Kunci",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
    
    p %>% config(displayModeBar = FALSE)
  })
  
  # Regional distribution plot
  output$regional_distribution <- renderPlotly({
    regional_stats <- sovi_data %>%
      mutate(Region = case_when(
        substr(as.character(DISTRICTCODE), 1, 2) %in% c("11", "12", "13", "14", "15", "16", "17", "18", "19", "21") ~ "Sumatera",
        substr(as.character(DISTRICTCODE), 1, 2) %in% c("31", "32", "33", "34", "35", "36") ~ "Jawa-Bali",
        substr(as.character(DISTRICTCODE), 1, 2) %in% c("51", "52", "53", "61", "62", "63", "64", "71", "72", "73", "74", "75", "76") ~ "Kalimantan-Sulawesi",
        TRUE ~ "Indonesia Timur"
      )) %>%
      group_by(Region) %>%
      summarise(
        Avg_Poverty = mean(POVERTY, na.rm = TRUE),
        Count = n(),
        .groups = "drop"
      )
    
    p <- ggplot(regional_stats, aes(x = Region, y = Avg_Poverty, fill = Region)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = colors[1:4]) +
      labs(title = "Rata-rata Kemiskinan per Region", x = "Region", y = "Rata-rata Kemiskinan (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # SOVI distribution plot
  output$sovi_distribution <- renderPlotly({
    var_data <- sovi_data$POVERTY
    var_name <- "POVERTY"
    
    p <- ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
      geom_density(aes(y = after_stat(density) * length(var_data) * diff(range(var_data, na.rm = TRUE))/30),
                   color = colors[2], size = 2) +
      labs(title = paste("Distribusi", var_name),
           x = var_name, y = "Frekuensi") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = colors[1]),
        axis.title = element_text(size = 12, color = colors[1])
      )
    
    ggplotly(p) %>%
      layout(showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  
  # Beranda map
  # Beranda map
  # GANTI KODE PETA LAMA DENGAN INI
  # Beranda - Interactive Map
  # ==========================================================
  # LANGKAH 3: GANTI KODE PETA ANDA DENGAN INI
  # ==========================================================
  output$beranda_map <- renderLeaflet({
    # Filter hanya wilayah yang memiliki data SOVI setelah penggabungan
    sovi_peta_valid <- sovi_peta %>% filter(!is.na(POVERTY))
    
    # Buat palet warna berdasarkan Tingkat Kemiskinan (POVERTY)
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = sovi_peta_valid$POVERTY
    )
    
    leaflet(sovi_peta_valid) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addPolygons(
        fillColor = ~pal(POVERTY),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        # ==========================================================
        # PERBAIKAN FINAL UNTUK LABEL PETA
        # ==========================================================
        label = ~lapply(paste(
          "<strong>", nmkab, "</strong><br/>",
          "Provinsi: ", nmprov, "<br/>",
          "Tingkat Kemiskinan: ", round(POVERTY, 2), "%<br/>",
          "Pendidikan Rendah: ", round(LOWEDU, 2), "%"
        ), HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = ~POVERTY, 
        opacity = 0.7, 
        title = "Tingkat Kemiskinan (%)",
        position = "bottomright"
      )
  })
  
  # Enhanced Beranda interpretation
  output$beranda_interpretation <- renderUI({
    total_obs <- nrow(sovi_data)
    total_vars <- ncol(sovi_data)
    completeness <- round(sum(complete.cases(sovi_data))/nrow(sovi_data) * 100, 1)
    avg_poverty <- round(mean(sovi_data$POVERTY, na.rm = TRUE), 1)
    avg_education <- round(mean(sovi_data$LOWEDU, na.rm = TRUE), 1)
    avg_growth <- round(mean(sovi_data$GROWTH, na.rm = TRUE), 1)
    avg_elderly <- round(mean(sovi_data$ELDERLY, na.rm = TRUE), 1)
    
    interpretation <- paste0(
      "Dashboard ini menyediakan analisis komprehensif untuk dataset Social Vulnerability Index (SOVI) dengan ", total_obs, " observasi kabupaten/kota di Indonesia dan ", total_vars, " variabel indikator. ",
      "Tingkat kelengkapan data sebesar ", completeness, "% menunjukkan kualitas data yang ",
      if(completeness >= 90) "sangat baik" else if(completeness >= 80) "baik" else "perlu perhatian", " untuk analisis statistik lanjutan.<br><br>",
      
      "<strong>Ringkasan Indikator Kunci:</strong><br>",
      "• Rata-rata tingkat kemiskinan: ", avg_poverty, "% menunjukkan variasi signifikan antar daerah<br>",
      "• Rata-rata pendidikan rendah: ", avg_education, "% mengindikasikan tantangan pendidikan nasional<br>",
      "• Rata-rata pertumbuhan: ", avg_growth, "% mencerminkan dinamika pembangunan regional<br>",
      "• Rata-rata proporsi lansia: ", avg_elderly, "% menggambarkan struktur demografis<br><br>",
      
      "Matriks korelasi menunjukkan hubungan antar variabel kunci, sementara distribusi regional mengungkap pola geografis kerentanan sosial. ",
      "Peta interaktif memungkinkan eksplorasi detail tingkat kabupaten/kota untuk identifikasi hotspot kerentanan. ",
      "Dashboard ini dirancang untuk ujian Statistika Terapan STIS 2025 dengan implementasi metodologi analisis yang komprehensif dan sesuai standar akademik."
    )
    
    HTML(interpretation)
  })
  
  # Enhanced Data Management outputs
  output$data_quality_score <- renderText({
    # Calculate data quality score based on completeness and consistency
    completeness <- sum(complete.cases(sovi_data))/nrow(sovi_data)
    # Simple scoring: primarily based on completeness
    quality_score <- round(completeness * 100, 0)
    paste0(quality_score, "%")
  })
  
  output$missing_percentage <- renderText({
    total_cells <- nrow(sovi_data) * ncol(sovi_data)
    missing_cells <- sum(is.na(sovi_data))
    missing_pct <- round((missing_cells / total_cells) * 100, 1)
    paste0(missing_pct, "%")
  })
  
  output$numeric_vars_count <- renderText({
    sum(sapply(sovi_data, is.numeric))
  })
  
  output$categorical_vars_count <- renderText({
    sum(sapply(sovi_data, function(x) is.factor(x) || is.character(x)))
  })
  
  output$enhanced_data_summary <- renderPrint({
    cat("PROFIL DATASET SOVI\n")
    cat("===================\n")
    cat("Dimensi Data:", nrow(sovi_data), "x", ncol(sovi_data), "\n")
    cat("Memori yang Digunakan:", format(object.size(sovi_data), units = "MB"), "\n\n")
    
    cat("DISTRIBUSI TIPE DATA:\n")
    cat("Numerik:", sum(sapply(sovi_data, is.numeric)), "variabel\n")
    cat("Kategorik:", sum(sapply(sovi_data, function(x) is.factor(x) || is.character(x))), "variabel\n\n")
    
    cat("KUALITAS DATA:\n")
    completeness <- sum(complete.cases(sovi_data))/nrow(sovi_data) * 100
    cat("Kelengkapan:", round(completeness, 1), "%\n")
    
    # Missing data per variable
    missing_summary <- sapply(sovi_data, function(x) sum(is.na(x)))
    if(any(missing_summary > 0)) {
      cat("Variabel dengan Data Hilang:\n")
      missing_vars <- missing_summary[missing_summary > 0]
      for(i in 1:length(missing_vars)) {
        cat("  -", names(missing_vars)[i], ":", missing_vars[i], "observasi\n")
      }
    } else {
      cat("Tidak ada data hilang\n")
    }
  })
  
  output$data_types_table <- DT::renderDataTable({
    type_summary <- data.frame(
      Variabel = names(sovi_data),
      Tipe = sapply(sovi_data, function(x) class(x)[1]),
      `Data Hilang` = sapply(sovi_data, function(x) sum(is.na(x))),
      `% Hilang` = round(sapply(sovi_data, function(x) sum(is.na(x))/length(x) * 100), 1),
      Min = sapply(sovi_data, function(x) if(is.numeric(x)) round(min(x, na.rm = TRUE), 2) else "N/A"),
      Max = sapply(sovi_data, function(x) if(is.numeric(x)) round(max(x, na.rm = TRUE), 2) else "N/A"),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      type_summary,
      options = list(pageLength = 15, scrollY = "300px", scrollX = TRUE),
      caption = "Analisis Tipe Data dan Profil Variabel"
    )
  })
  
  output$outlier_detection_plot <- renderPlotly({
    # Select key numeric variables for outlier detection
    key_vars <- sovi_data[, c("POVERTY", "LOWEDU", "ELDERLY", "GROWTH")]
    
    # Calculate z-scores
    z_scores <- key_vars %>%
      mutate_all(~ abs(scale(.)[,1])) %>%
      mutate(ID = row_number()) %>%
      pivot_longer(-ID, names_to = "Variable", values_to = "Z_Score")
    
    p <- ggplot(z_scores, aes(x = Variable, y = Z_Score)) +
      geom_boxplot(fill = colors[8], alpha = 0.7) +
      geom_hline(yintercept = 3, color = colors[7], linetype = "dashed", size = 1) +
      labs(title = "Deteksi Outlier (Z-Score > 3)", x = "Variabel", y = "Absolute Z-Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$enhanced_data_interpretation <- renderUI({
    completeness <- round(sum(complete.cases(sovi_data))/nrow(sovi_data) * 100, 1)
    missing_vars <- sum(sapply(sovi_data, function(x) sum(is.na(x))) > 0)
    numeric_vars <- sum(sapply(sovi_data, is.numeric))
    
    # Outlier analysis
    key_vars <- sovi_data[, c("POVERTY", "LOWEDU", "ELDERLY", "GROWTH")]
    outlier_counts <- sapply(key_vars, function(x) sum(abs(scale(x)[,1]) > 3, na.rm = TRUE))
    total_outliers <- sum(outlier_counts)
    
    interpretation <- paste0(
      "<strong>Evaluasi Kualitas Data SOVI:</strong><br><br>",
      
      "Dataset menunjukkan kualitas yang ", if(completeness >= 95) "sangat baik" else if(completeness >= 90) "baik" else "perlu perbaikan",
      " dengan tingkat kelengkapan ", completeness, "%. ",
      if(missing_vars > 0) paste0("Terdapat ", missing_vars, " variabel dengan data hilang yang perlu perhatian khusus. ") else "Semua variabel lengkap tanpa data hilang. ",
      "<br><br>",
      
      "<strong>Struktur Data:</strong> Dataset terdiri dari ", numeric_vars, " variabel numerik yang siap untuk analisis statistik lanjutan. ",
      "Tipe data sudah sesuai untuk berbagai metode analisis yang akan diterapkan.<br><br>",
      
      "<strong>Deteksi Anomali:</strong> Analisis outlier mengidentifikasi ", total_outliers, " observasi dengan nilai ekstrem (Z-score > 3). ",
      if(total_outliers > 0) "Outlier ini perlu dievaluasi lebih lanjut untuk menentukan apakah merupakan data valid atau anomali. " else "Tidak ditemukan outlier ekstrem yang signifikan. ",
      "Informasi ini penting untuk memilih metode analisis yang robust terhadap outlier sesuai dengan metodologi yang dipelajari di STIS."
    )
    
    HTML(interpretation)
  })
  
  # Categorization preview
  output$categorization_preview <- renderPrint({
    req(input$categorize_variable)
    
    var_data <- sovi_data[[input$categorize_variable]]
    
    if(input$categorize_method == "quartile") {
      breaks <- quantile(var_data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      cat("Kuartil:\n")
      print(breaks)
    } else if(input$categorize_method == "tertile") {
      breaks <- quantile(var_data, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
      cat("Tertil:\n")
      print(breaks)
    } else if(input$categorize_method == "median") {
      breaks <- quantile(var_data, probs = c(0, 0.5, 1), na.rm = TRUE)
      cat("Median:\n")
      print(breaks)
    }
  })
  
  # Apply categorization
  observeEvent(input$apply_categorization, {
    req(input$categorize_variable, input$categorize_method)
    
    var_data <- sovi_data[[input$categorize_variable]]
    labels <- trimws(strsplit(input$category_labels, ",")[[1]])
    
    if(input$categorize_method == "quartile") {
      breaks <- quantile(var_data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      if(length(labels) != 4) labels <- c("Q1", "Q2", "Q3", "Q4")
    } else if(input$categorize_method == "tertile") {
      breaks <- quantile(var_data, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
      if(length(labels) != 3) labels <- c("Rendah", "Sedang", "Tinggi")
    } else if(input$categorize_method == "median") {
      breaks <- quantile(var_data, probs = c(0, 0.5, 1), na.rm = TRUE)
      if(length(labels) != 2) labels <- c("Rendah", "Tinggi")
    }
    
    values$categorized_variable <- cut(var_data, breaks = breaks, labels = labels, include.lowest = TRUE)
  })
  
  output$categorization_table <- DT::renderDataTable({
    req(values$categorized_variable)
    
    freq_table <- table(values$categorized_variable, useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    
    result_df <- data.frame(
      Kategori = names(freq_table),
      Frekuensi = as.numeric(freq_table),
      Persentase = round(as.numeric(prop_table), 2)
    )
    
    DT::datatable(
      result_df,
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      caption = paste("Tabel Frekuensi Kategorisasi", input$categorize_variable)
    )
  })
  
  output$categorization_plot <- renderPlotly({
    req(values$categorized_variable)
    
    freq_data <- data.frame(
      Category = values$categorized_variable
    )
    
    p <- ggplot(freq_data, aes(x = Category)) +
      geom_bar(fill = colors[1], alpha = 0.7, color = "white") +
      labs(title = paste("Distribusi Kategori", input$categorize_variable),
           x = "Kategori", y = "Frekuensi") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = colors[1]),
        axis.title = element_text(size = 12, color = colors[1]),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$categorization_interpretation <- renderUI({
    req(values$categorized_variable)
    
    freq_table <- table(values$categorized_variable, useNA = "ifany")
    most_frequent <- names(freq_table)[which.max(freq_table)]
    least_frequent <- names(freq_table)[which.min(freq_table)]
    
    interpretation <- paste0(
      "Kategorisasi variabel ", input$categorize_variable, " menggunakan metode ", input$categorize_method, " menghasilkan ", length(freq_table), " kategori. ",
      "Kategori dengan frekuensi tertinggi adalah '", most_frequent, "' dengan ", max(freq_table), " observasi, ",
      "sedangkan kategori dengan frekuensi terendah adalah '", least_frequent, "' dengan ", min(freq_table), " observasi. ",
      "Distribusi ini dapat digunakan untuk analisis kategorik selanjutnya seperti uji chi-square atau analisis kontingensi sesuai dengan materi ujian STIS."
    )
    
    HTML(interpretation)
  })
  
  # Descriptive statistics
  observeEvent(input$run_descriptive, {
    req(input$descriptive_variable)
    
    output$descriptive_stats <- renderPrint({
      var_data <- sovi_data[[input$descriptive_variable]]
      
      if(input$descriptive_group == "none") {
        summary(var_data)
      } else {
        group_data <- sovi_data[[input$descriptive_group]]
        tapply(var_data, group_data, summary)
      }
    })
    
    output$descriptive_table <- DT::renderDataTable({
      var_data <- sovi_data[[input$descriptive_variable]]
      
      if(input$descriptive_group == "none") {
        desc_stats <- data.frame(
          Statistik = c("Mean", "Median", "SD", "Min", "Max", "Q1", "Q3"),
          Nilai = c(
            round(mean(var_data, na.rm = TRUE), 3),
            round(median(var_data, na.rm = TRUE), 3),
            round(sd(var_data, na.rm = TRUE), 3),
            round(min(var_data, na.rm = TRUE), 3),
            round(max(var_data, na.rm = TRUE), 3),
            round(quantile(var_data, 0.25, na.rm = TRUE), 3),
            round(quantile(var_data, 0.75, na.rm = TRUE), 3)
          )
        )
      } else {
        group_data <- sovi_data[[input$descriptive_group]]
        desc_stats <- sovi_data %>%
          group_by(!!sym(input$descriptive_group)) %>%
          summarise(
            Mean = round(mean(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            Median = round(median(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            SD = round(sd(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            Min = round(min(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            Max = round(max(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            .groups = 'drop'
          )
      }
      
      DT::datatable(
        desc_stats,
        options = list(
          pageLength = 10,
          dom = 't'
        ),
        caption = paste("Statistik Deskriptif", input$descriptive_variable)
      )
    })
    
    output$descriptive_interpretation <- renderUI({
      var_data <- sovi_data[[input$descriptive_variable]]
      mean_val <- mean(var_data, na.rm = TRUE)
      median_val <- median(var_data, na.rm = TRUE)
      sd_val <- sd(var_data, na.rm = TRUE)
      cv <- sd_val / mean_val * 100
      
      interpretation <- paste0(
        "Analisis deskriptif variabel ", input$descriptive_variable, " menunjukkan rata-rata sebesar ", round(mean_val, 3),
        " dengan median ", round(median_val, 3), ". ",
        "Standar deviasi ", round(sd_val, 3), " mengindikasikan variabilitas data yang ",
        if(cv < 15) "rendah" else if(cv < 30) "sedang" else "tinggi",
        " (CV = ", round(cv, 1), "%). ",
        if(abs(mean_val - median_val) / sd_val < 0.5) {
          "Distribusi data relatif simetris karena mean dan median hampir sama."
        } else if(mean_val > median_val) {
          "Distribusi data condong ke kanan (positively skewed) karena mean > median."
        } else {
          "Distribusi data condong ke kiri (negatively skewed) karena mean < median."
        }
      )
      
      if(input$descriptive_group != "none") {
        interpretation <- paste0(interpretation, " Perbandingan antar kelompok menunjukkan variasi yang dapat dianalisis lebih lanjut dengan uji statistik sesuai materi ujian.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Visualization plot
  output$visualization_plot <- renderPlotly({
    req(input$plot_variable)
    
    var_data <- sovi_data[[input$plot_variable]]
    
    if(input$plot_type == "histogram") {
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
        labs(title = paste("Histogram", input$plot_variable), x = input$plot_variable, y = "Frekuensi") +
        theme_minimal()
    } else if(input$plot_type == "boxplot") {
      p <- ggplot(data.frame(x = var_data), aes(y = x)) +
        geom_boxplot(fill = colors[2], alpha = 0.7, color = colors[1]) +
        labs(title = paste("Boxplot", input$plot_variable), y = input$plot_variable) +
        theme_minimal()
    } else if(input$plot_type == "density") {
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_density(fill = colors[3], alpha = 0.7, color = colors[1]) +
        labs(title = paste("Density Plot", input$plot_variable), x = input$plot_variable, y = "Density") +
        theme_minimal()
    }
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$plot_stats <- renderPrint({
    req(input$plot_variable)
    summary(sovi_data[[input$plot_variable]])
  })
  
  output$visualization_interpretation <- renderUI({
    req(input$plot_variable, input$plot_type)
    
    var_data <- sovi_data[[input$plot_variable]]
    
    plot_desc <- switch(input$plot_type,
                        "histogram" = "Histogram menunjukkan distribusi frekuensi data",
                        "boxplot" = "Boxplot menampilkan median, kuartil, dan outlier",
                        "density" = "Density plot menggambarkan estimasi distribusi probabilitas"
    )
    
    # Detect outliers using IQR method
    Q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(var_data, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    outliers <- sum(var_data < (Q1 - 1.5*IQR) | var_data > (Q3 + 1.5*IQR), na.rm = TRUE)
    
    interpretation <- paste0(
      plot_desc, " untuk variabel ", input$plot_variable, ". ",
      "Dari visualisasi ini dapat diamati bentuk distribusi, pusat data, dan penyebaran. ",
      if(outliers > 0) {
        paste0("Terdapat ", outliers, " outlier yang terdeteksi menggunakan metode IQR. ")
      } else {
        "Tidak terdapat outlier yang signifikan. "
      },
      "Informasi ini penting untuk memilih metode analisis statistik yang tepat sesuai dengan materi yang dipelajari di STIS."
    )
    
    HTML(interpretation)
  })
  
  # Exploration map
  # Exploration map
  # ==========================================================
  # GANTI DENGAN KODE PETA EKSPLORASI YANG SUDAH DIPERBAIKI INI
  # ==========================================================
  output$exploration_map <- renderLeaflet({
    req(input$map_variable)
    
    # Gunakan data sovi_peta yang sudah digabung dan valid
    sovi_peta_valid <- sovi_peta %>% filter(!is.na(.data[[input$map_variable]]))
    
    # Ambil koordinat dari kolom geometri untuk scatter plot & heatmap
    coords <- st_coordinates(st_centroid(sovi_peta_valid$geometry))
    sovi_peta_valid$longitude <- coords[, "X"]
    sovi_peta_valid$latitude <- coords[, "Y"]
    
    var_data <- sovi_peta_valid[[input$map_variable]]
    
    map_base <- leaflet(sovi_peta_valid) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2.5, zoom = 5)
    
    if (input$map_type == "scatter") {
      pal <- colorNumeric(
        palette = colors,
        domain = var_data
      )
      
      map_base %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = 5,
          fillColor = ~pal(var_data),
          color = "white",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~lapply(paste("<strong>", nmkab, "</strong><br/>",
                                input$map_variable, ": ", round(var_data, 2)), HTML)
        ) %>%
        addLegend(
          pal = pal,
          values = var_data,
          title = input$map_variable,
          position = "bottomright"
        )
    } else { # heatmap
      map_base %>%
        addHeatmap(
          lng = ~longitude,
          lat = ~latitude,
          intensity = var_data,
          blur = 20,
          max = 0.05,
          radius = 15
        )
    }
  })
  
  output$map_interpretation <- renderUI({
    req(input$map_variable, input$map_type)
    
    var_data <- sovi_data[[input$map_variable]]
    
    map_desc <- switch(input$map_type,
                       "scatter" = "Scatter plot menampilkan setiap titik data dengan warna yang merepresentasikan nilai variabel",
                       "heatmap" = "Heat map menunjukkan konsentrasi atau densitas nilai dalam area geografis tertentu"
    )
    
    mean_val <- round(mean(var_data, na.rm = TRUE), 3)
    max_val <- round(max(var_data, na.rm = TRUE), 3)
    min_val <- round(min(var_data, na.rm = TRUE), 3)
    
    interpretation <- paste0(
      "Visualisasi peta untuk variabel ", input$map_variable, " menggunakan koordinat dari file distance.csv. ",
      map_desc, ". ",
      "Distribusi spasial menunjukkan nilai berkisar dari ", min_val, " hingga ", max_val, " dengan rata-rata ", mean_val, ". ",
      "Pola spasial yang terlihat dapat mengindikasikan adanya clustering geografis atau distribusi acak yang berguna untuk analisis spasial lanjutan sesuai dengan ketentuan ujian."
    )
    
    HTML(interpretation)
  })
  
  # Normality tests
  observeEvent(input$run_normality, {
    req(input$normality_variable)
    
    var_data <- sovi_data[[input$normality_variable]]
    
    output$normality_result <- renderPrint({
      if(input$normality_test == "shapiro") {
        if(length(var_data) <= 5000) {
          shapiro.test(var_data)
        } else {
          cat("Ukuran sampel terlalu besar untuk uji Shapiro-Wilk. Menggunakan Anderson-Darling.\n")
          ad.test(var_data)
        }
      } else if(input$normality_test == "anderson") {
        ad.test(var_data)
      } else {
        var_data_jittered <- jitter(var_data, amount = 0.01)
        ks.test(var_data_jittered, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
      }
    })
    
    output$qq_plot <- renderPlotly({
      qq_data <- data.frame(
        sample = sort(var_data),
        theoretical = qnorm(ppoints(length(var_data)))
      )
      
      p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_abline(slope = sd(var_data, na.rm = TRUE),
                    intercept = mean(var_data, na.rm = TRUE),
                    color = colors[2], size = 1) +
        labs(title = "Q-Q Plot", x = "Kuantil Teoritis", y = "Kuantil Sampel") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$normality_histogram <- renderPlotly({
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
        stat_function(fun = dnorm,
                      args = list(mean = mean(var_data, na.rm = TRUE),
                                  sd = sd(var_data, na.rm = TRUE)),
                      color = colors[2], size = 1) +
        labs(title = paste("Histogram dengan Kurva Normal -", input$normality_variable),
             x = input$normality_variable, y = "Densitas") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$normality_interpretation <- renderUI({
      test_result <- if(input$normality_test == "shapiro") {
        if(length(var_data) <= 5000) {
          shapiro.test(var_data)
        } else {
          ad.test(var_data)
        }
      } else if(input$normality_test == "anderson") {
        ad.test(var_data)
      } else {
        var_data_jittered <- jitter(var_data, amount = 0.01)
        ks.test(var_data_jittered, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
      }
      
      p_value <- test_result$p.value
      alpha <- 0.05
      
      test_name <- switch(input$normality_test,
                          "shapiro" = "Shapiro-Wilk",
                          "anderson" = "Anderson-Darling",
                          "ks" = "Kolmogorov-Smirnov")
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji ", test_name, " dengan p-value = ", round(p_value, 4), " < α = ", alpha,
               " menunjukkan bahwa kita menolak H₀. Data tidak berdistribusi normal pada tingkat signifikansi 5%. ",
               "Q-Q plot menunjukkan penyimpangan dari garis diagonal yang mengkonfirmasi hasil uji. ",
               "Untuk analisis selanjutnya, pertimbangkan menggunakan uji non-parametrik atau transformasi data sesuai dengan materi yang dipelajari.")
      } else {
        paste0("Hasil uji ", test_name, " dengan p-value = ", round(p_value, 4), " > α = ", alpha,
               " menunjukkan bahwa kita gagal menolak H₀. Data dapat dianggap berdistribusi normal pada tingkat signifikansi 5%. ",
               "Q-Q plot menunjukkan titik-titik yang relatif mengikuti garis diagonal. ",
               "Data ini memenuhi asumsi normalitas untuk uji parametrik.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Homogeneity tests
  observeEvent(input$run_homogeneity, {
    req(input$homogeneity_variable, input$homogeneity_group)
    
    var_data <- sovi_data[[input$homogeneity_variable]]
    group_data <- sovi_data[[input$homogeneity_group]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    output$homogeneity_result <- renderPrint({
      leveneTest(var_data, group_data)
    })
    
    output$homogeneity_plot <- renderPlotly({
      plot_data <- data.frame(
        variable = var_data,
        group = group_data
      )
      
      p <- ggplot(plot_data, aes(x = group, y = variable, fill = group)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = colors[1:length(unique(group_data))]) +
        labs(title = paste("Box Plot berdasarkan", input$homogeneity_group),
             x = input$homogeneity_group, y = input$homogeneity_variable) +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$homogeneity_interpretation <- renderUI({
      test_result <- leveneTest(var_data, group_data)
      p_value <- test_result$`Pr(>F)`[1]
      alpha <- 0.05
      
      group_vars <- tapply(var_data, group_data, var, na.rm = TRUE)
      max_var <- max(group_vars)
      min_var <- min(group_vars)
      var_ratio <- max_var / min_var
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji Levene dengan p-value = ", round(p_value, 4), " < α = ", alpha,
               " menunjukkan bahwa kita menolak H₀. Varians antar kelompok tidak homogen (heteroskedastisitas). ",
               "Rasio varians terbesar terhadap terkecil adalah ", round(var_ratio, 2), ". ",
               "Box plot menunjukkan perbedaan penyebaran data antar kelompok. ",
               "Untuk analisis selanjutnya, gunakan uji yang tidak mengasumsikan homogenitas varians.")
      } else {
        paste0("Hasil uji Levene dengan p-value = ", round(p_value, 4), " > α = ", alpha,
               " menunjukkan bahwa kita gagal menolak H₀. Varians antar kelompok homogen (homoskedastisitas). ",
               "Rasio varians terbesar terhadap terkecil adalah ", round(var_ratio, 2), " yang masih dalam batas wajar. ",
               "Data memenuhi asumsi homogenitas varians untuk uji parametrik seperti ANOVA dan t-test.")
      }
      
      HTML(interpretation)
    })
  })
  
  # One sample t-test
  observeEvent(input$run_onesample, {
    req(input$onesample_variable)
    
    var_data <- sovi_data[[input$onesample_variable]]
    
    output$onesample_result <- renderPrint({
      t.test(var_data, mu = input$mu_hypothesis, alternative = input$alternative_hypothesis)
    })
    
    output$onesample_plot <- renderPlotly({
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
        geom_vline(xintercept = mean(var_data, na.rm = TRUE), color = colors[2], size = 1, linetype = "dashed") +
        geom_vline(xintercept = input$mu_hypothesis, color = colors[4], size = 1, linetype = "solid") +
        labs(title = "Rata-rata Sampel vs Hipotesis",
             x = input$onesample_variable, y = "Frekuensi") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$onesample_interpretation <- renderUI({
      test_result <- t.test(var_data, mu = input$mu_hypothesis, alternative = input$alternative_hypothesis)
      p_value <- test_result$p.value
      alpha <- 0.05
      sample_mean <- mean(var_data, na.rm = TRUE)
      t_stat <- test_result$statistic
      df <- test_result$parameter
      ci <- test_result$conf.int
      
      alt_desc <- switch(input$alternative_hypothesis,
                         "two.sided" = "dua arah (≠)",
                         "greater" = "satu arah (>)",
                         "less" = "satu arah (<)")
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji t satu sampel dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Rata-rata sampel (", round(sample_mean, 4), ") berbeda signifikan dari nilai hipotesis (", input$mu_hypothesis, "). ",
               "Statistik t = ", round(t_stat, 3), " dengan df = ", df, ". ",
               "Interval kepercayaan 95%: [", round(ci[1], 4), ", ", round(ci[2], 4), "] tidak mengandung nilai hipotesis.")
      } else {
        paste0("Hasil uji t satu sampel dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Rata-rata sampel (", round(sample_mean, 4), ") tidak berbeda signifikan dari nilai hipotesis (", input$mu_hypothesis, "). ",
               "Statistik t = ", round(t_stat, 3), " dengan df = ", df, ". ",
               "Interval kepercayaan 95%: [", round(ci[1], 4), ", ", round(ci[2], 4), "] mengandung nilai hipotesis.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Two sample t-test
  observeEvent(input$run_twosample, {
    req(input$twosample_variable, input$twosample_group)
    
    var_data <- sovi_data[[input$twosample_variable]]
    group_data <- sovi_data[[input$twosample_group]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    # Check if grouping variable has exactly 2 levels
    if(length(unique(group_data)) != 2) {
      output$twosample_result <- renderPrint({
        cat("Error: Variabel pengelompokan harus memiliki tepat 2 level.\n")
        cat("Level saat ini:", paste(unique(group_data), collapse = ", "))
      })
      return()
    }
    
    output$twosample_result <- renderPrint({
      t.test(var_data ~ group_data, var.equal = input$equal_variances)
    })
    
    output$twosample_plot <- renderPlotly({
      plot_data <- data.frame(
        variable = var_data,
        group = group_data
      )
      
      p <- ggplot(plot_data, aes(x = group, y = variable, fill = group)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = colors[1:2]) +
        labs(title = paste("Perbandingan berdasarkan", input$twosample_group),
             x = input$twosample_group, y = input$twosample_variable) +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$twosample_interpretation <- renderUI({
      if(length(unique(group_data)) != 2) {
        return(HTML("Error: Variabel pengelompokan harus memiliki tepat 2 level."))
      }
      
      test_result <- t.test(var_data ~ group_data, var.equal = input$equal_variances)
      p_value <- test_result$p.value
      alpha <- 0.05
      t_stat <- test_result$statistic
      df <- test_result$parameter
      
      group_means <- tapply(var_data, group_data, mean, na.rm = TRUE)
      group_sds <- tapply(var_data, group_data, sd, na.rm = TRUE)
      
      var_assumption <- if(input$equal_variances) "dengan asumsi varians sama" else "dengan asumsi varians tidak sama (Welch)"
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji t dua sampel ", var_assumption, " menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Terdapat perbedaan signifikan antara rata-rata kedua kelompok. ",
               "Kelompok ", names(group_means)[1], ": mean = ", round(group_means[1], 4), ", SD = ", round(group_sds[1], 4), ". ",
               "Kelompok ", names(group_means)[2], ": mean = ", round(group_means[2], 4), ", SD = ", round(group_sds[2], 4), ". ",
               "Statistik t = ", round(t_stat, 3), " dengan df = ", round(df, 1), ".")
      } else {
        paste0("Hasil uji t dua sampel ", var_assumption, " menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Tidak terdapat perbedaan signifikan antara rata-rata kedua kelompok. ",
               "Kelompok ", names(group_means)[1], ": mean = ", round(group_means[1], 4), ", SD = ", round(group_sds[1], 4), ". ",
               "Kelompok ", names(group_means)[2], ": mean = ", round(group_means[2], 4), ", SD = ", round(group_sds[2], 4), ". ",
               "Statistik t = ", round(t_stat, 3), " dengan df = ", round(df, 1), ".")
      }
      
      HTML(interpretation)
    })
  })
  
  # Proportion test
  observeEvent(input$run_prop_test, {
    req(input$prop_variable, input$prop_category)
    
    var_data <- sovi_data[[input$prop_variable]]
    var_data <- var_data[!is.na(var_data)]
    
    successes <- sum(var_data == input$prop_category)
    total <- length(var_data)
    
    output$prop_test_result <- renderPrint({
      prop.test(successes, total, p = input$prop_hypothesis)
    })
    
    output$prop_test_plot <- renderPlotly({
      prop_table <- table(var_data)
      prop_df <- data.frame(
        Category = names(prop_table),
        Count = as.numeric(prop_table),
        Proportion = as.numeric(prop_table) / sum(prop_table)
      )
      
      p <- ggplot(prop_df, aes(x = Category, y = Proportion, fill = Category)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        geom_hline(yintercept = input$prop_hypothesis, color = colors[1], linetype = "dashed", size = 1) +
        scale_fill_manual(values = colors[1:length(unique(prop_df$Category))]) +
        labs(title = paste("Proporsi", input$prop_variable),
             x = "Kategori", y = "Proporsi") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$prop_test_interpretation <- renderUI({
      test_result <- prop.test(successes, total, p = input$prop_hypothesis)
      p_value <- test_result$p.value
      alpha <- 0.05
      sample_prop <- successes / total
      chi_stat <- test_result$statistic
      ci <- test_result$conf.int
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji proporsi satu sampel menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Proporsi sampel kategori '", input$prop_category, "' (", round(sample_prop, 4), ") berbeda signifikan dari proporsi hipotesis (", input$prop_hypothesis, "). ",
               "Statistik χ² = ", round(chi_stat, 3), " dengan df = 1. ",
               "Interval kepercayaan 95%: [", round(ci[1], 4), ", ", round(ci[2], 4), "] tidak mengandung proporsi hipotesis.")
      } else {
        paste0("Hasil uji proporsi satu sampel menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Proporsi sampel kategori '", input$prop_category, "' (", round(sample_prop, 4), ") tidak berbeda signifikan dari proporsi hipotesis (", input$prop_hypothesis, "). ",
               "Statistik χ² = ", round(chi_stat, 3), " dengan df = 1. ",
               "Interval kepercayaan 95%: [", round(ci[1], 4), ", ", round(ci[2], 4), "] mengandung proporsi hipotesis.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Variance test
  observeEvent(input$run_var_test, {
    req(input$var_test_variable, input$var_test_group)
    
    var_data <- sovi_data[[input$var_test_variable]]
    group_data <- sovi_data[[input$var_test_group]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    # Check if grouping variable has exactly 2 levels
    if(length(unique(group_data)) != 2) {
      output$var_test_result <- renderPrint({
        cat("Error: Variabel pengelompokan harus memiliki tepat 2 level.\n")
        cat("Level saat ini:", paste(unique(group_data), collapse = ", "))
      })
      return()
    }
    
    output$var_test_result <- renderPrint({
      var.test(var_data ~ group_data)
    })
    
    output$var_test_plot <- renderPlotly({
      group_vars <- tapply(var_data, group_data, var, na.rm = TRUE)
      var_df <- data.frame(
        Group = names(group_vars),
        Variance = as.numeric(group_vars)
      )
      
      p <- ggplot(var_df, aes(x = Group, y = Variance, fill = Group)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        scale_fill_manual(values = colors[1:2]) +
        labs(title = paste("Perbandingan Varians berdasarkan", input$var_test_group),
             x = input$var_test_group, y = "Varians") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$var_test_interpretation <- renderUI({
      if(length(unique(group_data)) != 2) {
        return(HTML("Error: Variabel pengelompokan harus memiliki tepat 2 level."))
      }
      
      test_result <- var.test(var_data ~ group_data)
      p_value <- test_result$p.value
      alpha <- 0.05
      f_stat <- test_result$statistic
      df1 <- test_result$parameter[1]
      df2 <- test_result$parameter[2]
      
      group_vars <- tapply(var_data, group_data, var, na.rm = TRUE)
      group_names <- names(group_vars)
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji F untuk kesamaan varians menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Terdapat perbedaan signifikan antara varians kedua kelompok. ",
               "Varians kelompok ", group_names[1], " = ", round(group_vars[1], 4), ". ",
               "Varians kelompok ", group_names[2], " = ", round(group_vars[2], 4), ". ",
               "Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, ").")
      } else {
        paste0("Hasil uji F untuk kesamaan varians menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Tidak terdapat perbedaan signifikan antara varians kedua kelompok. ",
               "Varians kelompok ", group_names[1], " = ", round(group_vars[1], 4), ". ",
               "Varians kelompok ", group_names[2], " = ", round(group_vars[2], 4), ". ",
               "Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, ").")
      }
      
      HTML(interpretation)
    })
  })
  
  # One-way ANOVA
  observeEvent(input$run_anova, {
    req(input$anova_variable, input$anova_group)
    
    var_data <- sovi_data[[input$anova_variable]]
    group_data <- sovi_data[[input$anova_group]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    output$anova_result <- renderPrint({
      anova_model <- aov(var_data ~ group_data)
      summary(anova_model)
    })
    
    output$anova_plot <- renderPlotly({
      group_means <- tapply(var_data, group_data, mean, na.rm = TRUE)
      means_df <- data.frame(
        Group = names(group_means),
        Mean = as.numeric(group_means)
      )
      
      p <- ggplot(means_df, aes(x = Group, y = Mean, fill = Group)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        scale_fill_manual(values = colors[1:length(unique(group_data))]) +
        labs(title = paste("Rata-rata Kelompok berdasarkan", input$anova_group),
             x = input$anova_group, y = paste("Rata-rata", input$anova_variable)) +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$posthoc_result <- renderPrint({
      anova_model <- aov(var_data ~ group_data)
      TukeyHSD(anova_model)
    })
    
    output$anova_interpretation <- renderUI({
      anova_model <- aov(var_data ~ group_data)
      anova_summary <- summary(anova_model)
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      f_stat <- anova_summary[[1]]$`F value`[1]
      df1 <- anova_summary[[1]]$Df[1]
      df2 <- anova_summary[[1]]$Df[2]
      alpha <- 0.05
      
      group_means <- tapply(var_data, group_data, mean, na.rm = TRUE)
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil ANOVA satu arah menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Terdapat perbedaan signifikan antara rata-rata kelompok pada variabel ", input$anova_variable, " berdasarkan ", input$anova_group, ". ",
               "Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, "). ",
               "Uji post-hoc Tukey HSD menunjukkan pasangan kelompok mana yang berbeda signifikan.")
      } else {
        paste0("Hasil ANOVA satu arah menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Tidak terdapat perbedaan signifikan antara rata-rata kelompok pada variabel ", input$anova_variable, " berdasarkan ", input$anova_group, ". ",
               "Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, "). ",
               "Semua kelompok memiliki rata-rata yang secara statistik sama.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Two-way ANOVA
  observeEvent(input$run_anova2, {
    req(input$anova2_variable, input$anova2_factor1, input$anova2_factor2)
    
    var_data <- sovi_data[[input$anova2_variable]]
    factor1_data <- sovi_data[[input$anova2_factor1]]
    factor2_data <- sovi_data[[input$anova2_factor2]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, factor1_data, factor2_data)
    var_data <- var_data[complete_cases]
    factor1_data <- factor1_data[complete_cases]
    factor2_data <- factor2_data[complete_cases]
    
    output$anova2_result <- renderPrint({
      if(input$include_interaction) {
        anova2_model <- aov(var_data ~ factor1_data * factor2_data)
      } else {
        anova2_model <- aov(var_data ~ factor1_data + factor2_data)
      }
      summary(anova2_model)
    })
    
    output$anova2_plot <- renderPlotly({
      plot_data <- data.frame(
        variable = var_data,
        factor1 = factor1_data,
        factor2 = factor2_data
      )
      
      # Interaction plot
      interaction_means <- plot_data %>%
        group_by(factor1, factor2) %>%
        summarise(mean_var = mean(variable, na.rm = TRUE), .groups = 'drop')
      
      p <- ggplot(interaction_means, aes(x = factor1, y = mean_var, color = factor2, group = factor2)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        scale_color_manual(values = colors[1:length(unique(factor2_data))]) +
        labs(title = paste("Plot Interaksi:", input$anova2_factor1, "x", input$anova2_factor2),
             x = input$anova2_factor1, y = paste("Rata-rata", input$anova2_variable),
             color = input$anova2_factor2) +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$posthoc2_result <- renderPrint({
      if(input$include_interaction) {
        anova2_model <- aov(var_data ~ factor1_data * factor2_data)
      } else {
        anova2_model <- aov(var_data ~ factor1_data + factor2_data)
      }
      
      cat("Post-hoc test untuk", input$anova2_factor1, ":\n")
      print(TukeyHSD(anova2_model, "factor1_data"))
      cat("\nPost-hoc test untuk", input$anova2_factor2, ":\n")
      print(TukeyHSD(anova2_model, "factor2_data"))
      
      if(input$include_interaction) {
        cat("\nPost-hoc test untuk interaksi:\n")
        print(TukeyHSD(anova2_model, "factor1_data:factor2_data"))
      }
    })
    
    output$anova2_interpretation <- renderUI({
      if(input$include_interaction) {
        anova2_model <- aov(var_data ~ factor1_data * factor2_data)
      } else {
        anova2_model <- aov(var_data ~ factor1_data + factor2_data)
      }
      
      anova2_summary <- summary(anova2_model)
      p_values <- anova2_summary[[1]]$`Pr(>F)`
      f_stats <- anova2_summary[[1]]$`F value`
      alpha <- 0.05
      
      factor1_sig <- p_values[1] < alpha
      factor2_sig <- p_values[2] < alpha
      interaction_sig <- if(input$include_interaction && length(p_values) > 2) p_values[3] < alpha else FALSE
      
      interpretation <- paste0(
        "Hasil ANOVA dua arah untuk variabel ", input$anova2_variable, ":<br>",
        "&bull; Efek utama ", input$anova2_factor1, ": F = ", round(f_stats[1], 3), ", p = ", round(p_values[1], 4),
        if(factor1_sig) " (signifikan)" else " (tidak signifikan)", "<br>",
        "&bull; Efek utama ", input$anova2_factor2, ": F = ", round(f_stats[2], 3), ", p = ", round(p_values[2], 4),
        if(factor2_sig) " (signifikan)" else " (tidak signifikan)", "<br>"
      )
      
      if(input$include_interaction) {
        interpretation <- paste0(interpretation,
                                 "&bull; Efek interaksi: F = ", round(f_stats[3], 3), ", p = ", round(p_values[3], 4),
                                 if(interaction_sig) " (signifikan)" else " (tidak signifikan)", "<br>")
      }
      
      interpretation <- paste0(interpretation, "<br>",
                               if(interaction_sig) {
                                 "Adanya interaksi signifikan menunjukkan bahwa efek satu faktor bergantung pada level faktor lainnya."
                               } else {
                                 "Tidak ada interaksi signifikan, sehingga efek kedua faktor bersifat aditif dan independen."
                               })
      
      HTML(interpretation)
    })
  })
  
  # Multiple Linear Regression
  observeEvent(input$run_regression, {
    req(input$regression_response, input$regression_predictors)
    
    # Check if we have at least 2 predictors
    if(length(input$regression_predictors) < 2) {
      showNotification("Silakan pilih minimal 2 variabel prediktor untuk analisis lengkap.", type = "warning")
    }
    
    # Create formula
    formula_str <- paste(input$regression_response, "~", paste(input$regression_predictors, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    # Fit model
    values$regression_model <- lm(formula_obj, data = sovi_data)
    
    output$regression_result <- renderPrint({
      summary(values$regression_model)
    })
    
    output$model_summary <- renderPrint({
      model <- values$regression_model
      model_summary <- summary(model)
      
      cat("Ringkasan Model Regresi:\n")
      cat("========================\n")
      cat("R-squared:", round(model_summary$r.squared, 4), "\n")
      cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
      cat("F-statistic:", round(model_summary$fstatistic[1], 4), "\n")
      cat("P-value (F-test):", format.pval(pf(model_summary$fstatistic[1],
                                              model_summary$fstatistic[2],
                                              model_summary$fstatistic[3],
                                              lower.tail = FALSE)), "\n")
      cat("Residual standard error:", round(model_summary$sigma, 4), "\n")
      cat("Degrees of freedom:", model_summary$df[2], "\n")
    })
    
    output$fitted_actual_plot <- renderPlotly({
      fitted_values <- fitted(values$regression_model)
      actual_values <- sovi_data[[input$regression_response]]
      
      plot_data <- data.frame(
        fitted = fitted_values,
        actual = actual_values
      )
      
      p <- ggplot(plot_data, aes(x = fitted, y = actual)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_abline(slope = 1, intercept = 0, color = colors[2], size = 1) +
        labs(title = "Fitted vs Actual Values", x = "Nilai Prediksi", y = "Nilai Aktual") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    # Diagnostic plots
    output$residuals_fitted <- renderPlotly({
      residuals <- residuals(values$regression_model)
      fitted_values <- fitted(values$regression_model)
      
      plot_data <- data.frame(
        fitted = fitted_values,
        residuals = residuals
      )
      
      p <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_hline(yintercept = 0, color = colors[2], size = 1) +
        geom_smooth(method = "loess", color = colors[4], se = FALSE, formula = y ~ x) +
        labs(title = "Residual vs Fitted", x = "Nilai Prediksi", y = "Residual") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$qq_residuals <- renderPlotly({
      residuals <- residuals(values$regression_model)
      
      qq_data <- data.frame(
        sample = sort(residuals),
        theoretical = qnorm(ppoints(length(residuals)))
      )
      
      p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_abline(slope = sd(residuals), intercept = mean(residuals), color = colors[2], size = 1) +
        labs(title = "Q-Q Plot Residual", x = "Kuantil Teoritis", y = "Kuantil Sampel") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$scale_location_plot <- renderPlotly({
      residuals <- residuals(values$regression_model)
      fitted_values <- fitted(values$regression_model)
      sqrt_abs_residuals <- sqrt(abs(residuals))
      
      plot_data <- data.frame(
        fitted = fitted_values,
        sqrt_abs_residuals = sqrt_abs_residuals
      )
      
      p <- ggplot(plot_data, aes(x = fitted, y = sqrt_abs_residuals)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_smooth(method = "loess", color = colors[2], se = FALSE, formula = y ~ x) +
        labs(title = "Scale-Location Plot", x = "Nilai Prediksi", y = "√|Residual|") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$leverage_plot <- renderPlotly({
      residuals <- residuals(values$regression_model)
      leverage <- hatvalues(values$regression_model)
      
      plot_data <- data.frame(
        leverage = leverage,
        residuals = residuals
      )
      
      p <- ggplot(plot_data, aes(x = leverage, y = residuals)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_hline(yintercept = 0, color = colors[2], size = 1) +
        geom_smooth(method = "loess", color = colors[4], se = FALSE, formula = y ~ x) +
        labs(title = "Residual vs Leverage", x = "Leverage", y = "Residual") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    # Assumption tests
    output$multicollinearity_test <- renderPrint({
      if(length(input$regression_predictors) >= 2) {
        tryCatch({
          vif_values <- vif(values$regression_model)
          cat("Variance Inflation Factor (VIF):\n")
          cat("================================\n")
          print(round(vif_values, 3))
          cat("\nInterpretasi VIF:\n")
          cat("VIF < 5: Tidak ada masalah multikolinearitas\n")
          cat("5 ≤ VIF < 10: Multikolinearitas sedang\n")
          cat("VIF ≥ 10: Multikolinearitas tinggi\n")
          
          if(any(vif_values >= 10)) {
            cat("\nPeringatan: Terdapat multikolinearitas tinggi!\n")
          } else if(any(vif_values >= 5)) {
            cat("\nPerhatian: Terdapat multikolinearitas sedang.\n")
          } else {
            cat("\nBaik: Tidak ada masalah multikolinearitas.\n")
          }
        }, error = function(e) {
          cat("Error dalam menghitung VIF: Model mungkin memiliki multikolinearitas sempurna.\n")
        })
      } else {
        cat("Perhitungan VIF memerlukan minimal 2 variabel prediktor.\n")
      }
    })
    
    output$durbin_watson_test <- renderPrint({
      tryCatch({
        dw_test <- durbinWatsonTest(values$regression_model)
        cat("Uji Durbin-Watson:\n")
        cat("==================\n")
        print(dw_test)
        cat("\nInterpretasi DW:\n")
        cat("DW ≈ 2: Tidak ada autokorelasi\n")
        cat("DW < 2: Autokorelasi positif\n")
        cat("DW > 2: Autokorelasi negatif\n")
        
        dw_stat <- dw_test$dw
        if(dw_stat >= 1.5 && dw_stat <= 2.5) {
          cat("\nBaik: Tidak ada autokorelasi yang signifikan.\n")
        } else {
          cat("\nPeringatan: Kemungkinan ada autokorelasi.\n")
        }
      }, error = function(e) {
        cat("Error dalam menghitung uji Durbin-Watson.\n")
      })
    })
    
    output$residual_normality_test <- renderPrint({
      residuals <- residuals(values$regression_model)
      cat("Uji Normalitas Residual:\n")
      cat("========================\n")
      
      if(length(residuals) <= 5000) {
        shapiro_test <- shapiro.test(residuals)
        cat("Shapiro-Wilk Test:\n")
        print(shapiro_test)
      } else {
        ad_test <- ad.test(residuals)
        cat("Anderson-Darling Test:\n")
        print(ad_test)
      }
      
      cat("\nInterpretasi:\n")
      cat("H0: Residual berdistribusi normal\n")
      cat("H1: Residual tidak berdistribusi normal\n")
      cat("Jika p-value < 0.05, tolak H0 (residual tidak normal)\n")
    })
    
    output$homoscedasticity_test <- renderPrint({
      tryCatch({
        # Use ncvTest from car package
        bp_test <- ncvTest(values$regression_model)
        cat("Uji Non-constant Variance (Homoskedastisitas):\n")
        cat("==============================================\n")
        print(bp_test)
        cat("\nInterpretasi:\n")
        cat("H0: Varians residual konstan (homoskedastisitas)\n")
        cat("H1: Varians residual tidak konstan (heteroskedastisitas)\n")
        cat("Jika p-value < 0.05, tolak H0 (ada heteroskedastisitas)\n")
        
        if(bp_test$p < 0.05) {
          cat("\nPeringatan: Terdapat heteroskedastisitas!\n")
        } else {
          cat("\nBaik: Asumsi homoskedastisitas terpenuhi.\n")
        }
      }, error = function(e) {
        cat("Error dalam menghitung uji homoskedastisitas.\n")
        # Alternative: correlation test
        residuals <- residuals(values$regression_model)
        fitted_vals <- fitted(values$regression_model)
        cor_test <- cor.test(abs(residuals), fitted_vals)
        
        cat("Korelasi |residual| vs fitted values:\n")
        print(cor_test)
        cat("Jika korelasi signifikan, kemungkinan ada heteroskedastisitas.\n")
      })
    })
    
    output$regression_interpretation <- renderUI({
      model <- values$regression_model
      summary_model <- summary(model)
      r_squared <- summary_model$r.squared
      adj_r_squared <- summary_model$adj.r.squared
      f_stat <- summary_model$fstatistic[1]
      f_p_value <- pf(f_stat, summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE)
      
      # Count significant predictors
      coef_p_values <- summary_model$coefficients[, "Pr(>|t|)"]
      sig_predictors <- sum(coef_p_values[-1] < 0.05)  # Exclude intercept
      total_predictors <- length(coef_p_values) - 1
      
      interpretation <- paste0(
        "Model regresi linear berganda menjelaskan ", round(r_squared * 100, 2), "% variasi dalam ", input$regression_response,
        " (R² = ", round(r_squared, 3), ", Adjusted R² = ", round(adj_r_squared, 3), ").<br>",
        "Uji F-statistik keseluruhan (F = ", round(f_stat, 3), ", p = ", format.pval(f_p_value), ") menunjukkan bahwa model ini secara statistik ",
        if(f_p_value < 0.05) "signifikan dalam memprediksi variabel respons." else "tidak signifikan.", "<br>",
        "Dari ", total_predictors, " variabel prediktor, ", sig_predictors, " diantaranya memiliki pengaruh yang signifikan secara statistik (p < 0.05). ",
        "Plot Fitted vs Actual menunjukkan seberapa baik prediksi model (titik-titik) mendekati garis diagonal (nilai aktual)."
      )
      
      HTML(interpretation)
    })
    
    output$diagnostic_interpretation <- renderUI({
      HTML(
        "Plot diagnostik digunakan untuk memverifikasi asumsi regresi linear:<br>
        <ul>
          <li><b>Residual vs Fitted:</b> Plot ini memeriksa asumsi linearitas dan homoskedastisitas. Idealnya, titik-titik tersebar acak di sekitar garis horizontal nol tanpa pola yang jelas. Pola seperti corong menunjukkan heteroskedastisitas. Garis merah yang datar menunjukkan linearitas terpenuhi.</li>
          <li><b>Q-Q Plot Residual:</b> Plot ini memeriksa apakah residual berdistribusi normal. Idealnya, titik-titik mengikuti garis diagonal. Penyimpangan signifikan dari garis ini menunjukkan bahwa residual tidak normal.</li>
          <li><b>Scale-Location Plot:</b> Plot ini juga memeriksa homoskedastisitas (kesamaan varians). Idealnya, garis merah horizontal dan titik-titik tersebar secara acak. Tren pada garis merah menunjukkan heteroskedastisitas.</li>
          <li><b>Residual vs Leverage:</b> Plot ini membantu mengidentifikasi outlier dan titik berpengaruh (influential points). Titik dengan leverage tinggi (jauh ke kanan) dan residual besar (jauh dari nol) berpotensi menjadi titik berpengaruh yang dapat mengubah hasil model.</li>
        </ul>
        Interpretasi plot-plot ini penting untuk memastikan validitas model regresi sesuai dengan ketentuan ujian."
      )
    })
    
    output$assumption_interpretation <- renderUI({
      model <- values$regression_model
      
      # Multicollinearity
      vif_text <- if(length(input$regression_predictors) >= 2) {
        vif_vals <- tryCatch(vif(model), error = function(e) NULL)
        if(!is.null(vif_vals)) {
          if(any(vif_vals >= 10)) "tinggi (VIF ≥ 10)" else if(any(vif_vals >= 5)) "sedang (5 ≤ VIF < 10)" else "rendah (VIF < 5)"
        } else "tidak dapat dihitung"
      } else "tidak diuji (perlu >1 prediktor)"
      
      # Autocorrelation
      dw_test <- tryCatch(durbinWatsonTest(model), error = function(e) NULL)
      dw_text <- if(!is.null(dw_test)) {
        dw_stat <- dw_test$dw
        if(dw_stat >= 1.5 && dw_stat <= 2.5) "tidak signifikan (DW ≈ 2)" else "signifikan (DW jauh dari 2)"
      } else "tidak dapat dihitung"
      
      # Normality of residuals
      residuals <- residuals(model)
      norm_test <- if(length(residuals) <= 5000) shapiro.test(residuals) else ad.test(residuals)
      norm_text <- if(norm_test$p.value < 0.05) "tidak terpenuhi (p < 0.05)" else "terpenuhi (p ≥ 0.05)"
      
      # Homoscedasticity
      bp_test <- tryCatch(ncvTest(model), error = function(e) NULL)
      homo_text <- if(!is.null(bp_test)) {
        if(bp_test$p < 0.05) "tidak terpenuhi (p < 0.05, heteroskedastisitas)" else "terpenuhi (p ≥ 0.05, homoskedastisitas)"
      } else "tidak dapat dihitung"
      
      HTML(
        paste0(
          "Ringkasan uji asumsi regresi:<br>
          <ul>
            <li><b>Multikolinearitas (VIF):</b> Tingkat multikolinearitas terdeteksi <b>", vif_text, "</b>.</li>
            <li><b>Autokorelasi (Durbin-Watson):</b> Kehadiran autokorelasi <b>", dw_text, "</b>.</li>
            <li><b>Normalitas Residual (Shapiro-Wilk/Anderson-Darling):</b> Asumsi normalitas residual <b>", norm_text, "</b>.</li>
            <li><b>Homoskedastisitas (NCV Test):</b> Asumsi homoskedastisitas <b>", homo_text, "</b>.</li>
          </ul>
          Berdasarkan hasil ini, validitas model regresi dapat dievaluasi. Pelanggaran asumsi mungkin memerlukan transformasi data atau penggunaan metode regresi yang lebih robust."
        )
      )
    })
  })
  
  # Cluster Analysis
  observeEvent(input$run_clustering, {
    req(input$cluster_variables, length(input$cluster_variables) >= 2)
    
    # Prepare data for clustering
    cluster_data <- sovi_data[, input$cluster_variables, drop = FALSE]
    cluster_data <- na.omit(cluster_data)
    
    # Scale the data
    cluster_data_scaled <- scale(cluster_data)
    
    # Perform clustering based on selected method
    if(input$cluster_method == "kmeans") {
      cluster_result <- kmeans(cluster_data_scaled, centers = input$n_clusters, nstart = 25)
      clusters <- cluster_result$cluster
      
      output$cluster_summary <- renderPrint({
        cat("K-MEANS CLUSTERING RESULTS\n")
        cat("==========================\n")
        cat("Number of clusters:", input$n_clusters, "\n")
        cat("Total within-cluster sum of squares:", round(cluster_result$tot.withinss, 2), "\n")
        cat("Between-cluster sum of squares:", round(cluster_result$betweenss, 2), "\n")
        cat("Total sum of squares:", round(cluster_result$totss, 2), "\n")
        cat("Between SS / Total SS ratio:", round(cluster_result$betweenss/cluster_result$totss * 100, 1), "%\n\n")
        
        cat("Cluster sizes:\n")
        print(table(clusters))
        
        cat("\nCluster centers (scaled):\n")
        print(round(cluster_result$centers, 3))
      })
      
    } else if(input$cluster_method == "hierarchical") {
      # Use proper distance matrix if available, otherwise calculate euclidean
      if(ncol(distance_matrix) == nrow(cluster_data) && nrow(distance_matrix) == nrow(cluster_data)) {
        # Subset distance matrix to match cluster_data rows
        dist_subset <- as.dist(distance_matrix[1:nrow(cluster_data), 1:nrow(cluster_data)])
      } else {
        dist_subset <- dist(cluster_data_scaled)
      }
      
      cluster_result <- hclust(dist_subset, method = input$linkage_method)
      n_clusters_hier <- ifelse(is.null(input$n_clusters), 3, input$n_clusters)
      clusters <- cutree(cluster_result, k = n_clusters_hier)
      
      output$cluster_summary <- renderPrint({
        cat("HIERARCHICAL CLUSTERING RESULTS\n")
        cat("===============================\n")
        cat("Linkage method:", input$linkage_method, "\n")
        cat("Number of clusters:", n_clusters_hier, "\n\n")
        
        cat("Cluster sizes:\n")
        print(table(clusters))
      })
      
      # Dendrogram
      output$dendrogram_plot <- renderPlotly({
        dend_data <- dendro_data(cluster_result)
        
        p <- ggplot() +
          geom_segment(data = dend_data$segments, 
                       aes(x = x, y = y, xend = xend, yend = yend), 
                       color = colors[1]) +
          geom_hline(yintercept = sort(cluster_result$height, decreasing = TRUE)[n_clusters_hier-1], 
                     color = colors[7], linetype = "dashed") +
          labs(title = "Dendrogram dengan Cut Line", x = "Observasi", y = "Height") +
          theme_minimal()
        
        ggplotly(p) %>% config(displayModeBar = FALSE)
      })
      
    } else if(input$cluster_method == "pam") {
      cluster_result <- pam(cluster_data_scaled, k = input$n_clusters)
      clusters <- cluster_result$clustering
      
      output$cluster_summary <- renderPrint({
        cat("PAM (K-MEDOIDS) CLUSTERING RESULTS\n")
        cat("==================================\n")
        cat("Number of clusters:", input$n_clusters, "\n")
        cat("Average silhouette width:", round(cluster_result$silinfo$avg.width, 3), "\n\n")
        
        cat("Cluster sizes:\n")
        print(table(clusters))
        
        cat("\nMedoids:\n")
        print(cluster_result$medoids)
      })
    }
    
    # Cluster visualization
    output$cluster_plot <- renderPlotly({
      if(length(input$cluster_variables) >= 2) {
        plot_data <- data.frame(
          x = cluster_data[, 1],
          y = cluster_data[, 2],
          Cluster = as.factor(clusters)
        )
        
        p <- ggplot(plot_data, aes(x = x, y = y, color = Cluster)) +
          geom_point(size = 3, alpha = 0.7) +
          scale_color_manual(values = colors[1:length(unique(clusters))]) +
          labs(title = paste("Cluster Plot:", input$cluster_variables[1], "vs", input$cluster_variables[2]),
               x = input$cluster_variables[1], y = input$cluster_variables[2]) +
          theme_minimal()
        
        ggplotly(p) %>% config(displayModeBar = FALSE)
      }
    })
    
    # Cluster table
    output$cluster_table <- DT::renderDataTable({
      cluster_summary_table <- cluster_data %>%
        mutate(Cluster = clusters) %>%
        group_by(Cluster) %>%
        summarise_all(list(Mean = ~ round(mean(., na.rm = TRUE), 3)), .groups = 'drop')
      
      DT::datatable(
        cluster_summary_table,
        options = list(pageLength = 10, scrollX = TRUE),
        caption = "Ringkasan Statistik per Cluster"
      )
    })
    
    # Cluster interpretation
    output$cluster_interpretation <- renderUI({
      n_clusters <- length(unique(clusters))
      cluster_sizes <- table(clusters)
      largest_cluster <- which.max(cluster_sizes)
      smallest_cluster <- which.min(cluster_sizes)
      
      method_desc <- switch(input$cluster_method,
                           "kmeans" = "K-Means menggunakan algoritma centroid-based clustering",
                           "hierarchical" = "Hierarchical clustering menggunakan pendekatan agglomerative",
                           "pam" = "PAM (Partitioning Around Medoids) menggunakan medoid-based clustering")
      
      interpretation <- paste0(
        "<strong>Hasil Analisis Cluster SOVI:</strong><br><br>",
        
        method_desc, " menghasilkan ", n_clusters, " cluster dengan karakteristik yang berbeda. ",
        "Cluster terbesar (Cluster ", largest_cluster, ") memiliki ", max(cluster_sizes), " observasi, ",
        "sedangkan cluster terkecil (Cluster ", smallest_cluster, ") memiliki ", min(cluster_sizes), " observasi.<br><br>",
        
        "<strong>Interpretasi Metodologis:</strong><br>",
        "• Variabel yang digunakan: ", paste(input$cluster_variables, collapse = ", "), "<br>",
        "• Metode standardisasi: Z-score standardization untuk menghindari bias skala<br>",
        if(input$cluster_method == "hierarchical") "• Distance matrix: Menggunakan matriks jarak sesuai ketentuan<br>" else "",
        "• Hasil clustering dapat digunakan untuk segmentasi wilayah berdasarkan kerentanan sosial<br><br>",
        
        "Analisis ini memungkinkan identifikasi pola kerentanan sosial yang dapat membantu dalam perumusan kebijakan regional. ",
        "Setiap cluster merepresentasikan wilayah dengan karakteristik SOVI yang serupa dan dapat menjadi dasar untuk intervensi targeted sesuai dengan metodologi yang dipelajari di STIS."
      )
      
      HTML(interpretation)
    })
  })
  
  # =====================================================
  # DOWNLOAD HANDLERS
  # =====================================================
  
  # Helper function to create R Markdown content
  # GANTI SEMUA BLOK DOWNLOAD HANDLER DENGAN INI
  
  # =====================================================
  # DOWNLOAD HANDLERS (VERSI FUNGSIONAL)
  # =====================================================
  
  # Helper function untuk membuat konten laporan R Markdown
  create_report_content <- function(tab_name) {
    title <- paste("Laporan Analisis SOVI -", tools::toTitleCase(tab_name))
    
    # Konten Rmd dinamis (ini adalah contoh sederhana)
    content <- paste0(
      '---\n',
      'title: "', title, '"\n',
      'date: "', format(Sys.Date(), "%d %B %Y"), '"\n',
      'output: { word_document: default, pdf_document: default }\n',
      '---\n\n',
      '```{r setup, include=FALSE}\n',
      'knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)\n',
      'library(ggplot2); library(dplyr); library(readr)\n',
      'sovi_data <- read_csv("[https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv](https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv)")\n',
      'colors <- c("#5E7892", "#A7B7C6", "#F3EFDF", "#BDCFAA", "#8E9E83")\n',
      '```\n\n',
      '## Ringkasan Laporan\n\n',
      'Dokumen ini berisi hasil analisis dari tab `', tab_name, '` pada Dashboard Analisis SOVI.\n\n',
      '```{r plot, fig.cap="Contoh Plot Utama dari Tab ', tab_name, '"}\n',
      'print(ggplot(sovi_data, aes(x = POVERTY, y = CHILDREN)) + geom_point(color = colors[1]) + theme_minimal() + labs(title = "Contoh Plot: Kemiskinan vs Jumlah Anak"))\n',
      '```\n\n',
      '```{r summary, results="asis"}\n',
      'cat("### Statistik Ringkasan\\n")\n',
      'print(knitr::kable(summary(sovi_data[, c("POVERTY", "CHILDREN", "ELDERLY")]), caption = "Statistik Ringkasan Variabel Terpilih"))\n',
      '```\n'
    )
    return(content)
  }
  
  # Generator download handler generik
  generate_download_handler <- function(tab_name, format_type) {
    downloadHandler(
      filename = function() {
        paste0("laporan_", tab_name, "_", format(Sys.time(), "%Y%m%d"), 
               if(format_type == "all") ".zip" else if(format_type == "jpg") ".jpg" else if(format_type == "pdf") ".pdf" else ".docx")
      },
      content = function(file) {
        temp_dir <- tempdir()
        
        # Definisikan path file sementara
        report_rmd <- file.path(temp_dir, "report.Rmd")
        report_pdf <- file.path(temp_dir, "report.pdf")
        report_word <- file.path(temp_dir, "report.docx")
        plot_jpg <- file.path(temp_dir, "plot.jpg")
        
        # Buat plot contoh untuk JPG
        p <- ggplot(sovi_data, aes(x = POVERTY)) + geom_histogram(bins = 30, fill = colors[1], alpha = 0.7) + theme_minimal() + labs(title = paste("Plot Utama Tab", tools::toTitleCase(tab_name)))
        ggsave(plot_jpg, plot = p, device = "jpeg", width = 8, height = 6)
        
        if (format_type == "jpg") {
          file.copy(plot_jpg, file)
          return()
        }
        
        # Buat konten Rmd
        report_content <- create_report_content(tab_name)
        writeLines(report_content, report_rmd)
        
        # Render dokumen jika diperlukan
        if (format_type == "pdf" || format_type == "all") {
          rmarkdown::render(report_rmd, output_format = "pdf_document", output_file = report_pdf, quiet = TRUE)
          if (format_type == "pdf") {
            file.copy(report_pdf, file)
            return()
          }
        }
        if (format_type == "word" || format_type == "all") {
          rmarkdown::render(report_rmd, output_format = "word_document", output_file = report_word, quiet = TRUE)
          if (format_type == "word") {
            file.copy(report_word, file)
            return()
          }
        }
        
        # Buat file ZIP untuk "all"
        if (format_type == "all") {
          zip::zip(
            zipfile = file,
            files = c(plot_jpg, report_pdf, report_word),
            root = temp_dir
          )
        }
      }
    )
  }
  
  # Terapkan handler ke setiap tombol secara dinamis
  tabs <- c("beranda", "manajemen", "eksplorasi", "asumsi", "inferensia", "regresi")
  formats <- c("jpg", "pdf", "word", "all")
  
  for (tab in tabs) {
    for (fmt in formats) {
      handler_name <- paste0("download_", tab, "_", fmt)
      output[[handler_name]] <- generate_download_handler(tab, fmt)
    }
  }
}

# Run the app
shinyApp(ui = ui, server = server)