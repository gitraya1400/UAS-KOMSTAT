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


# Load SOVI data sesuai URL yang diberikan
sovi_data <- read_csv("D:/STIS SEM 4/KOMSTAT/Komstat UAS/sovi_data.csv")
distance_data <- read_csv("D:/STIS SEM 4/KOMSTAT/Komstat UAS/distance.csv")

# GANTI "NAMA_FILE_SHP_ANDA.shp" dengan nama file Anda yang sebenarnya
peta_kabupaten <- st_read("D:/STIS SEM 3/SIG/UTS/UTS-20241114T023840Z-001/UTS/Administrasi_Kabupaten.shp")

# Gabungkan kdprov dan kdkab untuk membuat DISTRICTCODE yang cocok
peta_kabupaten <- peta_kabupaten %>%
  mutate(
    # Pastikan keduanya numerik untuk menghindari error
    kdprov = as.numeric(as.character(kdprov)),
    kdkab = as.numeric(as.character(kdkab)),
    
    # Buat DISTRICTCODE: gabungkan kdprov dengan kdkab (yang diformat 2 digit)
    # Contoh: kdprov=11, kdkab=1 -> paste0("11", "01") -> "1101"
    DISTRICTCODE = as.numeric(paste0(kdprov, sprintf("%02d", kdkab)))
  )

# Gabungkan data sovi_data Anda dengan data peta yang sudah diperbaiki
sovi_peta <- left_join(peta_kabupaten, sovi_data, by = "DISTRICTCODE")
# Periksa struktur data
print("Struktur SOVI Data:")
print(names(sovi_data))
print("Struktur Distance Data:")
print(names(distance_data))

# Gunakan koordinat dari distance_data untuk peta
# DENGAN KODE INI
# PERBAIKAN: Gunakan nama kolom yang benar (LONGITUDE, LATITUDE) dari distance.csv
# ✅ Kode Perbaikan yang Benar

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

# Color palette sesuai yang diberikan
formal_colors <- c("#2C3E50", "#2980B9", "#ECF0F1", "#27AE60", "#F1C40F")
colors <- formal_colors

# Custom CSS
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
}
.btn-primary {
  background-color: ", colors[2], " !important;
  border-color: ", colors[2], " !important;
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
        
        # Metric Cards
        fluidRow(
          column(2,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("total_observations")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Total Observasi")
                 )
          ),
          column(2,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[4], " 0%, ", colors[5], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("total_variables")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Total Variabel")
                 )
          ),
          column(2,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[2], " 0%, ", colors[3], " 100%); color: ", colors[1], "; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("avg_poverty_rate")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Rata-rata Kemiskinan")
                 )
          ),
          column(2,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[5], " 0%, ", colors[4], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("avg_children")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Rata-rata Anak (CHILDREN)")
                 )
          ),
          column(2,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[4], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("avg_elderly")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Rata-rata Lansia (ELDERLY)")
                 )
          ),
          column(2,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[2], " 0%, ", colors[5], " 100%); color: ", colors[1], "; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("avg_lowedu")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Rata-rata Pendidikan Rendah (LOWEDU)")
                 )
          )
        ),
        
        # Main content
        fluidRow(
          column(8,
                 box(
                   title = "Distribusi Data SOVI", status = "primary", solidHeader = TRUE, width = NULL,
                   plotlyOutput("sovi_distribution", height = "350px")
                 )
          ),
          column(4,
                 box(
                   title = "Statistik Ringkasan", status = "info", solidHeader = TRUE, width = NULL,
                   verbatimTextOutput("summary_stats")
                 )
          )
        ),
        
        # Peta Distribusi
        fluidRow(
          column(12,
                 box(
                   title = "Peta Distribusi SOVI", status = "primary", solidHeader = TRUE, width = NULL,
                   leafletOutput("beranda_map", height = "400px")
                 )
          )
        ),
        
        # Interpretasi
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; border-left: 4px solid ", colors[1], ";"),
                   h4("Interpretasi Dashboard Beranda", style = paste0("color: ", colors[1], ";")),
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
        
        fluidRow(
          column(12,
                 box(
                   title = "Alat Manajemen Data", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Ringkasan Data",
                              br(),
                              fluidRow(
                                column(6,
                                       h4("Ringkasan Dataset", style = paste0("color: ", colors[1], ";")),
                                       verbatimTextOutput("data_summary")
                                ),
                                column(6,
                                       h4("Struktur Data", style = paste0("color: ", colors[1], ";")),
                                       verbatimTextOutput("data_structure")
                                )
                              ),
                              
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                h5("Interpretasi Ringkasan Data", style = paste0("color: ", colors[1], ";")),
                                uiOutput("data_summary_interpretation")
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
                     
                     tabPanel("Analisis Klaster",
                              br(),
                              fluidRow(
                                column(4,
                                  selectInput("cluster_vars", "Pilih Variabel (minimal 2):", choices = NULL, multiple = TRUE),
                                  selectInput("cluster_method", "Metode Klasterisasi:", choices = c("K-Means" = "kmeans", "Hierarchical" = "hclust")),
                                  numericInput("n_cluster", "Jumlah Klaster:", value = 3, min = 2, max = 10),
                                  actionButton("run_cluster", "Jalankan Klasterisasi", class = "btn-primary", style = "width:100%;")
                                ),
                                column(8,
                                  h4("Plot Klaster", style = paste0("color: ", colors[1], ";")),
                                  plotlyOutput("cluster_plot", height = "350px"),
                                  h4("Ringkasan Klaster", style = paste0("color: ", colors[1], ";")),
                                  DT::dataTableOutput("cluster_summary"),
                                  h4("Silhouette Score", style = paste0("color: ", colors[1], ";")),
                                  plotlyOutput("silhouette_plot", height = "250px")
                                )
                              ),
                              div(style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Klaster", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("cluster_interpretation")
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
    updateSelectInput(session, "cluster_vars", choices = numeric_vars)
  })
  
  # Update proportion category choices
  observe({
    req(input$prop_variable)
    categories <- unique(sovi_data[[input$prop_variable]])
    categories <- categories[!is.na(categories)]
    updateSelectInput(session, "prop_category", choices = categories)
  })
  
  # Home tab outputs
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
    round(mean(sovi_data$POVERTY, na.rm = TRUE), 2)
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
  
  output$summary_stats <- renderPrint({
    numeric_data <- select_if(sovi_data, is.numeric)
    if(ncol(numeric_data) > 0) {
      summary(numeric_data[1:min(5, ncol(numeric_data))])
    } else {
      "Tidak ada variabel numerik tersedia"
    }
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
  
  # Beranda interpretation
  output$beranda_interpretation <- renderUI({
    total_obs <- nrow(sovi_data)
    total_vars <- ncol(sovi_data)
    completeness <- round(sum(complete.cases(sovi_data))/nrow(sovi_data) * 100, 1)
    avg_poverty <- round(mean(sovi_data$POVERTY, na.rm = TRUE), 2)
    
    interpretation <- paste0(
      "Dashboard ini menyediakan analisis komprehensif untuk dataset SOVI dengan ", total_obs, " observasi dan ", total_vars, " variabel. ",
      "Tingkat kelengkapan data sebesar ", completeness, "% menunjukkan kualitas data yang ",
      if(completeness >= 90) "sangat baik" else if(completeness >= 80) "baik" else "perlu perhatian", ". ",
      "Dataset ini berisi informasi tentang Social Vulnerability Index yang mengukur kerentanan sosial berbagai wilayah. ",
      "Rata-rata tingkat kemiskinan adalah ", avg_poverty, "%. ",
      "Peta distribusi menunjukkan sebaran geografis data yang dapat membantu dalam analisis spasial. ",
      "Dashboard ini dikembangkan untuk ujian Statistika Terapan STIS 2025 dengan mengikuti semua ketentuan yang diberikan."
    )
    
    HTML(interpretation)
  })
  
  # Data management outputs
  output$data_summary <- DT::renderDataTable({
    num_vars <- select_if(sovi_data, is.numeric)
    desc <- data.frame(
      Variabel = names(num_vars),
      Mean = sapply(num_vars, function(x) round(mean(x, na.rm=TRUE),2)),
      Median = sapply(num_vars, function(x) round(median(x, na.rm=TRUE),2)),
      SD = sapply(num_vars, function(x) round(sd(x, na.rm=TRUE),2)),
      Min = sapply(num_vars, function(x) round(min(x, na.rm=TRUE),2)),
      Max = sapply(num_vars, function(x) round(max(x, na.rm=TRUE),2))
    )
    DT::datatable(desc, options = list(pageLength = 8, dom = 't'), caption = "Ringkasan Statistik Variabel Numerik")
  })
  
  output$data_structure <- renderPrint({
    str(sovi_data)
  })
  
  output$data_summary_interpretation <- renderUI({
    numeric_vars <- sum(sapply(sovi_data, is.numeric))
    char_vars <- sum(sapply(sovi_data, is.character))
    factor_vars <- sum(sapply(sovi_data, is.factor))
    
    interpretation <- paste0(
      "Dataset SOVI terdiri dari ", numeric_vars, " variabel numerik, ", char_vars, " variabel karakter, dan ", factor_vars, " variabel faktor. ",
      "Struktur data menunjukkan bahwa sebagian besar variabel adalah numerik yang cocok untuk analisis statistik. ",
      "Ringkasan statistik memberikan gambaran distribusi setiap variabel termasuk nilai minimum, maksimum, median, dan kuartil. ",
      "Data ini siap untuk digunakan dalam berbagai analisis statistik yang tersedia di dashboard sesuai dengan ketentuan ujian STIS 2025."
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
  
  # Cluster analysis
  observeEvent(input$run_cluster, {
    req(input$cluster_vars, length(input$cluster_vars) >= 2)
    data_cluster <- sovi_data[, input$cluster_vars]
    data_cluster <- na.omit(data_cluster)
    dist_mat <- dist(scale(data_cluster))
    nclust <- input$n_cluster
    method <- input$cluster_method
    if (method == "kmeans") {
      set.seed(123)
      clust <- kmeans(scale(data_cluster), centers = nclust, nstart = 25)
      cluster_assign <- clust$cluster
      sil <- silhouette(cluster_assign, dist_mat)
    } else {
      hc <- hclust(dist_mat, method = "ward.D2")
      cluster_assign <- cutree(hc, k = nclust)
      sil <- silhouette(cluster_assign, dist_mat)
    }
    sovi_data$CLUSTER <- NA
    sovi_data$CLUSTER[as.numeric(rownames(data_cluster))] <- cluster_assign
    values$cluster_assign <- cluster_assign
    values$silhouette <- sil
    values$data_cluster <- data_cluster
    # Summary table
    output$cluster_summary <- DT::renderDataTable({
      tab <- data.frame(Cluster = 1:nclust,
                        Size = as.numeric(table(cluster_assign)),
                        Ave.Sil.Width = tapply(sil[,3], cluster_assign, mean))
      DT::datatable(tab, options = list(dom = 't'), caption = "Ringkasan Klaster")
    })
    # Cluster plot (PCA 2D)
    output$cluster_plot <- renderPlotly({
      pca <- prcomp(scale(data_cluster))
      df <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], Cluster = factor(cluster_assign))
      p <- ggplot(df, aes(x = PC1, y = PC2, color = Cluster)) +
        geom_point(size = 2, alpha = 0.8) +
        scale_color_manual(values = colors[1:nclust]) +
        theme_minimal() +
        labs(title = "Visualisasi Klaster (PCA)")
      ggplotly(p)
    })
    # Silhouette plot
    output$silhouette_plot <- renderPlotly({
      sil_df <- data.frame(cluster = factor(sil[,1]), sil_width = sil[,3])
      p <- ggplot(sil_df, aes(x = cluster, y = sil_width, fill = cluster)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = colors[1:nclust]) +
        theme_minimal() +
        labs(title = "Silhouette Width per Cluster", x = "Cluster", y = "Silhouette Width")
      ggplotly(p)
    })
    # Interpretasi
    output$cluster_interpretation <- renderUI({
      avg_sil <- mean(sil[,3])
      HTML(paste0("Rata-rata silhouette width: ", round(avg_sil, 3), ". Nilai mendekati 1 menandakan klaster yang baik. Gunakan hasil klaster ini untuk analisis lanjutan."))
    })
  })
  
  # Average children
  output$avg_children <- renderText({ round(mean(sovi_data$CHILDREN, na.rm=TRUE),2) })
  # Average elderly
  output$avg_elderly <- renderText({ round(mean(sovi_data$ELDERLY, na.rm=TRUE),2) })
  # Average lowedu
  output$avg_lowedu <- renderText({ round(mean(sovi_data$LOWEDU, na.rm=TRUE),2) })
}

# Run the app
shinyApp(ui = ui, server = server)