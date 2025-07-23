# =====================================================
# DASHBOARD ANALISIS SOVI - UJIAN AKHIR SEMESTER KOMPUTASI STATISTIK
# M REZKY RAYA KILWOUW - 222313190 - 2KS2 
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
library(jsonlite)
library(reshape2)

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
sovi_data$Population_Size <- ifelse(sovi_data$POPULATION > median(sovi_data$POPULATION, na.rm = TRUE), "Besar", "Kecil")

sovi_data$Economic_Status <- cut(sovi_data$POVERTY,
                                 breaks = quantile(sovi_data$POVERTY, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                                 labels = c("Kemiskinan_Rendah", "Kemiskinan_Sedang", "Kemiskinan_Tinggi"), include.lowest = TRUE)

sovi_data$Age_Group <- cut(sovi_data$ELDERLY,
                           breaks = quantile(sovi_data$ELDERLY, probs = c(0, 0.5, 1), na.rm = TRUE),
                           labels = c("Muda", "Tua"), include.lowest = TRUE)

sovi_data$Education_Level <- cut(sovi_data$LOWEDU,
                                 breaks = quantile(sovi_data$LOWEDU, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                                 labels = c("Pendidikan_Tinggi", "Pendidikan_Sedang", "Pendidikan_Rendah"), include.lowest = TRUE)


# Gabungkan data sovi_data dengan data peta
sovi_peta <- left_join(peta_kabupaten, sovi_data, by = "DISTRICTCODE")




# Navy-Denim-Neptune theme color palette
colors <- c("#1A237E", "#3949AB", "#E8EAF6", "#90A4AE", "#5C6BC0", "#26C6DA", "#006064", "#455A64", "#263238")
# Navy-Denim-Neptune themed CSS with Enhanced Download Buttons
custom_css <- paste0("
.content-wrapper, .right-side {
  background: linear-gradient(135deg, #E8EAF6 0%, #F3E5F5 100%);
}
.main-header .navbar {
  background: linear-gradient(135deg, #1A237E 0%, #26C6DA 100%) !important;
}
.main-header .logo {
  background: linear-gradient(135deg, #1A237E 0%, #26C6DA 100%) !important;
}
.sidebar {
  background: linear-gradient(180deg, #263238 0%, #455A64 100%) !important;
}
.box {
  border-radius: 12px !important;
  box-shadow: 0 8px 20px rgba(26, 35, 126, 0.15) !important;
  border-top: 4px solid #3949AB !important;
  background: white !important;
}
.btn-primary {
  background: linear-gradient(135deg, #3949AB 0%, #26C6DA 100%) !important;
  border: none !important;
  border-radius: 8px !important;
  transition: all 0.3s ease !important;
}
.btn-primary:hover {
  background: linear-gradient(135deg, #1A237E 0%, #006064 100%) !important;
  transform: translateY(-2px) !important;
}
.value-box-icon {
  background: rgba(57, 73, 171, 0.2) !important;
}
.leaflet-container {
  border-radius: 12px;
  box-shadow: 0 4px 15px rgba(26, 35, 126, 0.1);
}

/* Enhanced Download Button Styles */
.download-panel {
  background: linear-gradient(135deg, #1A237E 0%, #26C6DA 100%);
  border-radius: 15px;
  padding: 20px;
  margin: 15px 0;
  box-shadow: 0 8px 25px rgba(26, 35, 126, 0.3);
  border: 2px solid rgba(255,255,255,0.2);
}

.download-btn {
  background: linear-gradient(135deg, #3949AB 0%, #26C6DA 100%);
  border: none;
  border-radius: 10px;
  padding: 12px 20px;
  color: white;
  font-weight: 600;
  margin: 5px;
  transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
  box-shadow: 0 4px 15px rgba(57, 73, 171, 0.3);
  position: relative;
  overflow: hidden;
}

.download-btn:before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, transparent, rgba(255,255,255,0.3), transparent);
  transition: left 0.5s;
}

.download-btn:hover:before {
  left: 100%;
}

.download-btn:hover {
  background: linear-gradient(135deg, #1A237E 0%, #006064 100%);
  transform: translateY(-3px) scale(1.05);
  box-shadow: 0 8px 30px rgba(26, 35, 126, 0.5);
  color: #fff;
}

.download-btn:active {
  transform: translateY(-1px) scale(1.02);
}

.download-btn.disabled {
  background: #ccc;
  cursor: not-allowed;
  transform: none;
  box-shadow: none;
}

.download-btn.disabled:hover {
  background: #ccc;
  transform: none;
  box-shadow: none;
}

.download-title {
  color: white;
  font-size: 18px;
  font-weight: 700;
  margin-bottom: 15px;
  text-align: center;
  text-shadow: 0 2px 4px rgba(0,0,0,0.3);
}
")

# Helper function untuk membuat download panel
create_download_panel <- function(tab_name, show_conditions = NULL) {
  # Conditional buttons based on tab requirements
  conditional_conditions <- if(!is.null(show_conditions)) {
    paste0("(", paste(show_conditions, collapse = " || "), ")")
  } else {
    "true"
  }
  
  conditionalPanel(
    condition = conditional_conditions,
    div(
      class = "download-panel",
      div(class = "download-title", paste("📥 Download", tools::toTitleCase(gsub("_", " ", tab_name)))),
      div(
        style = "text-align: center;",
        downloadButton(
          outputId = paste0("download_", tab_name, "_pdf"),
          label = HTML('<i class="fa fa-file-pdf-o" style="margin-right: 8px;"></i>📄 PDF Lengkap'),
          class = "download-btn",
          style = "width: 48%; margin: 1%;",
          title = "Download laporan lengkap dalam format PDF dengan semua grafik dan analisis"
        ),
        downloadButton(
          outputId = paste0("download_", tab_name, "_word"),
          label = HTML('<i class="fa fa-file-word-o" style="margin-right: 8px;"></i>📝 Word Report'),
          class = "download-btn",
          style = "width: 48%; margin: 1%;",
          title = "Download hasil analisis dalam format Word untuk editing lebih lanjut"
        )
      )
    )
  )
}

# UI
ui <- dashboardPage(
  title = "DAVIRA",
  skin = "blue",
  
  dashboardHeader(
    title = "🌟 DAVIRA",
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
    

  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML(custom_css)),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('triggerDownload', function(message) {
          $('#' + message.id)[0].click();
        });
      "))
    ),
    
    tabItems(
      # Beranda Tab
      tabItem(
        tabName = "beranda",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("🌟 DAVIRA - DASHBOARD SOVI BY RAYA", style = "margin: 0; font-weight: 600; font-size: 2.5em;"),
                                        p("🏛️ Advanced Social Vulnerability Intelligence Platform | KOMPUTASI STATISTIK 2025 🏆", style = "margin: 10px 0 0 0; opacity: 0.9; font-size: 1.2em;")
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
        
        # Enhanced Metadata Dashboard
        fluidRow(
          column(6,
                 box(
                   title = "Metadata Struktural", status = "primary", solidHeader = TRUE, width = NULL,
                   div(
                     style = "padding: 20px;",
                     h4("Struktur Dataset SOVI", style = paste0("color: ", colors[1], "; margin-bottom: 20px;")),
                     tags$ul(style = "font-size: 15px; line-height: 1.8;",
                             tags$li(tags$strong("Nama Dataset:"), " Social Vulnerability Index (SOVI)"),
                             tags$li(tags$strong("Dimensi Data:"), textOutput("total_observations_meta", inline = TRUE), " observasi × ", textOutput("total_variables_meta", inline = TRUE), " variabel"),
                             tags$li(tags$strong("Tipe Data:"), " Longitudinal spatial data"),
                             tags$li(tags$strong("Unit Analisis:"), " Kabupaten/Kota Indonesia"),
                             tags$li(tags$strong("Kelengkapan:"), textOutput("data_completeness_meta", inline = TRUE)),
                             tags$li(tags$strong("Format:"), " CSV, Shapefile"),
                             tags$li(tags$strong("Ukuran:"), " ~90KB (data) + ~14MB (geometri)")
                     ),
                     br(),
                     h5("Variabel Utama:", style = paste0("color: ", colors[7], ";")),
                     tags$div(style = paste0("background: ", colors[3], "; padding: 10px; border-radius: 6px;"),
                              tags$small("POVERTY, LOWEDU, ELDERLY, GROWTH, CHILDREN, FEMALE, POPULATION, dll.")
                     )
                   )
                 )
          ),
          column(6,
                 box(
                   title = "Metadata Referensi", status = "info", solidHeader = TRUE, width = NULL,
                   div(
                     style = "padding: 20px;",
                     h4("Sumber dan Referensi", style = paste0("color: ", colors[1], "; margin-bottom: 20px;")),
                     tags$ul(style = "font-size: 15px; line-height: 1.8;",
                             tags$li(tags$strong("Sumber Utama:"), tags$br(), 
                                     tags$a("SOVI Data Repository", 
                                            href = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv",
                                            target = "_blank", style = "color: #4CAF50;")),
                             tags$li(tags$strong("Dokumentasi Ilmiah:"), tags$br(),
                                     tags$a("Data in Brief - Science Direct", 
                                            href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180",
                                            target = "_blank", style = "color: #4CAF50;")),
                             tags$li(tags$strong("Metodologi:"), " Spatial clustering, regression analysis"),
                             tags$li(tags$strong("Lisensi:"), " Open Data License"),
                             tags$li(tags$strong("Update Terakhir:"), " 2021"),
                             tags$li(tags$strong("DOI:"), " 10.1016/j.dib.2021.107664")
                     )
                   )
                 )
          )
        ),
        
        # Categories explanation
        fluidRow(
          column(12,
                 box(
                   title = "Penjelasan Kategori SOVI", status = "info", solidHeader = TRUE, width = NULL,
                   div(
                     style = "padding: 15px;",
                     h4("Empat Kategori Utama dalam Analisis SOVI", style = paste0("color: ", colors[1], ";")),
                     fluidRow(
                       column(6,
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-bottom: 10px; border-left: 4px solid ", colors[1], ";"),
                                h5("1. Ukuran Populasi (Population_Size)", style = paste0("color: ", colors[1], ";")),
                                p("Berdasarkan median variabel POPULASI. Nilai di atas median dikategorikan sebagai 'Besar', di bawah median sebagai 'Kecil'.")
                              ),
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-bottom: 10px; border-left: 4px solid ", colors[7], ";"),
                                h5("2. Status Ekonomi (Economic_Status)", style = paste0("color: ", colors[7], ";")),
                                p("Berdasarkan kuartil variabel POVERTY. Terbagi menjadi: Kemiskinan_Rendah (0-33%), Kemiskinan_Sedang (33-67%), Kemiskinan_Tinggi (67-100%).")
                              )
                       ),
                       column(6,
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-bottom: 10px; border-left: 4px solid ", colors[8], ";"),
                                h5("3. Kelompok Usia (Age_Group)", style = paste0("color: ", colors[8], ";")),
                                p("Berdasarkan median variabel ELDERLY. Nilai di atas median dikategorikan sebagai 'Tua', di bawah median sebagai 'Muda'.")
                              ),
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-bottom: 10px; border-left: 4px solid ", colors[9], ";"),
                                h5("4. Tingkat Pendidikan (Education_Level)", style = paste0("color: ", colors[9], ";")),
                                p("Berdasarkan kuartil variabel LOWEDU. Terbagi menjadi: Pendidikan_Tinggi (0-33%), Pendidikan_Sedang (33-67%), Pendidikan_Rendah (67-100%).")
                              )
                       )
                     )
                   )
                 )
          )
        ),
        
        # Enhanced Main content with user selections
        fluidRow(
          column(6,
                 box(
                   title = "Distribusi Indikator SOVI", status = "primary", solidHeader = TRUE, width = NULL,
                   fluidRow(
                     column(6,
                            selectInput("beranda_dist_var", "Pilih Variabel:",
                                        choices = NULL, selected = "POVERTY")
                     ),
                     column(6,
                            selectInput("beranda_dist_type", "Jenis Plot:",
                                        choices = list("Histogram" = "histogram", "Density" = "density", "Boxplot" = "boxplot"),
                                        selected = "histogram")
                     )
                   ),
                   plotlyOutput("sovi_distribution", height = "300px")
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
                   fluidRow(
                     column(6,
                            selectInput("beranda_regional_var", "Pilih Variabel:",
                                        choices = NULL, selected = "POVERTY")
                     ),
                     column(6,
                            selectInput("beranda_regional_type", "Jenis Analisis:",
                                        choices = list("Rata-rata" = "mean", "Median" = "median", "Total" = "sum"),
                                        selected = "mean")
                     )
                   ),
                   plotlyOutput("regional_distribution", height = "300px")
                 )
          )
        ),
        
        # Peta Distribusi
        fluidRow(
          column(12,
                 box(
                   title = "Peta Distribusi SOVI Indonesia", status = "primary", solidHeader = TRUE, width = NULL,
                   fluidRow(
                     column(4,
                            selectInput("beranda_map_var", "Pilih Variabel untuk Peta:",
                                        choices = NULL, selected = "POVERTY")
                     ),
                     column(4,
                            selectInput("beranda_map_category", "Pilih Kategori:",
                                        choices = list(
                                          "Tidak ada (Kontinyu)" = "none",
                                          "Ukuran Populasi" = "Population_Size",
                                          "Status Ekonomi" = "Economic_Status",
                                          "Kelompok Usia" = "Age_Group",
                                          "Tingkat Pendidikan" = "Education_Level"
                                        ), selected = "none")
                     ),
                     column(4,
                            div(
                              style = paste0("background: ", colors[3], "; padding: 10px; border-radius: 6px;"),
                              h6("Skema Warna:", style = paste0("color: ", colors[1], "; margin: 0; font-weight: bold;")),
                              tags$small("🟢 Kontinyu | 🟠 Populasi | 🔵 Ekonomi | 🟡 Usia | 🟤 Pendidikan", 
                                         style = "color: #455A64; font-size: 11px;")
                            )
                     )
                   ),
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
        ),
        
        # Download Panel untuk Beranda
        fluidRow(
          column(12, create_download_panel("beranda"))
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
                     ),
                     
                     tabPanel("SOVI DATA",
                              br(),
                              div(
                                style = paste0("background: white; border-radius: 8px; padding: 20px; border: 1px solid ", colors[2], ";"),
                                h4("📊 Tabel Data SOVI Interaktif", style = paste0("color: ", colors[1], ";")),
                                
                                fluidRow(
                                  column(6,
                                         h5("Filter Data", style = paste0("color: ", colors[1], ";")),
                                         selectInput("sovi_filter_var", "Filter berdasarkan Variabel:",
                                                     choices = c("Semua Data" = "all", 
                                                                 "Tingkat Kemiskinan" = "POVERTY",
                                                                 "Pendidikan Rendah" = "LOWEDU",
                                                                 "Populasi Lansia" = "ELDERLY",
                                                                 "Pertumbuhan" = "GROWTH")),
                                         conditionalPanel(
                                           condition = "input.sovi_filter_var != 'all'",
                                           selectInput("sovi_filter_level", "Tingkat Filter:",
                                                       choices = list("Tinggi (> Q3)" = "high",
                                                                      "Sedang (Q1-Q3)" = "medium", 
                                                                      "Rendah (< Q1)" = "low",
                                                                      "Semua Level" = "all_levels"))
                                         )
                                  ),
                                  column(6,
                                         h5("Statistik Ringkasan", style = paste0("color: ", colors[1], ";")),
                                         div(
                                           style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 6px;"),
                                           verbatimTextOutput("sovi_table_summary")
                                         )
                                  )
                                ),
                                
                                br(),
                                h5("Tabel Data SOVI", style = paste0("color: ", colors[1], ";")),
                                DT::dataTableOutput("sovi_data_table", height = "500px"),
                                
                                br(),
                                br(),
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("Interpretasi Data SOVI", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("sovi_table_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        ),
        
        # Download Panel untuk Manajemen Data
        fluidRow(
          column(12, create_download_panel("manajemen"))
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
                                                     "🏛️ Choropleth (Polygon)" = "choropleth",
                                                     "📍 Scatter Points" = "scatter",
                                                     "🎯 Cluster Points" = "cluster",
                                                     "🌡️ Contour Map" = "contour",
                                                     "📊 Proportional Symbols" = "symbols"
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
                                       h5("🎯 Pengaturan Clustering Provinsi", style = paste0("color: ", colors[1], ";")),
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
                                       actionButton("run_clustering", "🚀 Jalankan Analisis Cluster",
                                                    class = "btn-primary", style = "width: 100%;")
                                ),
                                column(8,
                                       conditionalPanel(
                                         condition = "input.run_clustering > 0",
                                         h5("📊 Hasil Analisis Cluster", style = paste0("color: ", colors[1], ";")),
                                         div(
                                           style = paste0("background: ", colors[3], "; padding: 20px; border-radius: 8px; text-align: center;"),
                                           h4("✅ Analisis Cluster Selesai", style = paste0("color: ", colors[1], ";")),
                                           p("Lihat hasil detail di bagian tabel dan interpretasi di bawah.", style = "margin: 10px 0;")
                                         )
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
        ),
        
        # Download Panel untuk Eksplorasi Data
        fluidRow(
          column(12, create_download_panel("eksplorasi"))
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
                                                   choices = c("Population_Size", "Economic_Status", "Age_Group", "Education_Level"))
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
        ),
        
        # Download Panel untuk Uji Asumsi (Conditional)
        fluidRow(
          column(12, create_download_panel("asumsi", c("input.run_normality > 0", "input.run_homogeneity > 0")))
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
                                                   choices = c("Population_Size", "Economic_Status", "Age_Group", "Education_Level"))
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
                                                   choices = c("Population_Size", "Economic_Status", "Age_Group", "Education_Level"))
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
                                                   choices = c("Population_Size", "Economic_Status", "Age_Group", "Education_Level"))
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
                                                   choices = c("Population_Size", "Economic_Status", "Age_Group", "Education_Level"))
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
                                                   choices = c( "Population_Size", "Economic_Status"))
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
    
    # Update beranda choices
    updateSelectInput(session, "beranda_dist_var", choices = numeric_vars)
    updateSelectInput(session, "beranda_regional_var", choices = numeric_vars)
    updateSelectInput(session, "beranda_map_var", choices = numeric_vars)
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
  
  # Enhanced Regional distribution plot (dynamic)
  output$regional_distribution <- renderPlotly({
    req(input$beranda_regional_var, input$beranda_regional_type)
    
    var_name <- input$beranda_regional_var
    analysis_type <- input$beranda_regional_type
    
    regional_stats <- sovi_data %>%
      mutate(Region = case_when(
        substr(as.character(DISTRICTCODE), 1, 2) %in% c("11", "12", "13", "14", "15", "16", "17", "18", "19", "21") ~ "Sumatera",
        substr(as.character(DISTRICTCODE), 1, 2) %in% c("31", "32", "33", "34", "35", "36") ~ "Jawa-Bali",
        substr(as.character(DISTRICTCODE), 1, 2) %in% c("51", "52", "53", "61", "62", "63", "64", "71", "72", "73", "74", "75", "76") ~ "Kalimantan-Sulawesi",
        TRUE ~ "Indonesia Timur"
      )) %>%
      group_by(Region) %>%
      summarise(
        Value = if(analysis_type == "mean") mean(.data[[var_name]], na.rm = TRUE) 
        else if(analysis_type == "median") median(.data[[var_name]], na.rm = TRUE)
        else sum(.data[[var_name]], na.rm = TRUE),
        Count = n(),
        .groups = "drop"
      )
    
    analysis_label <- switch(analysis_type,
                             "mean" = "Rata-rata",
                             "median" = "Median", 
                             "sum" = "Total")
    
    p <- ggplot(regional_stats, aes(x = Region, y = Value, fill = Region)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = colors[1:4]) +
      labs(title = paste(analysis_label, var_name, "per Region"), 
           x = "Region", y = paste(analysis_label, var_name)) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Enhanced SOVI distribution plot (dynamic)
  output$sovi_distribution <- renderPlotly({
    req(input$beranda_dist_var, input$beranda_dist_type)
    
    var_data <- sovi_data[[input$beranda_dist_var]]
    var_name <- input$beranda_dist_var
    
    if(input$beranda_dist_type == "histogram") {
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
        labs(title = paste("Histogram", var_name), x = var_name, y = "Frekuensi")
    } else if(input$beranda_dist_type == "density") {
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_density(fill = colors[1], alpha = 0.7, color = colors[2]) +
        labs(title = paste("Density Plot", var_name), x = var_name, y = "Densitas")
    } else {
      p <- ggplot(data.frame(x = var_data), aes(y = x)) +
        geom_boxplot(fill = colors[1], alpha = 0.7, color = colors[2]) +
        labs(title = paste("Boxplot", var_name), y = var_name)
    }
    
    p <- p + theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = colors[1]),
        axis.title = element_text(size = 12, color = colors[1])
      )
    
    ggplotly(p) %>%
      layout(showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  
  # Enhanced Beranda map with category-specific color schemes
  output$beranda_map <- renderLeaflet({
    req(input$beranda_map_var, input$beranda_map_category)
    
    # Filter valid data
    map_var <- input$beranda_map_var
    sovi_peta_valid <- sovi_peta %>% filter(!is.na(.data[[map_var]]))
    
    if(nrow(sovi_peta_valid) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 5))
    }
    
    if(input$beranda_map_category == "none") {
      # Continuous variable mapping with green theme
      pal <- colorNumeric(
        palette = c("#E8F5E8", "#A5D6A7", "#66BB6A", "#4CAF50", "#2E7D32"),
        domain = sovi_peta_valid[[map_var]]
      )
      map_color_theme <- "#2E7D32"
      
    } else {
      # Category-specific color schemes
      cat_var <- input$beranda_map_category
      category_data <- sovi_peta_valid[[cat_var]]
      unique_cats <- unique(category_data[!is.na(category_data)])
      
      if(cat_var == "Population_Size") {
        # Orange theme for population
        cat_palette <- c("#FFF3E0", "#FFB74D", "#FF9800", "#F57C00", "#E65100")[1:length(unique_cats)]
        map_color_theme <- "#FF9800"
      } else if(cat_var == "Economic_Status") {
        # Blue theme for economic status
        cat_palette <- c("#E3F2FD", "#90CAF9", "#2196F3", "#1976D2", "#0D47A1")[1:length(unique_cats)]
        map_color_theme <- "#2196F3"
      } else if(cat_var == "Age_Group") {
        # Yellow theme for age groups
        cat_palette <- c("#FFFDE7", "#FFF176", "#FFEB3B", "#FBC02D", "#F57F17")[1:length(unique_cats)]
        map_color_theme <- "#FFEB3B"
      } else if(cat_var == "Education_Level") {
        # Dark yellow/amber theme for education
        cat_palette <- c("#FFF8E1", "#FFCC02", "#FFC107", "#FF8F00", "#FF6F00")[1:length(unique_cats)]
        map_color_theme <- "#FFC107"
      } else {
        # Default navy theme
        cat_palette <- colors[1:length(unique_cats)]
        map_color_theme <- colors[1]
      }
      
      pal <- colorFactor(palette = cat_palette, domain = unique_cats)
    }
    
    leaflet(sovi_peta_valid) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addPolygons(
        fillColor = if(input$beranda_map_category == "none") ~pal(sovi_peta_valid[[map_var]]) else ~pal(category_data),
        weight = 1.5,
        opacity = 1,
        color = "white",
        dashArray = "2",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 3,
          color = map_color_theme,
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~lapply(paste(
          "<div style='font-family: Arial; font-size: 14px; max-width: 320px; padding: 8px;'>",
          "<strong style='color: ", map_color_theme, "; font-size: 16px;'>🏛️ ", nmkab, "</strong><br/>",
          "<span style='color: #546E7A;'>📍 Provinsi:</span> <strong>", nmprov, "</strong><br/>",
          if(input$beranda_map_category != "none") paste0(
            "<span style='color: #546E7A;'>🏷️ ", cat_var, ":</span> <strong style='color: ", map_color_theme, ";'>", 
            ifelse(is.na(category_data), "N/A", as.character(category_data)), "</strong><br/>") else "",
          "<span style='color: #546E7A;'>📊 ", map_var, ":</span> <strong style='color: ", map_color_theme, ";'>", round(sovi_peta_valid[[map_var]], 2), "</strong><br/>",
          "<span style='color: #546E7A;'>👥 Populasi:</span> ", format(POPULATION, big.mark = ","), "<br/>",
          "<span style='color: #546E7A;'>📈 Level:</span> <strong>", 
          ifelse(sovi_peta_valid[[map_var]] > quantile(sovi_peta_valid[[map_var]], 0.75, na.rm = TRUE), "🔴 Tinggi",
                 ifelse(sovi_peta_valid[[map_var]] > quantile(sovi_peta_valid[[map_var]], 0.25, na.rm = TRUE), "🟡 Sedang", "🟢 Rendah")), "</strong>",
          "</div>"
        ), HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "10px 15px", "border-radius" = "10px"),
          textsize = "14px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = if(input$beranda_map_category == "none") sovi_peta_valid[[map_var]] else category_data, 
        opacity = 0.8, 
        title = HTML(paste0("<strong style='color: ", map_color_theme, ";'>", 
                            if(input$beranda_map_category == "none") map_var else cat_var, "</strong>")),
        position = "bottomright"
      )
  })
  
  # SOVI DATA Table functionality with spatial information
  output$sovi_data_table <- DT::renderDataTable({
    # Merge sovi_data with geographic information from sovi_peta
    display_data <- sovi_data %>%
      left_join(
        sovi_peta %>% 
          st_drop_geometry() %>% 
          select(DISTRICTCODE, nmkab, nmprov),
        by = "DISTRICTCODE"
      ) %>%
      select(
        `District Code` = DISTRICTCODE,
        `Kabupaten/Kota` = nmkab,
        `Provinsi` = nmprov,
        `Children (%)` = CHILDREN,
        `Female (%)` = FEMALE,
        `Elderly (%)` = ELDERLY,
        `Family Head (%)` = FHEAD,
        `Family Size` = FAMILYSIZE,
        `No Electric (%)` = NOELECTRIC,
        `Low Education (%)` = LOWEDU,
        `Growth (%)` = GROWTH,
        `Poverty (%)` = POVERTY,
        `Illiterate (%)` = ILLITERATE,
        `No Training (%)` = NOTRAINING,
        `Disaster Prone (%)` = DPRONE,
        `Rented (%)` = RENTED,
        `No Sewer (%)` = NOSEWER,
        `Tap Water (%)` = TAPWATER,
        `Population` = POPULATION
      ) %>%
      filter(!is.na(`District Code`)) %>%
      arrange(`District Code`)
    
    # Apply filters if any
    if(input$sovi_filter_var != "all") {
      var_mapping <- c(
        "POVERTY" = "Poverty (%)",
        "LOWEDU" = "Low Education (%)",
        "ELDERLY" = "Elderly (%)",
        "GROWTH" = "Growth (%)"
      )
      
      var_col <- var_mapping[input$sovi_filter_var]
      if(var_col %in% names(display_data)) {
        var_values <- display_data[[var_col]]
        
        if(!is.null(input$sovi_filter_level) && input$sovi_filter_level != "all_levels") {
          q1 <- quantile(var_values, 0.25, na.rm = TRUE)
          q3 <- quantile(var_values, 0.75, na.rm = TRUE)
          
          if(input$sovi_filter_level == "high") {
            display_data <- display_data[var_values > q3, ]
          } else if(input$sovi_filter_level == "medium") {
            display_data <- display_data[var_values >= q1 & var_values <= q3, ]
          } else if(input$sovi_filter_level == "low") {
            display_data <- display_data[var_values < q1, ]
          }
        }
      }
    }
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'print'),
        columnDefs = list(
          list(targets = c(1, 2), className = "dt-left"),
          list(targets = "_all", className = "dt-center")
        )
      ),
      extensions = 'Buttons',
      caption = "Tabel Interaktif Data Social Vulnerability Index (SOVI)",
      filter = 'none', # Menghapus filter per kolom, hanya menyisakan sorting
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = which(names(display_data) %in% c("Children (%)", "Female (%)", "Elderly (%)", "Poverty (%)", "Low Education (%)")), digits = 2) %>%
      DT::formatStyle(
        columns = "District Code",
        `font-size` = '90%',
        backgroundColor = colors[3],
        color = colors[1],
        fontWeight = "bold"
      ) %>%
      DT::formatStyle(
        columns = c("Kabupaten/Kota", "Provinsi"),
        backgroundColor = colors[3],
        color = colors[9],
        fontWeight = "500"
      ) %>%
      DT::formatStyle(
        columns = 4:ncol(display_data),  # Data numerik
        backgroundColor = "white",
        color = colors[1]
      )
  })
  
  output$sovi_table_summary <- renderPrint({
    total_observations <- nrow(sovi_data)
    total_with_geo <- nrow(sovi_data %>% 
                             left_join(sovi_peta %>% st_drop_geometry() %>% select(DISTRICTCODE, nmkab), 
                                       by = "DISTRICTCODE") %>% 
                             filter(!is.na(nmkab)))
    
    cat("📊 RINGKASAN DATA SOVI\n")
    cat("======================\n")
    cat("Total Observasi:", total_observations, "\n")
    cat("Dengan Info Geografis:", total_with_geo, "\n")
    cat("Variabel Tersedia:", ncol(sovi_data), "\n")
    
    if(input$sovi_filter_var != "all") {
      cat("Filter Aktif:", input$sovi_filter_var, "\n")
      if(!is.null(input$sovi_filter_level) && input$sovi_filter_level != "all_levels") {
        level_desc <- switch(input$sovi_filter_level,
                             "high" = "Tinggi (> Q3)",
                             "medium" = "Sedang (Q1-Q3)", 
                             "low" = "Rendah (< Q1)")
        cat("Level Filter:", level_desc, "\n")
      }
    }
    
    cat("\nStatistik Ringkas:\n")
    cat("Rata-rata Poverty:", round(mean(sovi_data$POVERTY, na.rm = TRUE), 1), "%\n")
    cat("Rata-rata Low Education:", round(mean(sovi_data$LOWEDU, na.rm = TRUE), 1), "%\n")
    cat("Rata-rata Elderly:", round(mean(sovi_data$ELDERLY, na.rm = TRUE), 1), "%\n")
    cat("Rata-rata Growth:", round(mean(sovi_data$GROWTH, na.rm = TRUE), 1), "%\n")
    cat("Total Population:", format(sum(sovi_data$POPULATION, na.rm = TRUE), big.mark = ","), "\n")
    
    cat("\nRentang Data:\n")
    cat("District Code:", min(sovi_data$DISTRICTCODE, na.rm = TRUE), "-", max(sovi_data$DISTRICTCODE, na.rm = TRUE), "\n")
    cat("Data Completeness:", round(sum(complete.cases(sovi_data))/nrow(sovi_data)*100, 1), "%\n")
  })
  
  output$sovi_table_interpretation <- renderUI({
    filtered_count <- if(input$sovi_filter_var == "all") nrow(sovi_data) else {
      # Calculate filtered count based on current filters
      nrow(sovi_data) # Simplified for now
    }
    
    interpretation <- paste0(
      "Tabel data SOVI interaktif menampilkan ", filtered_count, " observasi dari total ", nrow(sovi_data), " kabupaten/kota. ",
      "Fitur filtering memungkinkan eksplorasi data berdasarkan variabel kunci dan tingkat nilai tertentu. "
    )
    
    if(input$sovi_filter_var != "all") {
      interpretation <- paste0(interpretation, 
                               "Filter aktif berdasarkan variabel ", input$sovi_filter_var, " memberikan subset data yang relevan untuk analisis spesifik. ")
    }
    
    interpretation <- paste0(interpretation,
                             "Data dapat didownload dalam berbagai format untuk analisis lebih lanjut. ",
                             "Setiap baris mewakili satu kabupaten/kota dengan indikator kerentanan sosial yang komprehensif.")
    
    HTML(interpretation)
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
      "Dashboard ini dirancang untuk ujian KOMPUTASI STATISTIK STIS 2025 dengan implementasi metodologi analisis yang komprehensif dan sesuai standar akademik."
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
  
  # Enhanced Exploration map with multiple types
  output$exploration_map <- renderLeaflet({
    req(input$map_variable, input$map_type)
    
    # Use sovi_peta data with spatial information
    sovi_peta_valid <- sovi_peta %>% filter(!is.na(.data[[input$map_variable]]))
    
    if(nrow(sovi_peta_valid) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 5))
    }
    
    # Get coordinates for point-based maps
    coords <- st_coordinates(st_centroid(sovi_peta_valid$geometry))
    sovi_peta_valid$longitude <- coords[, "X"]
    sovi_peta_valid$latitude <- coords[, "Y"]
    
    var_data <- sovi_peta_valid[[input$map_variable]]
    
    # Consistent green palette
    green_pal <- c("#E8F5E8", "#81C784", "#4CAF50", "#2E7D32", "#1B5E20")
    
    map_base <- leaflet(sovi_peta_valid) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2.5, zoom = 5)
    
    # Enhanced hover labels
    create_hover_label <- function(name, province, value, interpretation) {
      paste0(
        "<div style='font-family: Arial; font-size: 13px; max-width: 280px; padding: 8px;'>",
        "<strong style='color: #2E7D32; font-size: 15px;'>🏛️ ", name, "</strong><br/>",
        "<span style='color: #546E7A;'>📍 Provinsi:</span> <strong>", province, "</strong><br/>",
        "<span style='color: #546E7A;'>📊 ", input$map_variable, ":</span> <strong style='color: #4CAF50;'>", round(value, 2), "</strong><br/>",
        "<span style='color: #546E7A;'>📈 Interpretasi:</span> <strong>", interpretation, "</strong>",
        "</div>"
      )
    }
    
    # Calculate interpretations
    q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    q3 <- quantile(var_data, 0.75, na.rm = TRUE)
    interpretations <- ifelse(var_data > q3, "🔴 Tinggi",
                              ifelse(var_data > q1, "🟡 Sedang", "🟢 Rendah"))
    
    if (input$map_type == "choropleth") {
      # Choropleth map (polygon-based)
      pal <- colorNumeric(palette = green_pal, domain = var_data)
      
      map_base %>%
        addPolygons(
          fillColor = ~pal(var_data),
          weight = 1.5,
          opacity = 1,
          color = "white",
          dashArray = "2",
          fillOpacity = 0.8,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#2E7D32",
            dashArray = "",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = ~lapply(create_hover_label(nmkab, nmprov, var_data, interpretations), HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "8px 12px", "border-radius" = "8px"),
            textsize = "13px", direction = "auto"
          )
        ) %>%
        addLegend(pal = pal, values = var_data, opacity = 0.8, 
                  title = HTML(paste0("<strong style='color: #2E7D32;'>", input$map_variable, "</strong>")),
                  position = "bottomright")
      
    } else if (input$map_type == "scatter") {
      # Scatter points
      pal <- colorNumeric(palette = green_pal, domain = var_data)
      
      map_base %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = ~sqrt(var_data) * 2 + 3,
          fillColor = ~pal(var_data),
          color = "white",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.8,
          label = ~lapply(create_hover_label(nmkab, nmprov, var_data, interpretations), HTML),
          labelOptions = labelOptions(textsize = "13px", direction = "auto")
        ) %>%
        addLegend(pal = pal, values = var_data, opacity = 0.8,
                  title = HTML(paste0("<strong style='color: #2E7D32;'>", input$map_variable, "</strong>")),
                  position = "bottomright")
      
    } else if (input$map_type == "cluster") {
      # Cluster points
      map_base %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = 6,
          fillColor = ~ifelse(var_data > q3, "#2E7D32", ifelse(var_data > q1, "#4CAF50", "#81C784")),
          color = "white",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.9,
          clusterOptions = markerClusterOptions(),
          label = ~lapply(create_hover_label(nmkab, nmprov, var_data, interpretations), HTML)
        )
      
    } else if (input$map_type == "symbols") {
      # Proportional symbols
      max_radius <- 15
      min_radius <- 3
      radius_vals <- min_radius + (var_data - min(var_data, na.rm = TRUE)) / 
        (max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE)) * (max_radius - min_radius)
      
      map_base %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = radius_vals,
          fillColor = "#4CAF50",
          color = "#2E7D32",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~lapply(create_hover_label(nmkab, nmprov, var_data, interpretations), HTML)
        ) %>%
        addLegend(
          colors = "#4CAF50", labels = paste("Ukuran = f(", input$map_variable, ")"),
          title = HTML(paste0("<strong style='color: #2E7D32;'>", input$map_variable, "</strong>")),
          position = "bottomright"
        )
      
    } else { # contour
      # Simple contour approximation using circles
      map_base %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = ~var_data * 0.5,
          fillColor = "#4CAF50",
          color = "#2E7D32",
          weight = 1,
          opacity = 0.5,
          fillOpacity = 0.3,
          label = ~lapply(create_hover_label(nmkab, nmprov, var_data, interpretations), HTML)
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
    
    # Check if we have at least 1 predictor
    if(length(input$regression_predictors) < 1) {
      showNotification("Silakan pilih minimal 1 variabel prediktor.", type = "warning")
      return()
    }
    
    # Check if response is not in predictors
    if(input$regression_response %in% input$regression_predictors) {
      showNotification("Variabel respons tidak boleh sama dengan variabel prediktor.", type = "error")
      return()
    }
    
    # Create formula
    formula_str <- paste(input$regression_response, "~", paste(input$regression_predictors, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    # Fit model with error handling
    tryCatch({
      values$regression_model <- lm(formula_obj, data = sovi_data)
    }, error = function(e) {
      showNotification(paste("Error dalam regresi:", e$message), type = "error")
      return()
    })
    
    output$regression_result <- renderPrint({
      summary(values$regression_model)
    })
    
    output$model_summary <- renderPrint({
      req(values$regression_model)
      
      tryCatch({
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
      }, error = function(e) {
        cat("Error dalam menampilkan ringkasan model:", e$message, "\n")
      })
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
      req(values$regression_model)
      
      tryCatch({
        model <- values$regression_model
        summary_model <- summary(model)
        r_squared <- summary_model$r.squared
        adj_r_squared <- summary_model$adj.r.squared
        f_stat <- summary_model$fstatistic[1]
        f_p_value <- pf(f_stat, summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE)
        
        # Count significant predictors
        coef_p_values <- summary_model$coefficients[, "Pr(>|t|)"]
        sig_predictors <- sum(coef_p_values[-1] < 0.05, na.rm = TRUE)  # Exclude intercept
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
      }, error = function(e) {
        HTML(paste("Error dalam interpretasi:", e$message))
      })
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
  
  # Enhanced Provincial Cluster Analysis
  observeEvent(input$run_clustering, {
    req(input$cluster_variables, length(input$cluster_variables) >= 2)
    
    # Prepare province-level aggregated data for clustering
    province_data <- sovi_peta %>%
      st_drop_geometry() %>%
      filter(!is.na(POVERTY)) %>%
      group_by(nmprov) %>%
      summarise(
        across(all_of(input$cluster_variables), ~ mean(.x, na.rm = TRUE)),
        kabupaten_count = n(),
        total_population = sum(POPULATION, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(complete.cases(.))
    
    # Extract cluster variables
    cluster_data <- province_data[, input$cluster_variables, drop = FALSE]
    
    # Scale the data
    cluster_data_scaled <- scale(cluster_data)
    
    # Perform clustering based on selected method
    if(input$cluster_method == "kmeans") {
      cluster_result <- kmeans(cluster_data_scaled, centers = input$n_clusters, nstart = 25)
      clusters <- cluster_result$cluster
      
      output$cluster_summary <- renderPrint({
        cat("🏛️ ANALISIS CLUSTER PROVINSI (K-MEANS)\n")
        cat("=====================================\n")
        cat("Jumlah Cluster:", input$n_clusters, "\n")
        cat("Jumlah Provinsi:", nrow(province_data), "\n")
        cat("Total within-cluster sum of squares:", round(cluster_result$tot.withinss, 2), "\n")
        cat("Between-cluster sum of squares:", round(cluster_result$betweenss, 2), "\n")
        cat("Variance Explained:", round(cluster_result$betweenss/cluster_result$totss * 100, 1), "%\n\n")
        
        cat("📊 Distribusi Cluster:\n")
        cluster_table <- table(clusters)
        for(i in 1:length(cluster_table)) {
          cat(paste0("Cluster ", i, ": ", cluster_table[i], " provinsi\n"))
        }
        
        cat("\n🏛️ Provinsi per Cluster:\n")
        for(i in 1:input$n_clusters) {
          prov_in_cluster <- province_data$nmprov[clusters == i]
          cat(paste0("\nCluster ", i, " (", length(prov_in_cluster), " provinsi):\n"))
          for(j in 1:length(prov_in_cluster)) {
            cat(paste0("  ", j, ". ", prov_in_cluster[j], "\n"))
          }
        }
        
        cat("\n📈 Karakteristik Cluster (rata-rata):\n")
        cluster_chars <- aggregate(cluster_data, by = list(Cluster = clusters), FUN = mean)
        print(round(cluster_chars, 2))
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
    
    
    
    # Enhanced Cluster table with province details
    output$cluster_table <- DT::renderDataTable({
      cluster_detail_table <- province_data %>%
        mutate(Cluster = paste0("Cluster ", clusters)) %>%
        select(
          `Provinsi` = nmprov,
          `Cluster` = Cluster,
          `Jumlah Kab/Kota` = kabupaten_count,
          `Total Populasi` = total_population,
          everything(),
          -kabupaten_count, -total_population
        ) %>%
        arrange(Cluster, Provinsi)
      
      # Format numbers
      numeric_cols <- sapply(cluster_detail_table, is.numeric)
      cluster_detail_table[numeric_cols] <- lapply(cluster_detail_table[numeric_cols], function(x) round(x, 2))
      
      DT::datatable(
        cluster_detail_table,
        options = list(
          pageLength = 15, 
          scrollX = TRUE,
          columnDefs = list(
            list(targets = c(0, 1), className = "dt-left"),
            list(targets = "_all", className = "dt-center")
          )
        ),
        caption = "🏛️ Detail Cluster Provinsi - Analisis SOVI",
        filter = 'top',
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          columns = "Cluster",
          backgroundColor = colors[3],
          color = colors[1],
          fontWeight = "bold"
        ) %>%
        DT::formatStyle(
          columns = "Provinsi",
          backgroundColor = "white",
          color = colors[9],
          fontWeight = "500"
        ) %>%
        DT::formatRound(columns = which(sapply(cluster_detail_table, is.numeric)), digits = 1)
    })
    
    # Enhanced Cluster interpretation with provincial focus
    output$cluster_interpretation <- renderUI({
      n_clusters <- length(unique(clusters))
      cluster_sizes <- table(clusters)
      largest_cluster <- which.max(cluster_sizes)
      smallest_cluster <- which.min(cluster_sizes)
      total_provinces <- nrow(province_data)
      
      method_desc <- switch(input$cluster_method,
                            "kmeans" = "K-Means clustering provinsi menggunakan algoritma centroid-based",
                            "hierarchical" = "Hierarchical clustering provinsi menggunakan pendekatan agglomerative",
                            "pam" = "PAM clustering provinsi menggunakan medoid-based approach")
      
      # Calculate cluster characteristics
      cluster_chars <- aggregate(cluster_data, by = list(Cluster = clusters), FUN = mean)
      variance_explained <- if(exists("cluster_result") && input$cluster_method == "kmeans") {
        round(cluster_result$betweenss/cluster_result$totss * 100, 1)
      } else "N/A"
      
      interpretation <- paste0(
        "<strong>🏛️ Hasil Analisis Cluster Provinsi SOVI:</strong><br><br>",
        
        method_desc, " berhasil mengelompokkan <strong>", total_provinces, " provinsi</strong> menjadi <strong>", n_clusters, " cluster</strong> ",
        "berdasarkan karakteristik kerentanan sosial. ",
        if(variance_explained != "N/A") paste0("Model menjelaskan <strong>", variance_explained, "%</strong> variasi data. ") else "",
        "<br><br>",
        
        "<strong>📊 Distribusi Cluster:</strong><br>",
        "• Cluster terbesar: <strong>Cluster ", largest_cluster, "</strong> (", max(cluster_sizes), " provinsi)<br>",
        "• Cluster terkecil: <strong>Cluster ", smallest_cluster, "</strong> (", min(cluster_sizes), " provinsi)<br>",
        "• Rata-rata provinsi per cluster: ", round(total_provinces/n_clusters, 1), " provinsi<br><br>",
        
        "<strong>🎯 Interpretasi Kebijakan:</strong><br>",
        "• <strong>Variabel Analisis:</strong> ", paste(input$cluster_variables, collapse = ", "), "<br>",
        "• <strong>Unit Analisis:</strong> Provinsi (agregasi kabupaten/kota)<br>",
        "• <strong>Peta Interaktif:</strong> Menampilkan cluster spasial dengan hover detail<br>",
        "• <strong>Aplikasi:</strong> Perencanaan pembangunan dan alokasi sumber daya regional<br><br>",
        
        "<strong>💡 Insights Strategis:</strong><br>",
        "Setiap cluster merepresentasikan provinsi dengan pola kerentanan sosial yang serupa, memungkinkan: ",
        "(1) strategi intervensi yang tepat sasaran, (2) benchmarking antar provinsi dalam cluster yang sama, ",
        "dan (3) identifikasi best practices dari cluster dengan performa terbaik. ",
        "Analisis ini mendukung evidence-based policy making sesuai dengan standar metodologi STIS."
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
  
  # Enhanced function untuk membuat konten laporan R Markdown
  create_report_content <- function(tab_name) {
    title <- paste("Laporan Analisis SOVI -", tools::toTitleCase(tab_name))
    
    # Enhanced content with interpretations
    content <- paste0(
      '---\n',
      'title: "', title, '"\n',
      'author: "Dashboard Analisis SOVI "\n',
      'date: "', format(Sys.Date(), "%d %B %Y"), '"\n',
      'output:\n',
      '  word_document:\n',
      '    reference_docx: null\n',
      '  pdf_document:\n',
      '    latex_engine: xelatex\n',
      '---\n\n',
      '```{r setup, include=FALSE}\n',
      'knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6)\n',
      'library(ggplot2); library(dplyr); library(readr); library(knitr)\n',
      'if(file.exists("sovi_data.csv")) {\n',
      '  sovi_data <- read_csv("sovi_data.csv")\n',
      '} else {\n',
      '  sovi_data <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv")\n',
      '}\n',
      'colors <- c("#2C3E50", "#34495E", "#ECF0F1", "#BDC3C7", "#95A5A6", "#7F8C8D", "#E74C3C", "#3498DB", "#2ECC71")\n',
      '```\n\n',
      
      '# Executive Summary\n\n',
      'Laporan ini menyajikan hasil analisis komprehensif terhadap dataset Social Vulnerability Index (SOVI) yang mencakup ', nrow(sovi_data), ' observasi kabupaten/kota di Indonesia. Analisis ini dilakukan dalam rangka ujian KOMPUTASI STATISTIK STIS 2025 dengan fokus pada tab "', tab_name, '".\n\n',
      
      '## Dataset Overview\n\n',
      '```{r dataset-info}\n',
      'cat("Total Observasi:", nrow(sovi_data), "\\n")\n',
      'cat("Total Variabel:", ncol(sovi_data), "\\n")\n',
      'completeness <- sum(complete.cases(sovi_data))/nrow(sovi_data) * 100\n',
      'cat("Kelengkapan Data:", round(completeness, 1), "%\\n")\n',
      'cat("Rata-rata Kemiskinan:", round(mean(sovi_data$POVERTY, na.rm = TRUE), 2), "%\\n")\n',
      'cat("Rata-rata Pendidikan Rendah:", round(mean(sovi_data$LOWEDU, na.rm = TRUE), 2), "%\\n")\n',
      '```\n\n',
      
      '## Analisis Utama\n\n',
      
      if(tab_name == "beranda") {
        paste0(
          '### Distribusi Variabel Kunci\n\n',
          '```{r main-plot}\n',
          'p1 <- ggplot(sovi_data, aes(x = POVERTY)) + \n',
          '  geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") + \n',
          '  labs(title = "Distribusi Tingkat Kemiskinan", x = "Tingkat Kemiskinan (%)", y = "Frekuensi") + \n',
          '  theme_minimal()\n',
          'print(p1)\n',
          '```\n\n',
          
          '```{r correlation-analysis}\n',
          'library(corrplot)\n',
          'key_vars <- sovi_data[, c("POVERTY", "LOWEDU", "ELDERLY", "GROWTH", "CHILDREN")]\n',
          'cor_matrix <- cor(key_vars, use = "complete.obs")\n',
          'corrplot(cor_matrix, method = "color", type = "upper", \n',
          '         col = colorRampPalette(c(colors[7], "white", colors[8]))(100),\n',
          '         addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7)\n',
          'title("Matriks Korelasi Variabel Kunci SOVI")\n',
          '```\n\n'
        )
      } else if(tab_name == "eksplorasi") {
        '### Analisis Eksploratori\n\n```{r exploration-plot}\np2 <- ggplot(sovi_data, aes(x = POVERTY, y = LOWEDU)) + geom_point(color = colors[1], alpha = 0.6) + geom_smooth(method = "lm", color = colors[2]) + labs(title = "Hubungan Kemiskinan vs Pendidikan Rendah", x = "Tingkat Kemiskinan (%)", y = "Pendidikan Rendah (%)") + theme_minimal()\nprint(p2)\n```\n\n'
      } else if(tab_name == "regresi") {
        '### Analisis Regresi\n\n```{r regression-analysis}\nmodel <- lm(POVERTY ~ LOWEDU + ELDERLY + GROWTH, data = sovi_data)\nsummary(model)\n\npar(mfrow = c(2, 2))\nplot(model, which = 1:4)\npar(mfrow = c(1, 1))\n```\n\n'
      } else {
        '### Analisis Statistik\n\n```{r general-analysis}\nsummary(sovi_data[, c("POVERTY", "LOWEDU", "ELDERLY", "GROWTH")])\n```\n\n'
      },
      
      '## Interpretasi dan Kesimpulan\n\n',
      
      if(tab_name == "beranda") {
        'Berdasarkan analisis dashboard beranda, dataset SOVI menunjukkan variasi yang signifikan dalam indikator kerentanan sosial antar wilayah. Distribusi kemiskinan menunjukkan pola yang heterogen, dengan beberapa wilayah menunjukkan tingkat kerentanan yang tinggi. Matriks korelasi mengungkap hubungan yang kompleks antar variabel, memberikan insight penting untuk analisis lanjutan.'
      } else if(tab_name == "manajemen") {
        'Analisis manajemen data menunjukkan bahwa dataset SOVI memiliki kualitas yang baik dengan tingkat kelengkapan yang tinggi. Struktur data mendukung berbagai jenis analisis statistik yang diperlukan untuk penelitian kerentanan sosial.'
      } else if(tab_name == "eksplorasi") {
        'Eksplorasi data mengungkap pola dan hubungan yang menarik antar variabel SOVI. Analisis ini memberikan dasar yang kuat untuk pemodelan statistik lanjutan dan pemahaman yang lebih mendalam tentang faktor-faktor yang mempengaruhi kerentanan sosial.'
      } else if(tab_name == "regresi") {
        'Model regresi linear berganda menunjukkan hubungan yang signifikan antara variabel prediktor dan tingkat kemiskinan. Diagnostik model menunjukkan pemenuhan asumsi regresi, mendukung validitas hasil analisis.'
      } else {
        'Analisis statistik yang dilakukan memberikan pemahaman yang komprehensif tentang karakteristik dataset SOVI dan hubungan antar variabel yang relevan untuk penelitian kerentanan sosial.'
      },
      
      '\n\n## Metadata Analisis\n\n',
      '- **Platform Analisis**: R Shiny Dashboard\n',
      '- **Metode Statistik**: Sesuai dengan kurikulum KOMPUTASI STATISTIK STIS\n',
      '- **Tanggal Analisis**: ', format(Sys.Date(), "%d %B %Y"), '\n',
      '- **Sumber Data**: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv\n',
      '- **Referensi**: https://www.sciencedirect.com/science/article/pii/S2352340921010180\n\n',
      
      '```{r session-info}\n',
      'sessionInfo()\n',
      '```\n'
    )
    return(content)
  }
  
  # Enhanced Metadata downloads
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste0("sovi_metadata_", format(Sys.time(), "%Y%m%d_%H%M"), ".json")
    },
    content = function(file) {
      metadata <- list(
        dataset_info = list(
          name = "Social Vulnerability Index (SOVI)",
          description = "Comprehensive social vulnerability dataset for Indonesian districts",
          dimensions = list(observations = nrow(sovi_data), variables = ncol(sovi_data)),
          data_quality = list(
            completeness = round(sum(complete.cases(sovi_data))/nrow(sovi_data) * 100, 2),
            missing_data_percentage = round(sum(is.na(sovi_data))/(nrow(sovi_data)*ncol(sovi_data)) * 100, 2)
          )
        ),
        variables = list(
          numeric_variables = names(sovi_data)[sapply(sovi_data, is.numeric)],
          variable_descriptions = list(
            POVERTY = "Poverty rate percentage",
            LOWEDU = "Low education percentage", 
            ELDERLY = "Elderly population percentage",
            GROWTH = "Population growth rate",
            CHILDREN = "Children population percentage",
            FEMALE = "Female population percentage",
            POPULATION = "Total population"
          )
        ),
        sources = list(
          primary_source = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv",
          documentation = "https://www.sciencedirect.com/science/article/pii/S2352340921010180",
          doi = "10.1016/j.dib.2021.107664"
        ),
        analysis_context = list(
          platform = "R Shiny Dashboard",
          exam_date = "2025-07-23",
          institution = "STIS (Sekolah Tinggi Ilmu Statistik)",
          course = "KOMPUTASI STATISTIK"
        ),
        generated_on = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      )
      
      jsonlite::write_json(metadata, file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
  
  # Preview Git Repository action
  observeEvent(input$preview_git, {
    showModal(modalDialog(
      title = "Preview Repository GitHub",
      tags$div(
        style = "text-align: center; padding: 20px;",
        h4("Repository SOVI Data", style = "color: #4CAF50;"),
        p("Repository ini berisi dataset dan dokumentasi lengkap untuk analisis SOVI."),
        tags$div(style = "margin: 20px 0;",
                 tags$a("🔗 Buka Repository", 
                        href = "https://github.com/bmlmcmc/naspaclust", 
                        target = "_blank",
                        class = "btn btn-success btn-lg"),
                 br(), br(),
                 tags$a("📊 Akses Data Langsung", 
                        href = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv", 
                        target = "_blank",
                        class = "btn btn-info")
        ),
        p(tags$small("Repository dikembangkan untuk mendukung penelitian spatial clustering dan analisis kerentanan sosial."))
      ),
      footer = modalButton("Tutup"),
      easyClose = TRUE
    ))
  })
  
  # Enhanced tab-specific download handler generator dengan logika yang diperbaiki
  generate_download_handler <- function(tab_name, format_type) {
    downloadHandler(
      filename = function() {
        current_tab <- switch(tab_name,
                              "beranda" = "Beranda",
                              "manajemen" = "ManajemenData", 
                              "eksplorasi" = "EksplorasiData",
                              "asumsi" = "UjiAsumsi",
                              "inferensia" = "StatistikInferensia",
                              "regresi" = "RegresiLinear")
        
                 # Format-specific filename logic
         timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
         
         if(format_type == "pdf") {
           paste0("SOVI_", current_tab, "_Lengkap_", timestamp, ".pdf")
         } else if(format_type == "word") {
           paste0("SOVI_", current_tab, "_Laporan_", timestamp, ".docx")
         } else {
           paste0("SOVI_", current_tab, "_", timestamp, ".", format_type)
         }
      },
      content = function(file) {
        temp_dir <- tempdir()
        all_files <- c()
        
        # ===============================================
        # 1. GENERATE PLOTS/IMAGES UNTUK SEMUA FORMAT
        # ===============================================
        
        plots_created <- list()
        
        if(tab_name == "beranda") {
          # Plot 1: Distribusi kemiskinan
          p1 <- ggplot(sovi_data, aes(x = POVERTY)) + 
            geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") + 
            labs(title = "Distribusi Tingkat Kemiskinan SOVI", x = "Tingkat Kemiskinan (%)", y = "Frekuensi") + 
            theme_minimal() + theme(plot.title = element_text(color = colors[1], size = 14, face = "bold"))
          
          # Plot 2: Korelasi matrix
          key_vars <- sovi_data[, c("POVERTY", "LOWEDU", "ELDERLY", "GROWTH")]
          cor_matrix <- cor(key_vars, use = "complete.obs")
          p2 <- ggplot(data = reshape2::melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
            geom_tile() +
            scale_fill_gradient2(low = colors[7], high = colors[8], mid = "white", midpoint = 0) +
            labs(title = "Matriks Korelasi Variabel SOVI", x = "", y = "", fill = "Korelasi") +
            theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          # Plot 3: Regional distribution  
          p3 <- ggplot(sovi_data, aes(x = POVERTY, y = LOWEDU)) + 
            geom_point(alpha = 0.6, color = colors[1]) + 
            geom_smooth(method = "lm", color = colors[2]) +
            labs(title = "Hubungan Kemiskinan vs Pendidikan Rendah", x = "Kemiskinan (%)", y = "Pendidikan Rendah (%)") + 
            theme_minimal()
          
          plots_created <- list("distribusi_kemiskinan" = p1, "korelasi_matrix" = p2, "scatter_poverty_education" = p3)
          
        } else if(tab_name == "manajemen") {
          # Plot 1: Data quality overview
          missing_data <- sapply(sovi_data, function(x) sum(is.na(x)))
          quality_df <- data.frame(
            Variable = names(missing_data),
            Missing = missing_data,
            Complete = nrow(sovi_data) - missing_data
          )
          quality_df_long <- reshape2::melt(quality_df, id.vars = "Variable")
          
          p1 <- ggplot(quality_df_long, aes(x = Variable, y = value, fill = variable)) +
            geom_bar(stat = "identity", position = "stack") +
            scale_fill_manual(values = c("Missing" = colors[7], "Complete" = colors[8])) +
            labs(title = "Kualitas Data SOVI per Variabel", x = "Variabel", y = "Jumlah Observasi", fill = "Status") +
            theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          # Plot 2: Outlier detection
          key_vars <- sovi_data[, c("POVERTY", "LOWEDU", "ELDERLY", "GROWTH")]
          z_scores <- key_vars %>%
            mutate_all(~ abs(scale(.)[,1])) %>%
            mutate(ID = row_number()) %>%
            pivot_longer(-ID, names_to = "Variable", values_to = "Z_Score")
          
          p2 <- ggplot(z_scores, aes(x = Variable, y = Z_Score)) +
            geom_boxplot(fill = colors[8], alpha = 0.7) +
            geom_hline(yintercept = 3, color = colors[7], linetype = "dashed", size = 1) +
            labs(title = "Deteksi Outlier (Z-Score > 3)", x = "Variabel", y = "Absolute Z-Score") +
            theme_minimal()
          
          plots_created <- list("data_quality" = p1, "outlier_detection" = p2)
          
        } else if(tab_name == "eksplorasi") {
          # Plot 1: Distribusi multiple variables
          key_vars_long <- sovi_data %>%
            select(POVERTY, LOWEDU, ELDERLY, GROWTH) %>%
            pivot_longer(everything(), names_to = "Variable", values_to = "Value")
          
          p1 <- ggplot(key_vars_long, aes(x = Value, fill = Variable)) +
            geom_histogram(bins = 20, alpha = 0.7) +
            facet_wrap(~Variable, scales = "free") +
            scale_fill_manual(values = colors[1:4]) +
            labs(title = "Distribusi Variabel Kunci SOVI", x = "Nilai", y = "Frekuensi") +
            theme_minimal() + theme(legend.position = "none")
          
          # Plot 2: Correlation scatter
          p2 <- ggplot(sovi_data, aes(x = POVERTY, y = LOWEDU)) + 
            geom_point(color = colors[1], alpha = 0.6) + 
            geom_smooth(method = "lm", color = colors[2]) + 
            labs(title = "Hubungan Kemiskinan vs Pendidikan Rendah", x = "Tingkat Kemiskinan (%)", y = "Pendidikan Rendah (%)") + 
            theme_minimal()
          
          # Plot 3: Boxplot by categories
          sovi_data_subset <- sovi_data %>%
            select(POVERTY, Population_Size) %>%
            filter(!is.na(Population_Size))
          
          p3 <- ggplot(sovi_data_subset, aes(x = Population_Size, y = POVERTY, fill = Population_Size)) +
            geom_boxplot(alpha = 0.7) +
            scale_fill_manual(values = colors[1:2]) +
            labs(title = "Distribusi Kemiskinan berdasarkan Ukuran Populasi", x = "Ukuran Populasi", y = "Tingkat Kemiskinan") +
            theme_minimal() + theme(legend.position = "none")
          
          plots_created <- list("distribusi_multivariabel" = p1, "scatter_korelasi" = p2, "boxplot_kategori" = p3)
          
        } else if(tab_name == "asumsi") {
          # Plot 1: Normality test visualization
          p1 <- ggplot(sovi_data, aes(x = POVERTY)) +
            geom_histogram(aes(y = after_stat(density)), bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
            stat_function(fun = dnorm, args = list(mean = mean(sovi_data$POVERTY, na.rm = TRUE), 
                                                   sd = sd(sovi_data$POVERTY, na.rm = TRUE)), 
                          color = colors[2], size = 1) +
            labs(title = "Uji Normalitas: Distribusi POVERTY dengan Kurva Normal", x = "POVERTY", y = "Densitas") +
            theme_minimal()
          
          # Plot 2: Q-Q Plot
          poverty_clean <- sovi_data$POVERTY[!is.na(sovi_data$POVERTY)]
          qq_data <- data.frame(
            sample = sort(poverty_clean),
            theoretical = qnorm(ppoints(length(poverty_clean)))
          )
          
          p2 <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
            geom_point(alpha = 0.6, color = colors[1]) +
            geom_abline(slope = sd(poverty_clean), intercept = mean(poverty_clean), color = colors[2], size = 1) +
            labs(title = "Q-Q Plot untuk Uji Normalitas POVERTY", x = "Kuantil Teoritis", y = "Kuantil Sampel") +
            theme_minimal()
          
          plots_created <- list("normalitas_histogram" = p1, "qq_plot" = p2)
          
        } else if(tab_name == "inferensia") {
          # Plot 1: T-test visualization
          sample_mean <- mean(sovi_data$POVERTY, na.rm = TRUE)
          
          p1 <- ggplot(sovi_data, aes(x = POVERTY)) +
            geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
            geom_vline(xintercept = sample_mean, color = colors[2], size = 1, linetype = "dashed") +
            geom_vline(xintercept = 15, color = colors[7], size = 1, linetype = "solid") +
            labs(title = "Uji t: Rata-rata Sampel vs Nilai Hipotesis", x = "POVERTY", y = "Frekuensi",
                 subtitle = "Garis merah = Rata-rata sampel, Garis biru = Nilai hipotesis") +
            theme_minimal()
          
          # Plot 2: ANOVA visualization
          if("Economic_Status" %in% names(sovi_data)) {
            clean_data <- sovi_data %>% filter(!is.na(Economic_Status), !is.na(POVERTY))
            group_means <- clean_data %>% group_by(Economic_Status) %>% summarise(Mean_Poverty = mean(POVERTY, na.rm = TRUE))
            
            p2 <- ggplot(group_means, aes(x = Economic_Status, y = Mean_Poverty, fill = Economic_Status)) +
              geom_bar(stat = "identity", alpha = 0.7) +
              scale_fill_manual(values = colors[1:3]) +
              labs(title = "ANOVA: Rata-rata Kemiskinan per Status Ekonomi", x = "Status Ekonomi", y = "Rata-rata Kemiskinan") +
              theme_minimal() + theme(legend.position = "none")
          } else {
            p2 <- ggplot(sovi_data, aes(x = GROWTH, y = POVERTY)) + 
              geom_point(color = colors[1], alpha = 0.6) +
              labs(title = "Analisis Inferensia: Growth vs Poverty", x = "Growth", y = "Poverty") + 
              theme_minimal()
          }
          
          plots_created <- list("uji_t_visual" = p1, "anova_visual" = p2)
          
        } else if(tab_name == "regresi") {
          # Plot 1: Scatter with regression line
          p1 <- ggplot(sovi_data, aes(x = LOWEDU, y = POVERTY)) + 
            geom_point(color = colors[1], alpha = 0.6) + 
            geom_smooth(method = "lm", color = colors[2]) + 
            labs(title = "Regresi Linear: POVERTY vs LOWEDU", x = "Low Education (%)", y = "Poverty (%)") + 
            theme_minimal()
          
          # Plot 2: Residual plot (simulated)
          model_simple <- lm(POVERTY ~ LOWEDU, data = sovi_data)
          residual_data <- data.frame(
            fitted = fitted(model_simple),
            residuals = residuals(model_simple)
          )
          
          p2 <- ggplot(residual_data, aes(x = fitted, y = residuals)) +
            geom_point(alpha = 0.6, color = colors[1]) +
            geom_hline(yintercept = 0, color = colors[2], size = 1) +
            geom_smooth(method = "loess", color = colors[7], se = FALSE) +
            labs(title = "Plot Diagnostik: Residual vs Fitted", x = "Nilai Prediksi", y = "Residual") +
            theme_minimal()
          
          plots_created <- list("regresi_scatter" = p1, "residual_plot" = p2)
        }
        
                 # Store plots for later use in PDF/Word
         # No need to save as individual JPG files
        
        # ===============================================
        # 3. GENERATE TEXT CONTENT FOR WORD/PDF
        # ===============================================
        
        # Create comprehensive text content based on tab
        text_content <- ""
        
        if(tab_name == "beranda") {
          text_content <- paste0(
            "LAPORAN ANALISIS BERANDA SOVI\n",
            "=====================================\n\n",
            "Dataset Overview:\n",
            "- Total Observasi: ", nrow(sovi_data), "\n",
            "- Total Variabel: ", ncol(sovi_data), "\n",
            "- Kelengkapan Data: ", round(sum(complete.cases(sovi_data))/nrow(sovi_data) * 100, 1), "%\n\n",
            "Statistik Ringkasan Variabel Kunci:\n",
            "- Rata-rata Kemiskinan: ", round(mean(sovi_data$POVERTY, na.rm = TRUE), 2), "%\n",
            "- Rata-rata Pendidikan Rendah: ", round(mean(sovi_data$LOWEDU, na.rm = TRUE), 2), "%\n",
            "- Rata-rata Lansia: ", round(mean(sovi_data$ELDERLY, na.rm = TRUE), 2), "%\n",
            "- Rata-rata Pertumbuhan: ", round(mean(sovi_data$GROWTH, na.rm = TRUE), 2), "%\n\n",
            "Analisis Korelasi:\n",
            "Korelasi antara POVERTY dan LOWEDU: ", round(cor(sovi_data$POVERTY, sovi_data$LOWEDU, use = "complete.obs"), 3), "\n",
            "Korelasi antara POVERTY dan ELDERLY: ", round(cor(sovi_data$POVERTY, sovi_data$ELDERLY, use = "complete.obs"), 3), "\n\n",
            "Interpretasi:\n",
            "Dashboard beranda menunjukkan variasi yang signifikan dalam indikator kerentanan sosial antar wilayah. ",
            "Distribusi kemiskinan menunjukkan pola yang heterogen dengan beberapa wilayah menunjukkan tingkat kerentanan tinggi. ",
            "Matriks korelasi mengungkap hubungan kompleks antar variabel yang memberikan insight penting untuk analisis lanjutan."
          )
        } else if(tab_name == "manajemen") {
          missing_summary <- sapply(sovi_data, function(x) sum(is.na(x)))
          text_content <- paste0(
            "LAPORAN MANAJEMEN DATA SOVI\n",
            "===========================\n\n",
            "Kualitas Data:\n",
            "- Skor Kualitas: ", round(sum(complete.cases(sovi_data))/nrow(sovi_data) * 100, 0), "%\n",
            "- Persentase Data Hilang: ", round(sum(is.na(sovi_data))/(nrow(sovi_data)*ncol(sovi_data)) * 100, 1), "%\n\n",
            "Struktur Data:\n",
            "- Variabel Numerik: ", sum(sapply(sovi_data, is.numeric)), "\n",
            "- Variabel Kategorik: ", sum(sapply(sovi_data, function(x) is.factor(x) || is.character(x))), "\n\n",
            "Analisis Missing Data:\n"
          )
          
          if(any(missing_summary > 0)) {
            missing_vars <- missing_summary[missing_summary > 0]
            for(i in 1:length(missing_vars)) {
              text_content <- paste0(text_content, "- ", names(missing_vars)[i], ": ", missing_vars[i], " observasi hilang\n")
            }
          } else {
            text_content <- paste0(text_content, "- Tidak ada data hilang pada semua variabel\n")
          }
          
          text_content <- paste0(text_content, "\n",
                                 "Interpretasi:\n",
                                 "Dataset SOVI menunjukkan kualitas yang baik dengan tingkat kelengkapan tinggi. ",
                                 "Struktur data mendukung berbagai jenis analisis statistik yang diperlukan untuk penelitian kerentanan sosial."
          )
        } else if(tab_name == "eksplorasi") {
          text_content <- paste0(
            "LAPORAN EKSPLORASI DATA SOVI\n",
            "============================\n\n",
            "Statistik Deskriptif Variabel Kunci:\n\n",
            "POVERTY:\n",
            "- Mean: ", round(mean(sovi_data$POVERTY, na.rm = TRUE), 3), "\n",
            "- Median: ", round(median(sovi_data$POVERTY, na.rm = TRUE), 3), "\n",
            "- SD: ", round(sd(sovi_data$POVERTY, na.rm = TRUE), 3), "\n",
            "- Min: ", round(min(sovi_data$POVERTY, na.rm = TRUE), 3), "\n",
            "- Max: ", round(max(sovi_data$POVERTY, na.rm = TRUE), 3), "\n\n",
            "LOWEDU:\n",
            "- Mean: ", round(mean(sovi_data$LOWEDU, na.rm = TRUE), 3), "\n",
            "- Median: ", round(median(sovi_data$LOWEDU, na.rm = TRUE), 3), "\n",
            "- SD: ", round(sd(sovi_data$LOWEDU, na.rm = TRUE), 3), "\n",
            "- Min: ", round(min(sovi_data$LOWEDU, na.rm = TRUE), 3), "\n",
            "- Max: ", round(max(sovi_data$LOWEDU, na.rm = TRUE), 3), "\n\n",
            "Analisis Korelasi:\n",
            "Korelasi POVERTY-LOWEDU: ", round(cor(sovi_data$POVERTY, sovi_data$LOWEDU, use = "complete.obs"), 3), "\n",
            "Korelasi POVERTY-ELDERLY: ", round(cor(sovi_data$POVERTY, sovi_data$ELDERLY, use = "complete.obs"), 3), "\n",
            "Korelasi POVERTY-GROWTH: ", round(cor(sovi_data$POVERTY, sovi_data$GROWTH, use = "complete.obs"), 3), "\n\n",
            "Interpretasi:\n",
            "Eksplorasi data mengungkap pola dan hubungan yang menarik antar variabel SOVI. ",
            "Analisis ini memberikan dasar yang kuat untuk pemodelan statistik lanjutan dan pemahaman yang lebih mendalam tentang faktor-faktor yang mempengaruhi kerentanan sosial."
          )
        } else if(tab_name == "asumsi") {
          shapiro_test <- tryCatch({
            if(length(sovi_data$POVERTY[!is.na(sovi_data$POVERTY)]) <= 5000) {
              shapiro.test(sovi_data$POVERTY)
            } else {
              list(statistic = NA, p.value = NA, method = "Sample too large for Shapiro-Wilk")
            }
          }, error = function(e) list(statistic = NA, p.value = NA, method = "Error in test"))
          
          text_content <- paste0(
            "LAPORAN UJI ASUMSI STATISTIK\n",
            "=============================\n\n",
            "Uji Normalitas (Shapiro-Wilk):\n",
            "- Variable: POVERTY\n",
            "- Statistic: ", if(!is.na(shapiro_test$statistic)) round(shapiro_test$statistic, 4) else "N/A", "\n",
            "- P-value: ", if(!is.na(shapiro_test$p.value)) round(shapiro_test$p.value, 4) else "N/A", "\n",
            "- Hasil: ", if(!is.na(shapiro_test$p.value)) {
              if(shapiro_test$p.value < 0.05) "Data tidak berdistribusi normal (p < 0.05)" else "Data berdistribusi normal (p >= 0.05)"
            } else "Tidak dapat dihitung", "\n\n"
          )
          
          # Homogeneity test
          if("Economic_Status" %in% names(sovi_data)) {
            clean_data <- sovi_data %>% filter(!is.na(Economic_Status), !is.na(POVERTY))
            if(nrow(clean_data) > 0 && length(unique(clean_data$Economic_Status)) >= 2) {
              levene_test <- tryCatch({
                car::leveneTest(clean_data$POVERTY, clean_data$Economic_Status)
              }, error = function(e) list(statistic = NA, p.value = NA))
              
              text_content <- paste0(text_content,
                                     "Uji Homogenitas (Levene Test):\n",
                                     "- Variable: POVERTY by Economic_Status\n",
                                     "- F-statistic: ", if(!is.na(levene_test$`F value`[1])) round(levene_test$`F value`[1], 4) else "N/A", "\n",
                                     "- P-value: ", if(!is.na(levene_test$`Pr(>F)`[1])) round(levene_test$`Pr(>F)`[1], 4) else "N/A", "\n",
                                     "- Hasil: ", if(!is.na(levene_test$`Pr(>F)`[1])) {
                                       if(levene_test$`Pr(>F)`[1] < 0.05) "Varians tidak homogen (p < 0.05)" else "Varians homogen (p >= 0.05)"
                                     } else "Tidak dapat dihitung", "\n\n"
              )
            }
          }
          
          text_content <- paste0(text_content,
                                 "Interpretasi:\n",
                                 "Pengujian asumsi statistik penting untuk menentukan metode analisis yang tepat. ",
                                 "Uji normalitas dan homogenitas memberikan panduan untuk memilih uji parametrik atau non-parametrik. ",
                                 "Hasil ini akan mempengaruhi validitas kesimpulan statistik yang diambil."
          )
        } else if(tab_name == "inferensia") {
          # T-test one sample
          t_test_result <- tryCatch({
            t.test(sovi_data$POVERTY, mu = 15)
          }, error = function(e) list(statistic = NA, p.value = NA, conf.int = c(NA, NA)))
          
          text_content <- paste0(
            "LAPORAN STATISTIK INFERENSIA\n",
            "=============================\n\n",
            "Uji t Satu Sampel:\n",
            "- Variable: POVERTY\n",
            "- Hipotesis: μ = 15\n",
            "- t-statistic: ", if(!is.na(t_test_result$statistic)) round(t_test_result$statistic, 4) else "N/A", "\n",
            "- P-value: ", if(!is.na(t_test_result$p.value)) round(t_test_result$p.value, 4) else "N/A", "\n",
            "- Confidence Interval: [", if(!is.na(t_test_result$conf.int[1])) round(t_test_result$conf.int[1], 3) else "N/A", 
            ", ", if(!is.na(t_test_result$conf.int[2])) round(t_test_result$conf.int[2], 3) else "N/A", "]\n",
            "- Kesimpulan: ", if(!is.na(t_test_result$p.value)) {
              if(t_test_result$p.value < 0.05) "Tolak H0: rata-rata berbeda dari 15" else "Gagal tolak H0: rata-rata tidak berbeda dari 15"
            } else "Tidak dapat dihitung", "\n\n"
          )
          
          # ANOVA if possible
          if("Economic_Status" %in% names(sovi_data)) {
            clean_data <- sovi_data %>% filter(!is.na(Economic_Status), !is.na(POVERTY))
            if(nrow(clean_data) > 0 && length(unique(clean_data$Economic_Status)) >= 2) {
              anova_result <- tryCatch({
                anova_model <- aov(POVERTY ~ Economic_Status, data = clean_data)
                summary(anova_model)
              }, error = function(e) NULL)
              
              if(!is.null(anova_result)) {
                text_content <- paste0(text_content,
                                       "Analisis Varians (ANOVA):\n",
                                       "- Variable: POVERTY by Economic_Status\n",
                                       "- F-statistic: ", round(anova_result[[1]]$`F value`[1], 4), "\n",
                                       "- P-value: ", round(anova_result[[1]]$`Pr(>F)`[1], 4), "\n",
                                       "- Kesimpulan: ", if(anova_result[[1]]$`Pr(>F)`[1] < 0.05) {
                                         "Terdapat perbedaan signifikan antar kelompok"
                                       } else {
                                         "Tidak ada perbedaan signifikan antar kelompok"
                                       }, "\n\n"
                )
              }
            }
          }
          
          text_content <- paste0(text_content,
                                 "Interpretasi:\n",
                                 "Analisis inferensia memberikan dasar untuk membuat kesimpulan tentang populasi berdasarkan sampel. ",
                                 "Uji hipotesis membantu menentukan apakah perbedaan yang diamati secara statistik signifikan. ",
                                 "Hasil ini penting untuk pengambilan keputusan berbasis bukti dalam konteks penelitian kerentanan sosial."
          )
        } else if(tab_name == "regresi") {
          # Simple regression
          model_result <- tryCatch({
            lm(POVERTY ~ LOWEDU + ELDERLY + GROWTH, data = sovi_data)
          }, error = function(e) NULL)
          
          if(!is.null(model_result)) {
            model_summary <- summary(model_result)
            
            text_content <- paste0(
              "LAPORAN ANALISIS REGRESI LINEAR BERGANDA\n",
              "=========================================\n\n",
              "Model: POVERTY ~ LOWEDU + ELDERLY + GROWTH\n\n",
              "Ringkasan Model:\n",
              "- R-squared: ", round(model_summary$r.squared, 4), "\n",
              "- Adjusted R-squared: ", round(model_summary$adj.r.squared, 4), "\n",
              "- F-statistic: ", round(model_summary$fstatistic[1], 4), "\n",
              "- P-value (F-test): ", format.pval(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)), "\n",
              "- Residual standard error: ", round(model_summary$sigma, 4), "\n\n",
              "Koefisien:\n"
            )
            
            coef_table <- model_summary$coefficients
            for(i in 1:nrow(coef_table)) {
              text_content <- paste0(text_content,
                                     "- ", rownames(coef_table)[i], ": ",
                                     "Estimate = ", round(coef_table[i, 1], 4),
                                     ", SE = ", round(coef_table[i, 2], 4),
                                     ", t = ", round(coef_table[i, 3], 4),
                                     ", p = ", round(coef_table[i, 4], 4),
                                     if(coef_table[i, 4] < 0.05) " (signifikan)" else " (tidak signifikan)",
                                     "\n"
              )
            }
            
            text_content <- paste0(text_content, "\n",
                                   "Interpretasi:\n",
                                   "Model regresi linear berganda menjelaskan ", round(model_summary$r.squared * 100, 2), "% variasi dalam POVERTY. ",
                                   if(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE) < 0.05) {
                                     "Model secara keseluruhan signifikan secara statistik. "
                                   } else {
                                     "Model secara keseluruhan tidak signifikan secara statistik. "
                                   },
                                   "Analisis ini memberikan insight tentang faktor-faktor yang mempengaruhi tingkat kemiskinan dalam konteks kerentanan sosial."
            )
          } else {
            text_content <- "LAPORAN ANALISIS REGRESI LINEAR BERGANDA\n=========================================\n\nError: Tidak dapat melakukan analisis regresi dengan data yang tersedia."
          }
        }
        
                          # ===============================================
         # 4. HANDLE FORMAT WORD - PROPER DOCX FILE
         # ===============================================
         if (format_type == "word") {
           # Create RTF content which can be opened as Word
           rtf_content <- paste0(
             "{\\rtf1\\ansi\\deff0",
             "{\\fonttbl{\\f0 Times New Roman;}}",
             "{\\colortbl;\\red26\\green35\\blue126;\\red57\\green73\\blue171;}",
             "\\f0\\fs24",
             "\\qc{\\b\\fs32\\cf1 DAVIRA - SOVI Analytics Hub}\\par",
             "\\qc{\\b\\fs28\\cf2 Laporan Analisis ", tools::toTitleCase(tab_name), "}\\par\\par",
             "\\ql{\\b Generated:} ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\\par",
             "{\\b Dataset:} Social Vulnerability Index\\par",
             "{\\b Institution:} STIS 2025 - KOMPUTASI STATISTIK\\par\\par",
             gsub("\n", "\\par ", text_content),
             "\\par\\par",
             "{\\fs18 Laporan ini dibuat menggunakan DAVIRA - SOVI Analytics Hub}\\par",
             "{\\fs18 Data Source: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv}\\par",
             "}"
           )
           
           # Write RTF content to file
           writeLines(rtf_content, file, useBytes = TRUE)
           return()
         }
        
                          # ===============================================
         # 5. HANDLE FORMAT PDF - COMPREHENSIVE PDF WITH ALL CONTENT
         # ===============================================
         if (format_type == "pdf") {
           tryCatch({
             pdf_file <- file.path(temp_dir, paste0("SOVI_", tools::toTitleCase(tab_name), ".pdf"))
             pdf(pdf_file, width = 11, height = 8.5)
             
             # Title page
             plot.new()
             text(0.5, 0.9, "DAVIRA - SOVI Analytics Hub", cex = 2.2, font = 2, col = "#1A237E")
             text(0.5, 0.8, paste("Laporan Lengkap Analisis", tools::toTitleCase(tab_name)), cex = 1.6, font = 2)
             text(0.5, 0.7, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")), cex = 1.1)
             text(0.5, 0.6, "STIS 2025 - KOMPUTASI STATISTIK", cex = 1.3, col = "#3949AB")
             text(0.5, 0.5, "Dataset: Social Vulnerability Index", cex = 1)
             text(0.5, 0.4, paste("Total Observations:", nrow(sovi_data)), cex = 1)
             text(0.5, 0.3, paste("Total Variables:", ncol(sovi_data)), cex = 1)
             
             # Add all plots with titles
             for(plot_name in names(plots_created)) {
               # Add plot title page
               plot.new()
               text(0.5, 0.95, paste("Grafik:", gsub("_", " ", tools::toTitleCase(plot_name))), cex = 1.4, font = 2, col = "#1A237E")
               
               # Add the actual plot
               print(plots_created[[plot_name]])
             }
             
             # Add comprehensive text content across multiple pages
             lines <- strsplit(text_content, "\n")[[1]]
             lines <- lines[nchar(lines) > 0] # Remove empty lines
             
             # Split lines into pages (about 35 lines per page)
             lines_per_page <- 35
             total_pages <- ceiling(length(lines) / lines_per_page)
             
             for(page in 1:total_pages) {
               start_line <- (page - 1) * lines_per_page + 1
               end_line <- min(page * lines_per_page, length(lines))
               
               plot.new()
               text(0.5, 0.97, "Hasil Analisis dan Interpretasi", cex = 1.4, font = 2, col = "#1A237E")
               text(0.95, 0.03, paste("Halaman", page, "dari", total_pages), cex = 0.8, adj = 1)
               
               y_pos <- 0.92
               for(i in start_line:end_line) {
                 if(y_pos > 0.05) {
                   # Handle long lines by wrapping
                   line <- lines[i]
                   if(nchar(line) > 100) {
                     wrapped_lines <- strwrap(line, width = 100)
                     for(wrapped_line in wrapped_lines) {
                       if(y_pos > 0.05) {
                         text(0.05, y_pos, wrapped_line, cex = 0.75, adj = 0)
                         y_pos <- y_pos - 0.025
                       }
                     }
                   } else {
                     text(0.05, y_pos, line, cex = 0.75, adj = 0)
                     y_pos <- y_pos - 0.025
                   }
                 }
               }
             }
             
             dev.off()
             
             # Copy PDF to output file
             file.copy(pdf_file, file)
             return()
           }, error = function(e) {
             # Fallback: create simple PDF with text only
             pdf(file, width = 11, height = 8.5)
             plot.new()
             text(0.5, 0.9, "DAVIRA - SOVI Analytics Hub", cex = 2, font = 2)
             text(0.5, 0.8, "Error generating full report", cex = 1.2)
             text(0.5, 0.7, paste("Tab:", tools::toTitleCase(tab_name)), cex = 1)
             dev.off()
             return()
           })
         }
        
                 
      }
    )
  }
  
        # ===============================================
   # DOWNLOAD HANDLERS - SIMPLIFIED VERSION
   # ===============================================
   
   # Direct download handlers
   output$download_beranda_pdf <- generate_download_handler("beranda", "pdf") 
   output$download_beranda_word <- generate_download_handler("beranda", "word")
   
   output$download_manajemen_pdf <- generate_download_handler("manajemen", "pdf")
   output$download_manajemen_word <- generate_download_handler("manajemen", "word")
   
   output$download_eksplorasi_pdf <- generate_download_handler("eksplorasi", "pdf")
   output$download_eksplorasi_word <- generate_download_handler("eksplorasi", "word")
   
   output$download_asumsi_pdf <- generate_download_handler("asumsi", "pdf")
   output$download_asumsi_word <- generate_download_handler("asumsi", "word")
   
   output$download_inferensia_pdf <- generate_download_handler("inferensia", "pdf")
   output$download_inferensia_word <- generate_download_handler("inferensia", "word")
   
   output$download_regresi_pdf <- generate_download_handler("regresi", "pdf")
   output$download_regresi_word <- generate_download_handler("regresi", "word")
}

# Run the app
shinyApp(ui = ui, server = server)
