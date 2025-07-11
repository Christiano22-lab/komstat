library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(lmtest)
library(car)

# Fungsi untuk menghapus pencilan menggunakan IQR
remove_outliers <- function(data, variable) {
  cat("\nMengolah variabel:", variable, "\n")
  cat("Jenis data:", class(data[[variable]]), "\n")
  Q1 <- quantile(data[[variable]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[variable]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  cat("Batas bawah IQR:", lower_bound, "\n")
  cat("Batas atas IQR:", upper_bound, "\n")
  data <- data %>% filter(data[[variable]] >= lower_bound & data[[variable]] <= upper_bound)
  cat("Pencilan pada variabel", variable, "telah dihapus menggunakan metode IQR.\n")
  return(data)
}

# Fungsi untuk memproses data contoh dengan multiple Y variables
process_data <- function(data) {
  # Asumsi kolom: Tanggal_Y, Beras_Y, Beras_Y1, Beras_Y2, Beras_Y3, Beras_Y4, Beras_Y5, Beras_Y6, Tanggal_X, Tavg_X1, RR_X2
  expected_cols <- c("Tanggal_Y", "Beras_Y", "Beras_Y1", "Beras_Y2", "Beras_Y3", "Beras_Y4", "Beras_Y5", "Beras_Y6", "Tanggal_X", "Tavg_X1", "RR_X2")
  
  if (ncol(data) >= length(expected_cols)) {
    colnames(data)[1:length(expected_cols)] <- expected_cols
  } else {
    # Fallback untuk data dengan struktur berbeda
    colnames(data) <- c("Tanggal_Y", "Beras_Y", "Tanggal_X", "Tavg_X1", "RR_X2")[1:ncol(data)]
  }
  
  data$Tanggal_Y <- as.Date(data$Tanggal_Y, format = "%d-%m-%Y")
  data$Tanggal_X <- as.Date(data$Tanggal_X, format = "%d-%m-%Y")
  
  # Pilih kolom yang tersedia
  available_cols <- intersect(expected_cols, colnames(data))
  data_clean <- data %>% select(all_of(available_cols))
  
  data_clean <- data_clean %>% filter(!is.na(Tanggal_Y) & !is.na(Tanggal_X))
  merged_data <- merge(data_clean, data_clean, by.x = "Tanggal_X", by.y = "Tanggal_Y", suffixes = c("", "_X"))
  
  final_data_cleaned <- merged_data %>% filter(Tavg_X1 != 888 & Tavg_X1 != 999 & RR_X2 != 888 & RR_X2 != 999)
  
  # Convert Y variables to numeric
  y_vars <- c("Beras_Y", "Beras_Y1", "Beras_Y2", "Beras_Y3", "Beras_Y4", "Beras_Y5", "Beras_Y6")
  available_y_vars <- intersect(y_vars, colnames(final_data_cleaned))
  
  for (var in available_y_vars) {
    final_data_cleaned[[var]] <- as.numeric(gsub(",", "", final_data_cleaned[[var]]))
  }
  
  final_data_cleaned$Tavg_X1 <- as.numeric(final_data_cleaned$Tavg_X1)
  final_data_cleaned$RR_X2 <- as.numeric(final_data_cleaned$RR_X2)
  
  # Remove outliers for all Y variables and X variables
  for (var in available_y_vars) {
    if (var %in% colnames(final_data_cleaned)) {
      final_data_cleaned <- remove_outliers(final_data_cleaned, var)
    }
  }
  
  final_data_cleaned <- final_data_cleaned %>%
    remove_outliers(., "Tavg_X1") %>%
    remove_outliers(., "RR_X2")
  
  # Select relevant columns
  final_cols <- c(available_y_vars, "Tavg_X1", "RR_X2")
  final_data_no_outliers <- final_data_cleaned %>% select(all_of(intersect(final_cols, colnames(final_data_cleaned))))
  
  return(final_data_no_outliers)
}

# Fungsi untuk memproses data pengguna dengan variabel yang dapat disesuaikan
process_user_custom_data <- function(data, num_vars, var_labels) {
  expected_cols <- num_vars + 1
  if (ncol(data) < expected_cols) {
    stop(paste("Data harus memiliki minimal", expected_cols, "kolom (Y dan", num_vars, "variabel X)"))
  }
  
  selected_data <- data[, 1:expected_cols]
  col_names <- c("Y", paste0("X", 1:num_vars))
  colnames(selected_data) <- col_names
  
  for (col in col_names) {
    selected_data[[col]] <- as.numeric(selected_data[[col]])
  }
  
  selected_data <- selected_data %>% filter(complete.cases(.))
  
  for (col in col_names) {
    selected_data <- remove_outliers(selected_data, col)
  }
  
  return(selected_data)
}

# UI: Bagian Antarmuka Pengguna
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Regresi"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "home", icon = icon("home")),
      menuItem("Data Contoh", tabName = "example_data", icon = icon("database")),
      menuItem("Import Data Custom", tabName = "import_custom", icon = icon("file-excel")),
      menuItem("Statistik Deskriptif", tabName = "desc_stats", icon = icon("chart-bar")),
      menuItem("Visualisasi Data", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Model Regresi & Hasil", tabName = "regression", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # Halaman Beranda
      tabItem(tabName = "home",
              h2("Selamat datang di Dashboard Analisis"),
              p("Dashboard ini memungkinkan Anda untuk menganalisis hubungan antara berbagai faktor dengan variabel yang ingin Anda teliti."),
              p("Fitur utama:"),
              p("1. Lihat contoh pengolahan data pada tab 'Data Contoh' dengan pilihan berbagai jenis beras."),
              p("2. Unggah data custom Anda dengan variabel yang dapat disesuaikan pada tab 'Import Data Custom'."),
              p("3. Analisis statistik, visualisasi, dan model regresi linier tersedia pada tab lainnya."),
              
              br(),
              h4("Panduan Penggunaan:"),
              p("Unduh panduan lengkap untuk menggunakan dashboard ini:"),
              downloadButton("downloadGuidebook", "Unduh Guidebook (PDF)", class = "btn-primary"),
              br(),
              br(),
              h4("Video Tutorial:"),
              tags$iframe(width = "560", height = "315", src = "https://www.youtube.com/embed/S5Tk0lxBLfA?si=DTkoaFLNXQ-3fAb2", 
                          frameborder = "0", allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture", 
                          allowfullscreen = TRUE, 
                          style = "max-width: 100%"),
              br(),
              h4("Cara Menggunakan Dashboard:"),
              p("• Pilih jenis beras yang ingin dianalisis pada data contoh"),
              p("• Atau tentukan berapa banyak faktor (variabel X) yang ingin Anda analisis untuk data custom"),
              p("• Berikan nama yang mudah dipahami untuk setiap variabel"),
              p("• Siapkan file Excel dengan format: Y, X1, X2, X3, ... "),
              p("• Unggah file dan lihat hasil analisisnya!")
      ),
      
      # Halaman Data Contoh
      tabItem(tabName = "example_data",
              h3("Contoh Data yang Telah Diproses"),
              p("Ini adalah contoh data yang menunjukkan hubungan antara harga berbagai jenis beras dengan suhu rata-rata dan curah hujan."),
              fluidRow(
                box(title = "Pilih Jenis Beras untuk Analisis", status = "info", solidHeader = TRUE, width = 12,
                    p("Pilih jenis beras yang ingin Anda analisis hubungannya dengan faktor cuaca:"),
                    uiOutput("y_variable_selector"),
                    br(),
                    p(em("Setiap jenis beras memiliki karakteristik harga yang berbeda dan mungkin merespons faktor cuaca dengan cara yang berbeda pula."))
                )
              ),
              fluidRow(
                box(title = "Data Contoh yang Telah Dibersihkan", status = "primary", solidHeader = TRUE,
                    tableOutput("exampleCleanedData"),
                    width = 12)
              )
      ),
      
      # Halaman Import Data Custom
      tabItem(tabName = "import_custom",
              h3("Unggah Data Anda Sendiri"),
              p("Ikuti langkah-langkah berikut untuk menganalisis data Anda:"),
              
              fluidRow(
                box(title = "Langkah 1: Tentukan Jumlah Faktor", status = "info", solidHeader = TRUE, width = 12,
                    p("Berapa banyak faktor (variabel independen) yang ingin Anda analisis pengaruhnya?"),
                    numericInput("num_vars", "Jumlah Faktor yang Akan Dianalisis:", 
                                 value = 2, min = 1, max = 10, step = 1),
                    p(em("Contoh: Jika ingin menganalisis pengaruh suhu dan curah hujan terhadap harga beras, maka pilih 2"))
                )
              ),
              
              fluidRow(
                box(title = "Langkah 2: Beri Nama untuk Setiap Variabel", status = "warning", solidHeader = TRUE, width = 12,
                    p("Berikan nama yang mudah dipahami untuk setiap variabel:"),
                    uiOutput("variable_labels_ui"),
                    p(em("Tips: Gunakan nama yang jelas seperti 'Harga Beras', 'Suhu Rata-rata', 'Curah Hujan', dll."))
                )
              ),
              
              fluidRow(
                box(title = "Langkah 3: Unggah File Excel", status = "success", solidHeader = TRUE, width = 12,
                    p("Format file Excel yang dibutuhkan:"),
                    p("• Kolom pertama: Data variabel yang ingin diprediksi (Y)"),
                    p("• Kolom selanjutnya: Data faktor-faktor yang mempengaruhi (X1, X2, X3, ...)"),
                    p("• Semua data harus berupa angka"),
                    br(),
                    fileInput("file_custom", "Pilih File Excel:", accept = ".xlsx"),
                    actionButton("process_custom_data", "Proses Data", class = "btn-success btn-lg")
                )
              ),
              
              fluidRow(
                box(title = "Informasi Variabel Anda", status = "primary", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("variable_info")
                )
              ),
              
              fluidRow(
                box(title = "Data Anda yang Telah Dibersihkan", status = "primary", solidHeader = TRUE,
                    tableOutput("customCleanedData"),
                    width = 12)
              )
      ),
      
      # Halaman Statistik Deskriptif
      tabItem(tabName = "desc_stats",
              h3("Ringkasan Statistik Data"),
              p("Lihat ringkasan statistik dasar dari data yang Anda pilih:"),
              radioButtons("data_choice_stats", "Pilih Data:", 
                           choices = c("Data Contoh" = "example", 
                                       "Data Saya" = "custom"),
                           selected = "example"),
              fluidRow(
                box(title = "Ringkasan Statistik", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("descStats"),
                    width = 12)
              )
      ),
      
      # Halaman Visualisasi Data
      tabItem(tabName = "visualization",
              h3("Grafik dan Visualisasi"),
              p("Lihat berbagai grafik untuk memahami pola data Anda:"),
              radioButtons("data_choice_viz", "Pilih Data:", 
                           choices = c("Data Contoh" = "example", 
                                       "Data Saya" = "custom"),
                           selected = "example"),
              fluidRow(
                box(title = "Grafik Residual vs Prediksi", status = "primary", solidHeader = TRUE,
                    p("Grafik ini menunjukkan seberapa baik model memprediksi data. Titik-titik yang tersebar acak menunjukkan model yang baik."),
                    plotOutput("residualsPlot", height = 300),
                    width = 12)
              ),
              fluidRow(
                box(title = "Distribusi Kesalahan Prediksi", status = "warning", solidHeader = TRUE,
                    p("Grafik ini menunjukkan distribusi kesalahan prediksi. Bentuk seperti lonceng menunjukkan model yang baik."),
                    plotOutput("histResiduals", height = 300),
                    width = 12)
              ),
              fluidRow(
                box(title = "Perbandingan Nilai Asli vs Prediksi", status = "success", solidHeader = TRUE,
                    p("Grafik ini membandingkan nilai asli dengan hasil prediksi. Semakin dekat ke garis merah, semakin akurat prediksinya."),
                    plotOutput("predictedVsActual", height = 300),
                    width = 12)
              )
      ),
      
      # Halaman Model Regresi & Hasil
      tabItem(tabName = "regression",
              h3("Hasil Analisis dan Kesimpulan"),
              p("Lihat hasil analisis lengkap dan interpretasi yang mudah dipahami:"),
              radioButtons("data_choice_reg", "Pilih Data:", 
                           choices = c("Data Contoh" = "example", 
                                       "Data Saya" = "custom"),
                           selected = "example"),
              fluidRow(
                box(title = "Detail Teknis Model", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("regressionSummary"),
                    width = 12)
              ),
              fluidRow(
                box(title = "Kesimpulan dan Interpretasi", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("regressionInterpretation"),
                    width = 12)
              )
      )
    )
  )
)

# Server: Bagian Fungsi untuk Menangani Logika Aplikasi
server <- function(input, output, session) {
  
  # Reactive values untuk menyimpan data
  example_data <- reactiveVal(NULL)
  custom_data <- reactiveVal(NULL)
  custom_labels <- reactiveVal(NULL)
  custom_num_vars <- reactiveVal(2)
  selected_y_var <- reactiveVal("Beras_Y")
  
  # Memuat dan memproses data contoh
  observe({
    tryCatch({
      df <- read_excel("Data.xlsx")
      example_data(process_data(df))
    }, error = function(e) {
      # Buat data contoh dummy jika file tidak tersedia
      dummy_data <- data.frame(
        Beras_Y = c(12000, 13000, 11500, 14000, 12500, 13500, 11000, 15000, 12800, 13200,
                    11800, 12200, 13800, 14500, 11200, 12900, 13100, 14200, 11900, 13400),
        Beras_Y1 = c(10000, 11000, 9500, 12000, 10500, 11500, 9000, 13000, 10800, 11200,
                     9800, 10200, 11800, 12500, 9200, 10900, 11100, 12200, 9900, 11400),
        Beras_Y2 = c(10500, 11500, 10000, 12500, 11000, 12000, 9500, 13500, 11300, 11700,
                     10300, 10700, 12300, 13000, 9700, 11400, 11600, 12700, 10400, 11900),
        Beras_Y3 = c(13000, 14000, 12500, 15000, 13500, 14500, 12000, 16000, 13800, 14200,
                     12800, 13200, 14800, 15500, 12200, 13900, 14100, 15200, 12900, 14400),
        Beras_Y4 = c(13500, 14500, 13000, 15500, 14000, 15000, 12500, 16500, 14300, 14700,
                     13300, 13700, 15300, 16000, 12700, 14400, 14600, 15700, 13400, 14900),
        Beras_Y5 = c(15000, 16000, 14500, 17000, 15500, 16500, 14000, 18000, 15800, 16200,
                     14800, 15200, 16800, 17500, 14200, 15900, 16100, 17200, 14900, 16400),
        Beras_Y6 = c(16000, 17000, 15500, 18000, 16500, 17500, 15000, 19000, 16800, 17200,
                     15800, 16200, 17800, 18500, 15200, 16900, 17100, 18200, 15900, 17400),
        Tavg_X1 = c(28, 29, 27, 30, 28.5, 29.5, 26, 31, 28.8, 29.2,
                    27.5, 28.2, 30.1, 31.2, 26.8, 29.8, 28.9, 30.5, 27.8, 29.9),
        RR_X2 = c(150, 120, 180, 100, 160, 110, 200, 90, 140, 130,
                  170, 125, 95, 85, 190, 105, 135, 115, 175, 145)
      )
      example_data(dummy_data)
    })
  })
  
  # UI untuk memilih variabel Y
  output$y_variable_selector <- renderUI({
    data <- example_data()
    if (is.null(data)) return(NULL)
    
    # Identifikasi variabel Y yang tersedia
    y_vars <- colnames(data)[grepl("^Beras_Y", colnames(data))]
    
    # Buat pilihan dengan label yang lebih deskriptif
    choices <- list()
    for (var in y_vars) {
      if (var == "Beras_Y") {
        choices[["Beras Umum (Y)"]] <- var
      } else if (var == "Beras_Y1") {
        choices[["Beras Kualitas Bawah I (Y1)"]] <- var
      } else if (var == "Beras_Y2") {
        choices[["Beras Kualitas Bawah II (Y2)"]] <- var
      } else if (var == "Beras_Y3") {
        choices[["Beras Kualitas Medium I (Y3)"]] <- var
      } else if (var == "Beras_Y4") {
        choices[["Beras Kualitas Medium II (Y4)"]] <- var
      } else if (var == "Beras_Y5") {
        choices[["Beras Kualitas Super I (Y5)"]] <- var
      } else if (var == "Beras_Y6") {
        choices[["Beras Kualitas Super II (Y6)"]] <- var
      } else {
        choices[[var]] <- var
      }
    }
    
    selectInput("selected_y_variable", 
                "Pilih Jenis Beras:", 
                choices = choices,
                selected = selected_y_var())
  })
  
  # Update selected Y variable
  observeEvent(input$selected_y_variable, {
    if (!is.null(input$selected_y_variable)) {
      selected_y_var(input$selected_y_variable)
    }
  })
  
  # Handler untuk mengunduh guidebook
  output$downloadGuidebook <- downloadHandler(
    filename = function() {
      paste("Guidebook_Dashboard_Analisis_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      guidebook_path <- "guidebook.pdf"
      
      if (file.exists(guidebook_path)) {
        file.copy(guidebook_path, file)
      } else {
        tryCatch({
          pdf(file, width = 8.5, height = 11)
          plot.new()
          text(0.5, 0.9, "GUIDEBOOK DASHBOARD ANALISIS", cex = 2, font = 2)
          text(0.5, 0.8, "Panduan Penggunaan Dashboard", cex = 1.5)
          text(0.1, 0.7, "1. Pilih jenis beras untuk data contoh", adj = 0)
          text(0.1, 0.65, "2. Atau pilih jumlah faktor untuk data custom", adj = 0)
          text(0.1, 0.6, "3. Beri nama untuk setiap variabel", adj = 0)
          text(0.1, 0.55, "4. Upload file Excel dengan format yang benar", adj = 0)
          text(0.1, 0.5, "5. Lihat hasil analisis di tab yang tersedia", adj = 0)
          text(0.1, 0.4, "Format Data Excel:", font = 2, adj = 0)
          text(0.1, 0.35, "- Kolom 1: Variabel target (Y)", adj = 0)
          text(0.1, 0.3, "- Kolom 2-n: Faktor-faktor (X1, X2, ...)", adj = 0)
          text(0.1, 0.25, "- Semua data harus berupa angka", adj = 0)
          text(0.1, 0.2, "- Tidak boleh ada sel kosong", adj = 0)
          dev.off()
        }, error = function(e) {
          writeLines(c(
            "GUIDEBOOK DASHBOARD ANALISIS",
            "",
            "Panduan Penggunaan:",
            "1. Pilih jenis beras untuk data contoh",
            "2. Atau pilih jumlah faktor untuk data custom",
            "3. Beri nama untuk setiap variabel", 
            "4. Upload file Excel dengan format yang benar",
            "5. Lihat hasil analisis di tab yang tersedia",
            "",
            "Format Data Excel:",
            "- Kolom 1: Variabel target (Y)",
            "- Kolom 2-n: Faktor-faktor (X1, X2, ...)",
            "- Semua data harus berupa angka",
            "- Tidak boleh ada sel kosong"
          ), file)
        })
      }
    },
    contentType = "application/pdf"
  )
  
  # UI dinamis untuk label variabel
  output$variable_labels_ui <- renderUI({
    num_vars <- input$num_vars
    if (is.null(num_vars) || num_vars < 1) return(NULL)
    
    label_inputs <- list()
    
    label_inputs[[1]] <- textInput("label_Y", "Nama untuk Variabel yang Ingin Diprediksi (Y):", 
                                   value = "Variabel Target", placeholder = "Contoh: Harga Beras, Penjualan, dll.")
    
    for (i in 1:num_vars) {
      label_inputs[[i+1]] <- textInput(paste0("label_X", i), 
                                       paste("Nama untuk Faktor", i, ":"), 
                                       value = paste("Faktor", i), 
                                       placeholder = "Contoh: Suhu, Curah Hujan, dll.")
    }
    
    do.call(tagList, label_inputs)
  })
  
  # Menangani unggahan file custom
  observeEvent(input$process_custom_data, {
    req(input$file_custom, input$num_vars)
    file_path <- input$file_custom$datapath
    num_vars <- input$num_vars
    
    labels <- list()
    labels[["Y"]] <- ifelse(is.null(input$label_Y) || input$label_Y == "", "Variabel Target", input$label_Y)
    
    for (i in 1:num_vars) {
      label_input <- paste0("label_X", i)
      labels[[paste0("X", i)]] <- ifelse(is.null(input[[label_input]]) || input[[label_input]] == "", 
                                         paste0("Faktor", i), input[[label_input]])
    }
    
    tryCatch({
      data <- read_excel(file_path)
      processed_data <- process_user_custom_data(data, num_vars, labels)
      custom_data(processed_data)
      custom_labels(labels)
      custom_num_vars(num_vars)
      
      showModal(modalDialog(
        title = "Berhasil! 🎉",
        paste("Data Anda dengan", num_vars, "faktor telah berhasil diproses dan siap dianalisis!"),
        easyClose = TRUE,
        footer = NULL
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Oops! Ada Masalah 😅",
        paste("Gagal memproses data. Pastikan:", 
              "\n• File Excel memiliki format yang benar", 
              "\n• Semua data berupa angka", 
              "\n• Tidak ada sel yang kosong", 
              "\n\nDetail error:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  output$variable_info <- renderText({
    if (!is.null(custom_labels())) {
      labels <- custom_labels()
      info_text <- "✅ Variabel yang Akan Dianalisis:\n\n"
      info_text <- paste0(info_text, "🎯 Target Prediksi: ", labels[["Y"]], "\n")
      info_text <- paste0(info_text, "\n📊 Faktor-faktor yang Mempengaruhi:\n")
      
      x_vars <- names(labels)[names(labels) != "Y"]
      for (i in 1:length(x_vars)) {
        var_name <- x_vars[i]
        info_text <- paste0(info_text, "   ", i, ". ", labels[[var_name]], "\n")
      }
      return(info_text)
    } else {
      return("Belum ada data yang diproses. Silakan unggah file Excel Anda terlebih dahulu.")
    }
  })
  
  # Function to get example data with selected Y variable
  get_example_data_with_selected_y <- reactive({
    data <- example_data()
    y_var <- selected_y_var()
    
    if (is.null(data) || is.null(y_var)) return(NULL)
    
    if (y_var %in% colnames(data)) {
      # Create new dataset with selected Y variable renamed to a standard name
      selected_data <- data %>%
        select(all_of(c(y_var, "Tavg_X1", "RR_X2"))) %>%
        rename(Selected_Y = !!y_var)
      
      return(selected_data)
    }
    
    return(NULL)
  })
  
  output$exampleCleanedData <- renderTable({
    data <- get_example_data_with_selected_y()
    if (is.null(data)) return(NULL)
    
    # Rename columns for display
    display_data <- data
    y_var_name <- selected_y_var()
    
    # Get descriptive name for Y variable
    y_display_name <- switch(y_var_name,
                             "Beras_Y" = "Beras Umum",
                             "Beras_Y1" = "Beras Kualitas Bawah I",
                             "Beras_Y2" = "Beras Kualitas Bawah II", 
                             "Beras_Y3" = "Beras Kualitas Medium I",
                             "Beras_Y4" = "Beras Kualitas Medium II",
                             "Beras_Y5" = "Beras Kualitas Super I",
                             "Beras_Y6" = "Beras Kualitas Super II",
                             y_var_name)
    
    colnames(display_data) <- c(y_display_name, "Suhu Rata-rata (°C)", "Curah Hujan (mm)")
    head(display_data, 10)
  })
  
  output$customCleanedData <- renderTable({
    req(custom_data())
    head(custom_data(), 10)
  })
  
  selected_data_stats <- reactive({
    if (input$data_choice_stats == "example") {
      return(get_example_data_with_selected_y())
    } else if (input$data_choice_stats == "custom") {
      req(custom_data())
      return(custom_data())
    }
    return(NULL)
  })
  
  selected_data_viz <- reactive({
    if (input$data_choice_viz == "example") {
      return(get_example_data_with_selected_y())
    } else if (input$data_choice_viz == "custom") {
      req(custom_data())
      return(custom_data())
    }
    return(NULL)
  })
  
  selected_data_reg <- reactive({
    if (input$data_choice_reg == "example") {
      return(get_example_data_with_selected_y())
    } else if (input$data_choice_reg == "custom") {
      req(custom_data())
      return(custom_data())
    }
    return(NULL)
  })
  
  output$descStats <- renderPrint({
    data <- selected_data_stats()
    if (is.null(data)) {
      cat("Silakan pilih data yang tersedia atau unggah data custom Anda terlebih dahulu.")
      return()
    }
    
    cat("📊 RINGKASAN STATISTIK DATA\n")
    cat(paste(rep("=", 40), collapse = ""), "\n\n")
    
    if (input$data_choice_stats == "example") {
      y_var_name <- selected_y_var()
      y_display_name <- switch(y_var_name,
                               "Beras_Y" = "Beras Umum",
                               "Beras_Y1" = "Beras Kualitas Bawah I",
                               "Beras_Y2" = "Beras Kualitas Bawah II", 
                               "Beras_Y3" = "Beras Kualitas Medium I",
                               "Beras_Y4" = "Beras Kualitas Medium II",
                               "Beras_Y5" = "Beras Kualitas Super I",
                               "Beras_Y6" = "Beras Kualitas Super II",
                               y_var_name)
      
      cat("🎯 Variabel Target:", y_display_name, "\n")
      cat("   Min: Rp", format(min(data$Selected_Y, na.rm = TRUE), big.mark = ","), "\n")
      cat("   Max: Rp", format(max(data$Selected_Y, na.rm = TRUE), big.mark = ","), "\n")
      cat("   Rata-rata: Rp", format(round(mean(data$Selected_Y, na.rm = TRUE), 0), big.mark = ","), "\n")
      cat("   Median: Rp", format(round(median(data$Selected_Y, na.rm = TRUE), 0), big.mark = ","), "\n\n")
      
      cat("🌡 Suhu Rata-rata:\n")
      cat("   Min:", min(data$Tavg_X1, na.rm = TRUE), "°C\n")
      cat("   Max:", max(data$Tavg_X1, na.rm = TRUE), "°C\n")
      cat("   Rata-rata:", round(mean(data$Tavg_X1, na.rm = TRUE), 2), "°C\n")
      cat("   Median:", round(median(data$Tavg_X1, na.rm = TRUE), 2), "°C\n\n")
      
      cat("🌧 Curah Hujan:\n")
      cat("   Min:", min(data$RR_X2, na.rm = TRUE), "mm\n")
      cat("   Max:", max(data$RR_X2, na.rm = TRUE), "mm\n")
      cat("   Rata-rata:", round(mean(data$RR_X2, na.rm = TRUE), 2), "mm\n")
      cat("   Median:", round(median(data$RR_X2, na.rm = TRUE), 2), "mm\n\n")
      
    } else if ("Y" %in% colnames(data) && !is.null(custom_labels())) {
      labels <- custom_labels()
      cat("🎯 Variabel Target:", labels[["Y"]], "\n")
      cat("   Min:", min(data$Y, na.rm = TRUE), "\n")
      cat("   Max:", max(data$Y, na.rm = TRUE), "\n")
      cat("   Rata-rata:", round(mean(data$Y, na.rm = TRUE), 2), "\n")
      cat("   Median:", round(median(data$Y, na.rm = TRUE), 2), "\n\n")
      
      x_vars <- colnames(data)[colnames(data) != "Y"]
      for (var in x_vars) {
        var_label <- labels[[var]]
        cat("📈", var_label, ":\n")
        cat("   Min:", min(data[[var]], na.rm = TRUE), "\n")
        cat("   Max:", max(data[[var]], na.rm = TRUE), "\n")
        cat("   Rata-rata:", round(mean(data[[var]], na.rm = TRUE), 2), "\n")
        cat("   Median:", round(median(data[[var]], na.rm = TRUE), 2), "\n\n")
      }
    } else {
      summary(data)
    }
  })
  
  model_viz <- reactive({
    data <- selected_data_viz()
    if (is.null(data)) return(NULL)
    
    tryCatch({
      if ("Selected_Y" %in% colnames(data)) {
        return(lm(Selected_Y ~ Tavg_X1 + RR_X2, data = data))
      } else if ("Y" %in% colnames(data)) {
        x_vars <- colnames(data)[colnames(data) != "Y"]
        formula_str <- paste("Y ~", paste(x_vars, collapse = " + "))
        return(lm(as.formula(formula_str), data = data))
      }
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  model_reg <- reactive({
    data <- selected_data_reg()
    if (is.null(data)) return(NULL)
    
    tryCatch({
      if ("Selected_Y" %in% colnames(data)) {
        return(lm(Selected_Y ~ Tavg_X1 + RR_X2, data = data))
      } else if ("Y" %in% colnames(data)) {
        x_vars <- colnames(data)[colnames(data) != "Y"]
        formula_str <- paste("Y ~", paste(x_vars, collapse = " + "))
        return(lm(as.formula(formula_str), data = data))
      }
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$residualsPlot <- renderPlot({
    model <- model_viz()
    if (is.null(model)) {
      plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
      text(5, 5, "Silakan pilih data yang tersedia\natau unggah data custom terlebih dahulu", cex=1.2)
      return()
    }
    
    residuals <- model$residuals
    fitted_values <- model$fitted.values
    ggplot(data.frame(fitted = fitted_values, residuals = residuals), aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
      labs(title = "Grafik Residual vs Prediksi", 
           x = "Nilai Prediksi", 
           y = "Residual (Selisih)",
           subtitle = "Titik yang tersebar acak menunjukkan model yang baik") +
      theme_minimal()
  })
  
  output$histResiduals <- renderPlot({
    model <- model_viz()
    if (is.null(model)) {
      plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
      text(5, 5, "Silakan pilih data yang tersedia\natau unggah data custom terlebih dahulu", cex=1.2)
      return()
    }
    
    residuals <- model$residuals
    ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
      geom_histogram(fill = "lightblue", color = "darkblue", alpha = 0.7, bins = 30) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
      labs(title = "Distribusi Kesalahan Prediksi", 
           x = "Kesalahan Prediksi", 
           y = "Frekuensi",
           subtitle = "Bentuk seperti lonceng menunjukkan model yang baik") +
      theme_minimal()
  })
  
  output$predictedVsActual <- renderPlot({
    model <- model_viz()
    data <- selected_data_viz()
    if (is.null(model) || is.null(data)) {
      plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
      text(5, 5, "Silakan pilih data yang tersedia\natau unggah data custom terlebih dahulu", cex=1.2)
      return()
    }
    
    fitted_values <- model$fitted.values
    
    if ("Selected_Y" %in% colnames(data)) {
      y_var_name <- selected_y_var()
      y_display_name <- switch(y_var_name,
                               "Beras_Y" = "Beras Umum",
                               "Beras_Y1" = "Beras Kualitas Bawah I",
                               "Beras_Y2" = "Beras Kualitas Bawah II", 
                               "Beras_Y3" = "Beras Kualitas Medium I",
                               "Beras_Y4" = "Beras Kualitas Medium II",
                               "Beras_Y5" = "Beras Kualitas Super I",
                               "Beras_Y6" = "Beras Kualitas Super II",
                               y_var_name)
      
      ggplot(data, aes(x = Selected_Y, y = fitted_values)) +
        geom_point(alpha = 0.6, color = "darkgreen") +
        geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
        labs(title = "Perbandingan Nilai Asli vs Prediksi", 
             x = paste("Harga Asli", y_display_name, "(Rp)"), 
             y = paste("Prediksi Harga", y_display_name, "(Rp)"),
             subtitle = "Semakin dekat ke garis merah, semakin akurat") +
        theme_minimal()
    } else if ("Y" %in% colnames(data)) {
      y_label <- ifelse(!is.null(custom_labels()), custom_labels()[["Y"]], "Nilai Y")
      ggplot(data, aes(x = Y, y = fitted_values)) +
        geom_point(alpha = 0.6, color = "darkgreen") +
        geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
        labs(title = "Perbandingan Nilai Asli vs Prediksi", 
             x = paste("Nilai Asli", y_label), 
             y = paste("Prediksi", y_label),
             subtitle = "Semakin dekat ke garis merah, semakin akurat") +
        theme_minimal()
    }
  })
  
  output$regressionSummary <- renderPrint({
    model <- model_reg()
    if (is.null(model)) {
      cat("Silakan pilih data yang tersedia atau unggah data custom Anda terlebih dahulu.")
      return()
    }
    summary(model)
  })
  
  output$regressionInterpretation <- renderPrint({
    model <- model_reg()
    data <- selected_data_reg()
    
    if (is.null(model) || is.null(data)) {
      cat("Silakan pilih data yang tersedia atau unggah data custom Anda terlebih dahulu.")
      return()
    }
    
    tryCatch({
      summary_model <- summary(model)
      coefs <- coefficients(model)
      residuals <- model$residuals
      
      # Determine variable names based on data type
      dep_var_name <- "variabel yang ingin diprediksi"
      if (input$data_choice_reg == "example") {
        y_var_name <- selected_y_var()
        dep_var_name <- switch(y_var_name,
                               "Beras_Y" = "harga Beras Umum",
                               "Beras_Y1" = "harga Beras Kualitas Bawah I",
                               "Beras_Y2" = "harga Beras Kualitas Bawah II", 
                               "Beras_Y3" = "harga Beras Kualitas Medium I",
                               "Beras_Y4" = "harga Beras Kualitas Medium II",
                               "Beras_Y5" = "harga Beras Kualitas Super I",
                               "Beras_Y6" = "harga Beras Kualitas Super II",
                               "harga beras")
      } else if (!is.null(custom_labels())) {
        dep_var_name <- custom_labels()[["Y"]]
      }
      
      cat("🔍 HASIL ANALISIS LENGKAP DATA ANDA\n")
      cat(paste(rep("=", 50), collapse = ""), "\n\n")
      
      cat("📋 INFORMASI DASAR:\n")
      cat(paste(rep("-", 20), collapse = ""), "\n")
      cat("• Jumlah data yang dianalisis:", nrow(data), "observasi\n")
      cat("• Jumlah faktor yang diuji:", length(coefs) - 1, "faktor\n")
      cat("• Variabel target:", dep_var_name, "\n")
      
      if (input$data_choice_reg == "example") {
        cat("• Faktor-faktor yang diuji:\n")
        cat("  1. Suhu Rata-rata (°C)\n")
        cat("  2. Curah Hujan (mm)\n")
      } else if (!is.null(custom_labels())) {
        labels <- custom_labels()
        x_vars <- names(labels)[names(labels) != "Y"]
        cat("• Faktor-faktor yang diuji:\n")
        for (i in 1:length(x_vars)) {
          cat("  ", i, ".", labels[[x_vars[i]]], "\n")
        }
      }
      cat("\n")
      
      r_squared <- summary_model$r.squared
      adj_r_squared <- summary_model$adj.r.squared
      
      cat("📊 SEBERAPA AKURAT MODEL INI?\n")
      cat(paste(rep("-", 30), collapse = ""), "\n")
      
      if (r_squared >= 0.9) {
        cat("🌟 LUAR BIASA! (R² =", round(r_squared, 3), ")\n")
        cat("   Model ini sangat akurat dan dapat menjelaskan", round(r_squared * 100, 1), "% dari perubahan", dep_var_name, ".\n")
        cat("   Ini berarti faktor-faktor yang Anda pilih hampir sempurna dalam memprediksi hasil!\n")
        cat("   💡 Tingkat akurasi ini sangat jarang ditemukan dalam data dunia nyata.\n\n")
      } else if (r_squared >= 0.8) {
        cat("🌟 SANGAT BAIK! (R² =", round(r_squared, 3), ")\n")
        cat("   Model ini dapat menjelaskan", round(r_squared * 100, 1), "% dari perubahan", dep_var_name, ".\n")
        cat("   Faktor-faktor yang Anda analisis sangat berpengaruh dan dapat diandalkan untuk prediksi!\n")
        cat("   💡 Hanya", round((1-r_squared) * 100, 1), "% yang dipengaruhi faktor lain di luar analisis ini.\n\n")
      } else if (r_squared >= 0.6) {
        cat("👍 CUKUP BAIK! (R² =", round(r_squared, 3), ")\n")
        cat("   Model ini dapat menjelaskan", round(r_squared * 100, 1), "% dari perubahan", dep_var_name, ".\n")
        cat("   Faktor-faktor yang Anda pilih memiliki pengaruh yang cukup signifikan.\n")
        cat("   💡 Masih ada", round((1-r_squared) * 100, 1), "% yang dipengaruhi faktor lain yang belum dimasukkan.\n")
        cat("   🔍 Pertimbangkan untuk menambah faktor lain jika memungkinkan.\n\n")
      } else if (r_squared >= 0.4) {
        cat("⚠  SEDANG (R² =", round(r_squared, 3), ")\n")
        cat("   Model ini hanya dapat menjelaskan", round(r_squared * 100, 1), "% dari perubahan", dep_var_name, ".\n")
        cat("   Ada", round((1-r_squared) * 100, 1), "% yang dipengaruhi faktor lain yang belum diidentifikasi.\n")
        cat("   💡 Saran: Cari faktor tambahan yang mungkin berpengaruh.\n")
        cat("   🔍 Atau periksa apakah ada pola non-linear dalam data.\n\n")
      } else {
        cat("❌ KURANG BAIK (R² =", round(r_squared, 3), ")\n")
        cat("   Model ini hanya menjelaskan", round(r_squared * 100, 1), "% dari perubahan", dep_var_name, ".\n")
        cat("   Sebagian besar (", round((1-r_squared) * 100, 1), "%) dipengaruhi faktor lain.\n")
        cat("   💡 Saran: Pertimbangkan faktor-faktor lain yang mungkin lebih berpengaruh.\n")
        cat("   🔍 Atau gunakan metode analisis yang berbeda.\n\n")
      }
      
      f_stat <- summary_model$fstatistic
      if (!is.null(f_stat)) {
        f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
        cat("🧪 UJI KELAYAKAN MODEL SECARA KESELURUHAN:\n")
        cat(paste(rep("-", 40), collapse = ""), "\n")
        if (f_p_value < 0.001) {
          cat("✅ SANGAT SIGNIFIKAN (p < 0.001)\n")
          cat("   Model ini secara statistik sangat layak dan dapat diandalkan!\n")
        } else if (f_p_value < 0.01) {
          cat("✅ SIGNIFIKAN (p < 0.01)\n")
          cat("   Model ini secara statistik layak dan dapat diandalkan.\n")
        } else if (f_p_value < 0.05) {
          cat("✅ CUKUP SIGNIFIKAN (p < 0.05)\n")
          cat("   Model ini secara statistik cukup layak.\n")
        } else {
          cat("❌ TIDAK SIGNIFIKAN (p ≥ 0.05)\n")
          cat("   Model ini secara statistik kurang layak untuk digunakan.\n")
        }
        cat("\n")
      }
      
      cat("💡 PENGARUH SETIAP FAKTOR SECARA DETAIL:\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
      
      if (input$data_choice_reg == "example") {
        # Handle example data with specific variable names
        if (length(coefs) >= 3) {
          cat("🌡 SUHU RATA-RATA:\n")
          p_value_temp <- summary_model$coefficients[2, 4]
          coef_temp <- coefs[2]
          std_error_temp <- summary_model$coefficients[2, 2]
          
          if (p_value_temp < 0.001) {
            significance_level <- "SANGAT SIGNIFIKAN"
            confidence <- "99.9%"
          } else if (p_value_temp < 0.01) {
            significance_level <- "SIGNIFIKAN"
            confidence <- "99%"
          } else if (p_value_temp < 0.05) {
            significance_level <- "CUKUP SIGNIFIKAN"
            confidence <- "95%"
          } else if (p_value_temp < 0.1) {
            significance_level <- "LEMAH"
            confidence <- "90%"
          } else {
            significance_level <- "TIDAK SIGNIFIKAN"
            confidence <- "kurang dari 90%"
          }
          
          if (p_value_temp < 0.05) {
            if (coef_temp > 0) {
              cat("   ✅", significance_level, "- BERPENGARUH POSITIF\n")
              cat("   📊 Setiap kenaikan 1°C suhu akan meningkatkan", dep_var_name, "sebesar Rp", format(round(abs(coef_temp), 0), big.mark = ","), "\n")
              cat("   🎯 Tingkat keyakinan:", confidence, "\n")
              
              if (abs(coef_temp) > 1000) {
                cat("   💭 Pengaruh ini tergolong BESAR - perubahan suhu berdampak signifikan pada harga\n")
              } else if (abs(coef_temp) > 100) {
                cat("   💭 Pengaruh ini tergolong SEDANG - ada dampak yang terukur\n")
              } else {
                cat("   💭 Pengaruh ini tergolong KECIL - dampak ada tapi tidak terlalu besar\n")
              }
            } else {
              cat("   ✅", significance_level, "- BERPENGARUH NEGATIF\n")
              cat("   📊 Setiap kenaikan 1°C suhu akan menurunkan", dep_var_name, "sebesar Rp", format(round(abs(coef_temp), 0), big.mark = ","), "\n")
              cat("   🎯 Tingkat keyakinan:", confidence, "\n")
              
              if (abs(coef_temp) > 1000) {
                cat("   💭 Pengaruh negatif ini tergolong BESAR - suhu tinggi menurunkan harga secara signifikan\n")
              } else if (abs(coef_temp) > 100) {
                cat("   💭 Pengaruh negatif ini tergolong SEDANG - ada dampak yang terukur\n")
              } else {
                cat("   💭 Pengaruh negatif ini tergolong KECIL - dampak ada tapi tidak terlalu besar\n")
              }
            }
          } else {
            cat("   ❌", significance_level, "\n")
            cat("   📊 Perubahan suhu tidak terbukti mempengaruhi", dep_var_name, "secara konsisten\n")
            cat("   🎯 Tingkat keyakinan hanya:", confidence, "\n")
            cat("   💭 Mungkin pengaruh suhu tidak konsisten atau terlalu kecil untuk dideteksi\n")
          }
          
          cat("   📏 Standar Error: Rp", format(round(std_error_temp, 0), big.mark = ","), "- menunjukkan tingkat ketidakpastian estimasi\n")
          cat("\n")
          
          cat("🌧 CURAH HUJAN:\n")
          p_value_rain <- summary_model$coefficients[3, 4]
          coef_rain <- coefs[3]
          std_error_rain <- summary_model$coefficients[3, 2]
          
          if (p_value_rain < 0.001) {
            significance_level <- "SANGAT SIGNIFIKAN"
            confidence <- "99.9%"
          } else if (p_value_rain < 0.01) {
            significance_level <- "SIGNIFIKAN"
            confidence <- "99%"
          } else if (p_value_rain < 0.05) {
            significance_level <- "CUKUP SIGNIFIKAN"
            confidence <- "95%"
          } else if (p_value_rain < 0.1) {
            significance_level <- "LEMAH"
            confidence <- "90%"
          } else {
            significance_level <- "TIDAK SIGNIFIKAN"
            confidence <- "kurang dari 90%"
          }
          
          if (p_value_rain < 0.05) {
            if (coef_rain > 0) {
              cat("   ✅", significance_level, "- BERPENGARUH POSITIF\n")
              cat("   📊 Setiap kenaikan 1mm curah hujan akan meningkatkan", dep_var_name, "sebesar Rp", format(round(abs(coef_rain), 0), big.mark = ","), "\n")
              cat("   🎯 Tingkat keyakinan:", confidence, "\n")
              
              if (abs(coef_rain) > 100) {
                cat("   💭 Pengaruh ini tergolong BESAR - curah hujan tinggi meningkatkan harga secara signifikan\n")
              } else if (abs(coef_rain) > 10) {
                cat("   💭 Pengaruh ini tergolong SEDANG - ada dampak yang terukur\n")
              } else {
                cat("   💭 Pengaruh ini tergolong KECIL - dampak ada tapi tidak terlalu besar\n")
              }
            } else {
              cat("   ✅", significance_level, "- BERPENGARUH NEGATIF\n")
              cat("   📊 Setiap kenaikan 1mm curah hujan akan menurunkan", dep_var_name, "sebesar Rp", format(round(abs(coef_rain), 0), big.mark = ","), "\n")
              cat("   🎯 Tingkat keyakinan:", confidence, "\n")
              
              if (abs(coef_rain) > 100) {
                cat("   💭 Pengaruh negatif ini tergolong BESAR - curah hujan tinggi menurunkan harga secara signifikan\n")
              } else if (abs(coef_rain) > 10) {
                cat("   💭 Pengaruh negatif ini tergolong SEDANG - ada dampak yang terukur\n")
              } else {
                cat("   💭 Pengaruh negatif ini tergolong KECIL - dampak ada tapi tidak terlalu besar\n")
              }
            }
          } else {
            cat("   ❌", significance_level, "\n")
            cat("   📊 Perubahan curah hujan tidak terbukti mempengaruhi", dep_var_name, "secara konsisten\n")
            cat("   🎯 Tingkat keyakinan hanya:", confidence, "\n")
            cat("   💭 Mungkin pengaruh curah hujan tidak konsisten atau terlalu kecil untuk dideteksi\n")
          }
          
          cat("   📏 Standar Error: Rp", format(round(std_error_rain, 2), big.mark = ","), "- menunjukkan tingkat ketidakpastian estimasi\n")
          cat("\n")
        }
      } else if (!is.null(custom_labels())) {
        # Handle custom data
        labels <- custom_labels()
        x_vars <- names(coefs)[-1]
        for (i in 1:length(x_vars)) {
          var_name <- x_vars[i]
          coef_value <- coefs[i+1]
          p_value <- summary_model$coefficients[i+1, 4]
          std_error <- summary_model$coefficients[i+1, 2]
          
          if (var_name %in% names(labels)) {
            var_label <- labels[[var_name]]
          } else {
            var_label <- var_name
          }
          
          cat("📈", toupper(var_label), ":\n")
          
          if (p_value < 0.001) {
            significance_level <- "SANGAT SIGNIFIKAN"
            confidence <- "99.9%"
          } else if (p_value < 0.01) {
            significance_level <- "SIGNIFIKAN"
            confidence <- "99%"
          } else if (p_value < 0.05) {
            significance_level <- "CUKUP SIGNIFIKAN"
            confidence <- "95%"
          } else if (p_value < 0.1) {
            significance_level <- "LEMAH"
            confidence <- "90%"
          } else {
            significance_level <- "TIDAK SIGNIFIKAN"
            confidence <- "kurang dari 90%"
          }
          
          if (p_value < 0.05) {
            if (coef_value > 0) {
              cat("   ✅", significance_level, "- BERPENGARUH POSITIF\n")
              cat("   📊 Setiap kenaikan 1 unit pada", var_label, "akan meningkatkan", labels[["Y"]], "sebesar", round(abs(coef_value), 4), "\n")
              cat("   🎯 Tingkat keyakinan:", confidence, "\n")
              
              if (coef_value > 1) {
                cat("   💭 Pengaruh ini tergolong BESAR - perubahan kecil pada", var_label, "berdampak besar\n")
              } else if (coef_value > 0.1) {
                cat("   💭 Pengaruh ini tergolong SEDANG - ada dampak yang terukur\n")
              } else {
                cat("   💭 Pengaruh ini tergolong KECIL - dampak ada tapi tidak terlalu besar\n")
              }
            } else {
              cat("   ✅", significance_level, "- BERPENGARUH NEGATIF\n")
              cat("   📊 Setiap kenaikan 1 unit pada", var_label, "akan menurunkan", labels[["Y"]], "sebesar", round(abs(coef_value), 4), "\n")
              cat("   🎯 Tingkat keyakinan:", confidence, "\n")
              
              if (abs(coef_value) > 1) {
                cat("   💭 Pengaruh negatif ini tergolong BESAR - perubahan kecil berdampak besar\n")
              } else if (abs(coef_value) > 0.1) {
                cat("   💭 Pengaruh negatif ini tergolong SEDANG - ada dampak yang terukur\n")
              } else {
                cat("   💭 Pengaruh negatif ini tergolong KECIL - dampak ada tapi tidak terlalu besar\n")
              }
            }
          } else {
            cat("   ❌", significance_level, "\n")
            cat("   📊 Perubahan pada", var_label, "tidak terbukti mempengaruhi", labels[["Y"]], "secara konsisten\n")
            cat("   🎯 Tingkat keyakinan hanya:", confidence, "\n")
            cat("   💭 Mungkin pengaruhnya tidak konsisten atau terlalu kecil untuk dideteksi\n")
          }
          
          cat("   📏 Standar Error:", round(std_error, 4), "- menunjukkan tingkat ketidakpastian estimasi\n")
          cat("\n")
        }
      }
      
      cat("🔬 VALIDITAS DAN KUALITAS MODEL:\n")
      cat(paste(rep("-", 35), collapse = ""), "\n")
      
      if (length(residuals) >= 3 && length(residuals) <= 5000) {
        shapiro_test <- shapiro.test(residuals)
        if (shapiro_test$p.value > 0.05) {
          cat("✅ Distribusi kesalahan: NORMAL (Sangat Baik!)\n")
          cat("   💡 Kesalahan prediksi terdistribusi normal, model dapat diandalkan\n")
        } else {
          cat("⚠  Distribusi kesalahan: TIDAK NORMAL (Perlu Perhatian)\n")
          cat("   💡 Ada pola tertentu dalam kesalahan, mungkin perlu transformasi data\n")
        }
      } else {
        cat("❓ Uji normalitas tidak dapat dilakukan (ukuran sampel tidak sesuai)\n")
      }
      
      tryCatch({
        bp_test <- bptest(model)
        if (bp_test$p.value > 0.05) {
          cat("✅ Konsistensi kesalahan: KONSISTEN (Sangat Baik!)\n")
          cat("   💡 Tingkat kesalahan prediksi konsisten di semua level, model stabil\n")
        } else {
          cat("⚠  Konsistensi kesalahan: TIDAK KONSISTEN (Perlu Perhatian)\n")
          cat("   💡 Kesalahan prediksi bervariasi, mungkin perlu penyesuaian model\n")
        }
      }, error = function(e) {
        cat("❓ Uji konsistensi kesalahan tidak dapat dilakukan\n")
      })
      
      if (length(coefs) > 2) {
        tryCatch({
          vif_result <- vif(model)
          if (all(vif_result < 5)) {
            cat("✅ Keterkaitan antar faktor: INDEPENDEN (Sangat Baik!)\n")
            cat("   💡 Setiap faktor memberikan informasi unik, tidak saling tumpang tindih\n")
          } else if (all(vif_result < 10)) {
            cat("⚠  Keterkaitan antar faktor: SEDIKIT TERKAIT (Masih Dapat Diterima)\n")
            cat("   💡 Ada sedikit tumpang tindih informasi antar faktor\n")
          } else {
            cat("❌ Keterkaitan antar faktor: SANGAT TERKAIT (Bermasalah)\n")
            cat("   💡 Beberapa faktor mengukur hal yang sama, pertimbangkan menghapus salah satu\n")
            cat("   🔍 Faktor dengan VIF tinggi:\n")
            high_vif <- vif_result[vif_result >= 10]
            for (i in 1:length(high_vif)) {
              cat("      -", names(high_vif)[i], ": VIF =", round(high_vif[i], 2), "\n")
            }
          }
        }, error = function(e) {
          cat("❓ Uji keterkaitan antar faktor tidak dapat dilakukan\n")
        })
      } else {
        cat("✅ Keterkaitan antar faktor: TIDAK RELEVAN (Hanya 1 faktor)\n")
      }
      
      cat("\n")
      cat("🎯 REKOMENDASI DAN KESIMPULAN AKHIR:\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
      
      significant_vars <- 0
      if (input$data_choice_reg == "example") {
        if (length(coefs) >= 2 && summary_model$coefficients[2, 4] < 0.05) significant_vars <- significant_vars + 1
        if (length(coefs) >= 3 && summary_model$coefficients[3, 4] < 0.05) significant_vars <- significant_vars + 1
      } else if (!is.null(custom_labels())) {
        labels <- custom_labels()
        x_vars <- names(coefs)[-1]
        for (i in 1:length(x_vars)) {
          p_value <- summary_model$coefficients[i+1, 4]
          if (p_value < 0.05) significant_vars <- significant_vars + 1
        }
      }
      
      if (significant_vars > 0 && r_squared >= 0.7) {
        cat("🎉 KESIMPULAN: Model Anda SANGAT BAIK dan dapat diandalkan!\n")
        cat("   ✅", significant_vars, "faktor terbukti berpengaruh signifikan\n")
        cat("   ✅ Tingkat akurasi prediksi sangat tinggi (", round(r_squared * 100, 1), "%)\n")
        cat("   💡 REKOMENDASI: Gunakan model ini untuk prediksi dan pengambilan keputusan\n")
      } else if (significant_vars > 0 && r_squared >= 0.5) {
        cat("👍 KESIMPULAN: Model Anda CUKUP BAIK dan dapat digunakan dengan hati-hati\n")
        cat("   ✅", significant_vars, "faktor terbukti berpengaruh signifikan\n")
        cat("   ⚠  Tingkat akurasi prediksi sedang (", round(r_squared * 100, 1), "%)\n")
        cat("   💡 REKOMENDASI: Pertimbangkan menambah faktor lain untuk meningkatkan akurasi\n")
      } else if (significant_vars > 0) {
        cat("⚠  KESIMPULAN: Model Anda menunjukkan ada pengaruh, tapi akurasi rendah\n")
        cat("   ✅", significant_vars, "faktor terbukti berpengaruh signifikan\n")
        cat("   ❌ Tingkat akurasi prediksi rendah (", round(r_squared * 100, 1), "%)\n")
        cat("   💡 REKOMENDASI: Cari faktor tambahan yang lebih berpengaruh\n")
      } else {
        cat("🤔 KESIMPULAN: Model perlu diperbaiki\n")
        cat("   ❌ Tidak ada faktor yang terbukti berpengaruh signifikan\n")
        cat("   ❌ Tingkat akurasi prediksi rendah (", round(r_squared * 100, 1), "%)\n")
        cat("   💡 REKOMENDASI: \n")
        cat("      - Periksa kembali data Anda\n")
        cat("      - Coba faktor-faktor yang berbeda\n")
        cat("      - Pertimbangkan metode analisis lain\n")
      }
      
      cat("\n📋 SARAN PRAKTIS UNTUK LANGKAH SELANJUTNYA:\n")
      if (r_squared < 0.5) {
        cat("   1. 🔍 Eksplorasi faktor-faktor lain yang mungkin berpengaruh\n")
        cat("   2. 📊 Periksa apakah ada pola waktu atau musiman dalam data\n")
        cat("   3. 🧮 Pertimbangkan transformasi data (log, akar kuadrat, dll.)\n")
      }
      if (significant_vars < length(coefs) - 1) {
        cat("   4. ✂  Pertimbangkan menghapus faktor yang tidak signifikan\n")
      }
      cat("   5. 📈 Kumpulkan lebih banyak data jika memungkinkan\n")
      cat("   6. 🔄 Validasi model dengan data baru untuk memastikan konsistensi\n")
      
      if (input$data_choice_reg == "example") {
        cat("\n🌾 INTERPRETASI KHUSUS UNTUK ANALISIS BERAS:\n")
        y_var_name <- selected_y_var()
        y_display_name <- switch(y_var_name,
                                 "Beras_Y" = "Beras Umum",
                                 "Beras_Y1" = "Beras Kualitas Bawah I",
                                 "Beras_Y2" = "Beras Kualitas Bawah II", 
                                 "Beras_Y3" = "Beras Kualitas Medium I",
                                 "Beras_Y4" = "Beras Kualitas Medium II",
                                 "Beras_Y5" = "Beras Kualitas Super I",
                                 "Beras_Y6" = "Beras Kualitas Super II",
                                 "Beras")
        
        cat("   📊 Analisis ini menunjukkan bagaimana cuaca mempengaruhi harga", y_display_name, "\n")
        cat("   🌡 Suhu yang lebih tinggi/rendah dapat mempengaruhi produktivitas padi\n")
        cat("   🌧 Curah hujan yang optimal diperlukan untuk pertumbuhan padi yang baik\n")
        cat("   💰 Fluktuasi harga beras sangat dipengaruhi oleh kondisi cuaca regional\n")
        cat("   🎯 Model ini dapat membantu prediksi harga berdasarkan prakiraan cuaca\n")
      }
      
    }, error = function(e) {
      cat("❌ Terjadi kesalahan dalam analisis:\n")
      cat("Detail error:", e$message, "\n")
      cat("Silakan periksa data Anda dan coba lagi.")
    })
  })
}

# Menjalankan Aplikasi Shiny
shinyApp(ui = ui, server = server)
