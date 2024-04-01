# by Christiano dos Santos, PhD
library(shiny)
library(DT)
library(ggplot2) 

options(shiny.maxRequestSize=30*1024^2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Get ORCA v4 or v5 IR data from output file"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "file",
        label = h4("Choose a file to upload...")
      ),
      selectInput(
        inputId = "methodSelected",
        label = "Select convolution method",
        choices = c("lorentzian", "gaussian")
      ),
      
      sliderInput(inputId = "omega", label = "FWHM", min = 4, max = 50, value = 10, step = 2),
      p("Author: Christiano dos Santos, PhD.", 
        a("Linkedin", href = "https://www.linkedin.com/in/christianosantos/"),
      ),
      p(a("ORCID", href = "https://orcid.org/0000-0001-6415-2714/"),),
      
      img(
        src = "rosto.png", alt="foto de rosto",
        width = "70px", height = "70px"
      ),
      p("Infrared (IR) spectroscopy is essential for understanding molecular structure.
      ORCA, a computational tool, accurately predicts IR spectra.", a("ORCA", href="https://orcaforum.kofo.mpg.de/index.php"),
        "
      However, spectrum analysis remains laborious.This tool automates IR spectrum extraction from ORCA output files. 
      offering a user-friendly interface for rapid spectrum interpretation.

    Features include ORCA output file parsing, automatic IR data extraction, and flexible analysis options,
    including selection of convolution methods such as Lorentzian or Gaussian..
    Users can download spectra for offline analysis.

Keywords: Infrared Spectroscopy, ORCA, Spectrum Analysis, Automation, Computational Chemistry, Data Visualization
")
    ),
  # Main panel
  mainPanel(
    plotOutput("plot"),  # Render the plot output
    DTOutput("fileContents"),  # Render the data table output
    
    downloadButton("downloadData", "Download Data")  # Botão de download
  )
  )
)

# Define server logic required to draw a histogram
# Função para calcular a convolução de Lorentz
convLorentz <- function(wave_j, frequency, T2_list, scale, omega) {
  intensity <- sum(scale * T2_list * omega / (4 * (wave_j - frequency)^2 + omega^2))
  return(intensity)
}

# Função para calcular a convolução gaussiana
convGauss <- function(wave_j, frequency, T2_list, scale, sigma) {
  intensity <- sum((1/sqrt(2)) * T2_list * exp(-((wave_j - frequency) / (2*sigma))^2))
  return(intensity)
}

identifyOrcaVersion <- function(file_content) {
  #print(file_content)
  # Verifica se a linha contém a versão 4
  if (any(grepl("Program Version 4", file_content))) {
    showNotification("Orca Version 4", type = "warning")
    return("Version 4")
  }
  # Verifica se a linha contém a versão 5
  else if (any(grepl("Program Version 5", file_content))) {
    showNotification("Orca Version 5", type = "warning")
    return("Version 5")
  }
  else {
    return("Unknown Version")
  }
}


parseOrcaFile <- function(orca_file){
  orca_data_first_50_lines <- head(orca_file, 50)
  
  orca_Version <- identifyOrcaVersion(orca_data_first_50_lines)
  
  # Processamento para a versão 4
  if (orca_Version == "Version 4") {
    # Encontre o índice da linha contendo "IR SPECTRUM"
    start_idx <- which(grepl("IR SPECTRUM", orca_file))
    start_idx <- start_idx[1]  # Pega apenas o primeiro índice
    
    # A partir da linha "IR SPECTRUM", conte 5 linhas para começar a extração
    start_idx <- start_idx + 5
    
    # Encontre o índice da linha contendo "The first frequency considered to be a vibration is"
    end_idx <- which(grepl("The first frequency considered to be a vibration is", orca_file))
    end_idx <- end_idx[length(end_idx)]  # Pegue o último índice
    
    # A última linha com valores está 2 linhas antes dessa linha final
    end_idx <- end_idx - 2
    
    # Extraia as linhas relevantes
    vib_lines <- orca_file[start_idx:end_idx]
    
    # Crie um data frame vazio para armazenar freq e T2
    freq_t_df <- data.frame(freq = numeric(0), T2 = numeric(0))
    
    # Iterar sobre cada linha em vib_lines
    for (line in vib_lines) {
      # Trim espaços em branco à esquerda e à direita
      trimmed_line <- trimws(line, which = "both")
      
      # Divida a linha pelo número consecutivo de espaços usando a expressão regular '\\s+'
      line_values <- unlist(strsplit(trimmed_line, "\\s+"))
      
      # Extraia o segundo e quinto valores da linha dividida (freq e T2)
      freq <- as.numeric(line_values[2])
      T2 <- as.numeric(line_values[3])  # Aqui você pode ajustar o índice conforme necessário
      
      # Adicione os valores de freq e T2 ao data frame
      freq_t_df <- rbind(freq_t_df, data.frame(freq = freq, T2 = T2))
    }
    freq_t_data <- freq_t_df
    return(freq_t_df)
  
  }
  else if (orca_Version == "Version 5") {
    # Encontre os índices das linhas contendo "IR SPECTRUM" e "-----"
    start_idx <- which(grepl("IR SPECTRUM", orca_file))
    end_idx <- which(grepl("Dirac delta", orca_file))
    
    # Extraia as linhas relevantes - remova 6 linhas de cabeçalho e 2 de cauda
    vib_lines <- orca_file[(start_idx + 6):(end_idx - 2)]
    
    # Crie um data frame vazio para armazenar freq e T2
    freq_t_df <- data.frame(freq = numeric(0), T2 = numeric(0))
    
    # Iterar sobre cada linha em vib_lines
    for (line in vib_lines) {
      # Trim espaços em branco à esquerda e à direita
      trimmed_line <- trimws(line, which = "both")
      
      # Divida a linha pelo número consecutivo de espaços usando a expressão regular '\\s+'
      line_values <- unlist(strsplit(trimmed_line, "\\s+"))
      
      # Extraia o segundo e quinto valores da linha dividida
      freq <- as.numeric(line_values[2])
      T2 <- as.numeric(line_values[5])
      
      # Adicione os valores de freq e T ao data frame
      freq_t_df <- rbind(freq_t_df, data.frame(freq = freq, T2 = T2))
    }
    freq_t_data <- freq_t_df
    return(freq_t_df)
  }
  
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Definir um reativo global para freq_t_df
  freq_t_df <- reactive({
    req(input$file)  # Garante que um arquivo foi carregado
    
    # Verifica se o arquivo existe
    if (!file.exists(input$file$datapath)) {
      showNotification("Erro: arquivo não encontrado", type = "error")
      return(NULL)
    }
    
    # Tenta ler o conteúdo do arquivo
    orca_data <- tryCatch({
      readLines(input$file$datapath)
    }, error = function(e) {
      showNotification("Erro ao ler orca_data",)
      return(NULL)
    })
    
    # Verifica se a leitura foi bem-sucedida
    if (is.null(orca_data)) {
      showNotification("Erro: falha ao ler o conteúdo do arquivo", type = "error")
      return(NULL)
    }
    else {
      freq_t_df <- parseOrcaFile(orca_data)
      return(freq_t_df)
    }
    
    
  })
  
  
  output$fileContents <- renderDT({
    
    # Verifica se um arquivo foi carregado
    req(input$file)
   
    # Verifica se o arquivo existe
    if (!file.exists(input$file$datapath)) {
      return(NULL)
    }
    # Tenta ler o conteúdo do arquivo
    orca_data <- tryCatch({
      readLines(input$file$datapath)
    }, error = function(e) {
      showNotification("Erro ao ler orca_data",)
      return(NULL)
    })
    
    # Verifica se a leitura foi bem-sucedida
    if (is.null(orca_data)) {
      showNotification("Erro: falha ao ler o conteúdo do arquivo", type = "error")
      return(NULL)
    }
    
    # Retorna o data frame freq_t_df
    freq_t_df()
    })
  
  
  # Renderiza o gráfico
  output$plot <- renderPlot({
    req(freq_t_df())  # Garante que freq_t_df não seja NULL
    
    # Crie uma sequência de valores de onda de 400 a 4000 cm^-1
    wave <- seq(400, 4000, by = 1)
    scale <- 1
    omega <- input$omega
    metodo <- input$methodSelected
    
    # Calcular a convolução com base no método selecionado
    if (metodo == "lorentzian") {
      calc_Intensity <- sapply(wave, function(wave_j) convLorentz(wave_j, freq_t_df()$freq, freq_t_df()$T2, scale, omega))
    } 
    else if (metodo == "gaussian") {
      sigma <- input$omega  # Você precisará definir a entrada sigma no seu UI também
      calc_Intensity <- sapply(wave, function(wave_j) convGauss(wave_j, freq_t_df()$freq, freq_t_df()$T2, scale, sigma))
    }
   
    calc_Intensity <- calc_Intensity/max(calc_Intensity)
  
    
    # Calcular a transmitância
    transmittance <- 10^(-calc_Intensity)
    
    # Criar um DataFrame com os valores de onda e transmitância
    df_plot <- data.frame(wave = wave, transmittance = transmittance)
    
    # Plotar os valores de onda versus transmitância
    ggplot(df_plot, aes(x = wave, y = transmittance)) +
      geom_line() +
      labs(x = "Wavenumber/cm", y = "Transmittance - %") +
      ggtitle(paste("Transmittance x Wavenumber -", input$file$name))
  })
  
  # Função para criar o arquivo CSV e vinculá-lo ao botão de download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("transmittance_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Escrever os dados em um arquivo CSV
      write.csv(df_plot, file, row.names = FALSE)
    }
  )
}
# Run the application 
shinyApp(ui = ui, server = server)
