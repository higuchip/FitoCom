# ui.R
library(shiny)
library(shinythemes)
library(markdown)

# Leitura do README.md
readme_text <- includeMarkdown("README.md")

# Interface do usuário
fluidPage(
  navbarPage(
    title = h4("FitoCom 1.6"),
    theme = shinytheme("flatly"),
    tabPanel(
      h4("Home"),
      fluidRow(
        column(12, readme_text)
      )
    ),
    tabPanel(
      h4("Descritores Fitossociológicos"),
      sidebarLayout(
        sidebarPanel(
          helpText(h3("Entrada dos dados")),
          fileInput("arquivo", "Escolha arquivo CSV", 
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          numericInput("area", "Área da parcela (m²)", 200)
        ),
        mainPanel(
          tabsetPanel(
            id = "tabselected",
            tabPanel("Resumo", value = 1, 
                     textOutput("resumo1"),
                     textOutput("resumo2"),
                     textOutput("resumo3"),
                     textOutput("resumo4"),
                     textOutput("resumo5"),
                     textOutput("resumo6")),
            tabPanel("Tabela Fitossociológica", value = 2, 
                     downloadButton('downloadTab1', 'Exportar como CSV'),
                     br(),
                     "Estimativas de Densidade Absoluta (DA ind/ha), Relativa (DR %), Dominância Absoluta (DoA m²/ha), Relativa (DoR %), Frequência Absoluta (FA %), Relativa (FR %) e Valor de Importância (VI %)",
                     tableOutput("fito"))
          )
        )
      )
    ),
    tabPanel(
      h4("Esforço Amostral"),
      sidebarLayout(
        sidebarPanel(
          helpText(h3("Parâmetros para Esforço Amostral")),
          selectInput("samp.sis", "Sistema de Amostragem:",
                      choices = c("Aleatória Simples" = "AS", "Estratificada" = "EST", "Sistemática" = "SIS")),
          numericInput("area_floresta", "Área Total da Floresta (ha)", 150),
          numericInput("alfa", "Nível de significância", 0.05),
          numericInput("LE", "Limite de Erro Admissível (%)", 10),
          textInput('area_estratos', 'Áreas dos estratos (ha, separadas por vírgulas)')
        ),
        mainPanel(
          tags$style(type="text/css", "#esforco { font-size: 20px; }"),
          verbatimTextOutput("esforco")
        )
      )
    ),
    tabPanel(
      h4("Curva de Acumulação"),
      sidebarLayout(
        sidebarPanel(
          helpText(h3("Diversidade")),
          selectInput("curvaInput", "Tipo de Curva",
                      choices = c("rarefação/extrapolação" = 1, 
                                  "curva de completude de amostragem" = 2,
                                  "curva baseada em cobertura" = 3))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Interpolação/Extrapolação", 
                     downloadButton('downloadTab_diversidade', 'Exportar como CSV'),
                     br(),
                     "Estimativas de riqueza (t - intensidade amostral; ordem - valor de q utilizado (0); qD - Riqueza; qD.LCL, qD.UCL - intervalos de confiança de 95% para qD; SC - cobertura da amostragem; SC.LCL, SC.UCL - intervalos de confiança de 95% para SC).",
                     tableOutput("inter_extra")),
            tabPanel("Curva", 
                     downloadButton('downloadPlot', 'Exportar como JPG'), 
                     plotOutput("myplot"))
          )
        )
      )
    ),
    tabPanel(
      h4("Espécies Indicadoras"),
      sidebarLayout(
        sidebarPanel(
          helpText(h3("Espécies Indicadoras")),
          helpText("1) Inserir arquivo no menu 'Descritores Fitossociológicos'."),
          helpText("2) Definir colunas que representam as parcelas e os setores para os quais serão definidas as espécies indicadoras."),
          selectInput("columns1", "Selecione as Parcelas", choices = NULL),
          selectInput("columns", "Selecione os Setores", choices = NULL)
        ),
        mainPanel(
          downloadButton('download_ind', 'Exportar como CSV'),
          br(),
          "Espécies indicadoras, conforme setores de análise, com respectivos valores indicadores e significâncias",
          tableOutput("indicadoras")
        )
      )
    ),
    navbarMenu(
      h4("Sobre"),
      tabPanel(
        h4("Instruções de Uso"),
        fluidRow(
          column(8, includeMarkdown("instrucoes.md"))
        )
      ),
      tabPanel(
        h4("Referências"),
        fluidRow(
          column(8, includeMarkdown("referencias.md"))
        )
      ),
      tabPanel(
        h4("Contato"),
        fluidRow(
          column(6, includeMarkdown("contato.md")),
          column(4,
                 img(class="img-polaroid", 
                     src=paste0("http://labdendro.com/blog/wp-content/",
                                "/uploads/2018/09/",
                                "labdendro.png")))
        )
      )
    )
  )
)
