library(markdown)
library(vegan)
library(labdsv)
library(iNEXT)
library(shinythemes)
library(ggplot2)

navbarPage(h4("FitoCom 1.5"),
           theme = shinytheme("flatly"),
           navbarMenu(h4("Menu"),
                      fluidRow(
                        column(6,
                               includeMarkdown("README.md")
                        ),
                        column(4,
                               img(class="img-polaroid",
                                   src=paste0("http://labdendro.com/blog/wp-content/",
                                              "/uploads/2018/09/",
                                              "labdendro.png")))
                      ),
                      tabPanel(h4("Descritores Fitossociológicos"),
                               
                               sidebarLayout(
                                 sidebarPanel(helpText(h3("Descritores Fitossociológicos")),
                                   fileInput("arquivo", "Escolha arquivo CSV",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                                   numericInput("area", "Área da parcela (m2)", 200),
                                   
                                   selectInput("curvaInput", "Sistema de Amostragem",
                                               choices = c("Aleatória Simples" =1, 
                                                           "Sistemática" =2,
                                                           "Estratificada"=3)),
                                   numericInput("area_fragmento", "Área da floresta (ha)", 200)
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Resumo",
                                              textOutput("resumo1"),
                                              textOutput("resumo2"),
                                              textOutput("resumo3"),
                                              textOutput("resumo4"),
                                              textOutput("resumo5")),
                                     tabPanel("Tabela Fitossociológica", 
                                              downloadButton('downloadTab1', 'Exportar como csv'),
                                              br(),
                                              ("Estimativas de Densidade Absoluta (DA ind/ha) e Relativa (DR %), Dominância Absoluta (DoA m2/ha) e Relativa (DoR %) e
                                               Frequência Absoluta (FA %) e Relativa (FR %) e Valor de Importância (VI %)"),
                                              tableOutput("fito") )
                                     ))
                                 )
                               
                                 ),
                      
                      tabPanel(h4("Diversidade"),
                               sidebarLayout(
                                 sidebarPanel(helpText(h3("Diversidade")),
                                   selectInput("curvaInput", "Tipo de Curva",
                                               choices = c("rarefaction/extrapolation" =1, 
                                                           "sample completeness curve" =2,
                                                           "coverage-based rarefaction/extrapolation curve"=3))
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Interporlação/Extrapolação", 
                                              downloadButton('downloadTab_diversidade', 'Exportar como csv'),
                                              br(),
                                              ("Estimativas de riqueza (t - intensidade amostral; order - valor de q utilizado (0); qD - Riqueza
                                               ; qD.LCL, qD.UCL - intervalos de 
                                               confiança de 95% para qD; SC - cobertura da amostragem;
                                               SC.LCL, SC.UCL - intervalos de confiança de 95% para SC )."),
                                              tableOutput("inter_extra") ),
                                     tabPanel("Curva", downloadButton('downloadPlot', 'Exportar como jpg'), plotOutput("myplot"))
                                   ))
                                 )
                               
                               ),
                      tabPanel(h4("Espécies Indicadoras"),
                               sidebarLayout(
                                 sidebarPanel(helpText(h3("Espécies Indicadoras")),
                                              helpText("1) inserir", tags$b("Arquivo"),
                                              "no menu", tags$b("Descritores Fitossociológicos")),
                                              helpText("2) Definir colunas que representam as parcelas
                                              e os setores para quais serão definidas as espécies
                                              indicadoras"),
                                    # textInput("text", "Nome do setor:")
                                   selectInput("columns1", "Selecione as Parcelas", choices = NULL),
                                   selectInput("columns", "Selecione os Setores", choices = NULL)

                                 ),
                                 mainPanel(downloadButton('download_ind', 'Exportar como csv'),
                                           br(),
                                           ("Espécies indicadoras, conforme setores de análise, com respectivos
                                            valores indicadores e significâncias"),
                                           tableOutput("indicadoras")
                                 )
                               )
                               )
                      
           ),
           
           navbarMenu(h4("Sobre"),
                  
                      tabPanel(h4("Instruções de uso"),

                                        fluidRow(
                                          column(8,
                                                 includeMarkdown("instrucoes.md")
                                          )
                                        
                                        )
                               ),
                      
                      tabPanel(h4("Referências"),
                               
                               fluidRow(
                                 column(8,
                                        includeMarkdown("referencias.md")
                                 )
                                 
                               )
                      ),
                      tabPanel(h4("Contato"),
                               
                               fluidRow(
                                 column(6,
                                        includeMarkdown("contato.md")
                                 ),
                                 column(4,
                                        img(class="img-polaroid",
                                            src=paste0("http://labdendro.com/blog/wp-content/",
                                                       "/uploads/2018/09/",
                                                       "labdendro.png")))
                               )
                      )
                      
                      )
           
        
)