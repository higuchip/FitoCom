# Aplicativo Web para Análises Fitossociológicas

#### Desenvolvido integralmente por meio da linguagem de programação estatística R, junto com o pacote Shiny.

Autor: Pedro Higuchi - higuchip at gmail.com 

**Versão: 1.5**

Updates:

* Correção de bugs
* Adicionada Análise do esforço amostral com base na abundância e área basal
* Adicionada a determinação do número de Hill, na parte "Resumo"


### Instruções de uso

#### Arquivo de entrada
* Arquivo de entrada deve estar formatado conforme [arquivo exemplo](https://raw.githubusercontent.com/higuchip/FitoCom/master/dados_exemplo.csv)
, que deve ter necessariamente as colunas **parc** (parcelas), **spp** (espécies), **dap** ou **cap** (circunferência ou diâmetro na altura do peito em cm).
* Apenas no caso de amostragem estratificada, é necessário que o arquivo tenha coluna "estratos".

#### Descritores fitossociológicos:

* Indicar tamanho de parcelas em m2
* Análise do Esforço amostral: No caso de Amostragem Estratificada é necessária a inclusão de coluna "estrato" no arquivo entrada.


#### Espécies indicadoras:

* Necessário carregar o arquivo na aba "Descritores fitossociológicos", em seguida selecionar as colunas de parcelas e setores, para os quais serão definidas as espécies indicadoras.

### Pacotes e funcões utilizados:

*  Chang, W.; Cheng, J.; Allaire, JJ; Xie, Y.;  McPherson, J.  2017. shiny: Web Application Framework for R. R package version
  1.0.4. https://CRAN.R-project.org/package=shiny

* Dalagnol, Ricardo; Christo, Alexandre Gabriel; Higuchi, Pedro; Rodrigues, Arthur Vinicius. 
Função para cálculo dos descritores fitossociológicos e similaridade entre sítios. Disponível em: https://github.com/ricds/fitoR.

* Higuchi, P. sampling.analysis: Função em linguagem de programação estatística R para análise do processo amostragem de levantamentos fitossociógicos em função do número de indivíduos e da área basal. 2019. #Disponvel em https://github.com/higuchip/sampling.analysis

* Hsieh, T.C.;  Ma, K. H.; Chao. A. 2018 iNEXT: iNterpolation and EXTrapolation for
species diversity. R package version 2.0.17 URL:
  http://chao.stat.nthu.edu.tw/blog/software-download/.

*  Roberts, D. W. (2016). labdsv: Ordination and Multivariate Analysis
  for Ecology. R package version 1.8-0.
  https://CRAN.R-project.org/package=labdsv

                

### Referências:

* Chao, A. et al. 2014. Rarefaction and extrapolation with Hill numbers: a framework for sampling and estimation in species diversity studies. Ecological Monographs 84:45-67.
* Dufrene, M.; Legendre, P. 1997. Species assemblages and indicator species: the need for a flexible asymmetrical approach. Ecol. Monogr. 67(3):345-366.
* Felfili, J.M. et al. Procedimentos e métodos de amostragem de vegetação. Fitossociologia no Brasil: métodos e estudos de casos. Viçosa, MG: Universidade Federal de Viçosa, p. 86-121, 2011.
* Scolforo, J.R.S.; Mello, J.M. Inventário Florestal. Lavras/: UFLA/FAEPE, 561p., 2006