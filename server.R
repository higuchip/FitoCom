

function(input, output, session) {
  

  ##RESUMO
  

  output$resumo1 <- renderText(
    {
      inFile <- input$arquivo
      
      if (is.null(inFile))
        return(NULL)
      
      veg<-read.table(inFile$datapath, header = T,
                      sep = ";", dec=",")
      matriz<-as.data.frame.matrix(table(veg$spp, veg$parc))
      area<-as.numeric(input$area)
      
      
      #numero de parcelas
      nparc<-length(levels(as.factor(veg$parc)))
      
      #area total amostrada
      area.parc=(area*nparc)
      
      #densidade
      dta=length(veg$spp)/(area.parc/10000)
      
      #desvio da densidade entre parcelas
      dtadesv = 0
      dtai = 1
      vetor=1
      while(dtai <= nparc)
      {
        length(vetor) <- nparc
        vetor[dtai] = sum(matriz[,dtai])
        dtadesv = sd(vetor)/(area/10000)
        dtai = dtai + 1
      }
      
      #calcula o numero de ind amostrados
      N<-apply(matriz,1,sum)
      
      #calcula densidades
      DA<-apply(matriz,1,sum)/(area.parc/10000)
      DR<-DA/sum(DA)*100
      
      #calcula frequencias
      freq<-(if (length(dim(matriz)) > 1)
      {apply(matriz > 0,1,sum)} else sum(matriz > 0))
      FA<-(freq/nparc)*100
      FR<-(FA/sum(FA))*100
      
      #checa por NAs nos dados e transforma em zeros
      veg[is.na(veg)] <- 0
      
      #determina se existe "caps" ou "daps" e quais colunas estao
      cols = grep('cap', colnames(veg))
      ncols = length(cols)
      if (ncols>0) param="cap"
      
      cols2 = grep('dap', colnames(veg))
      ncols2 = length(cols2)
      if (ncols2>0) param="dap"
      
      if (param=="dap") cols=cols2
      if (param=="dap") ncols=ncols2
      
      i=1
      veg$areasec=0
      while (i<=ncols)
      {
        if (param=="cap") veg$areasec<-veg$areasec+((pi*(veg[,cols[i]]/pi)^2)/40000)
        if (param=="dap") veg$areasec<-veg$areasec+((pi*veg[,cols[i]]^2)/40000)
        i=i+1
      }
      
      #calcula as dominancias
      DoA<-tapply(veg$areasec, veg$spp, sum)/(area.parc/10000)
      DoR<-DoA/sum(DoA) *100
      
      # area basal por espécie
      AB<-tapply(veg$areasec, veg$spp, sum)
      
      #area basal
      abta=sum(DoA)
      
      #desvio da area basal entre parcelas
      somag<-tapply(veg$areasec, veg$parc, sum)/(area/10000)
      abdesv = sd(somag)
      
      #calcula o indice de valor de importancia
      VI<-(DR+DoR+FR)/3
      fito=data.frame(N=N,AB=AB,DA=DA,DR=DR,DoA=DoA,DoR=DoR,FA=FA,FR=FR,VI=VI)
      
      #calcula os indices de diversidade
      Pi<-N/sum(N)
      Pi<-Pi*log(Pi)
      SW=-sum(Pi)
      S=nrow(fito)
      J=SW/log(S)
      
      
      paste ("Densidade total por hectare = ", round(dta,digits=2), "\u00B1",round(dtadesv,digits=2),"ind/ha")
      
      
      
      
      # 
    })
  
  
  output$resumo2 <- renderText(
    {
      inFile <- input$arquivo
      
      if (is.null(inFile))
        return(NULL)
      
      veg<-read.table(inFile$datapath, header = T,
                      sep = ";", dec=",")
      matriz<-as.data.frame.matrix(table(veg$spp, veg$parc))
      area<-as.numeric(input$area)
      
      
      #numero de parcelas
      nparc<-length(levels(as.factor(veg$parc)))
      
      #area total amostrada
      area.parc=(area*nparc)
      
      #densidade
      dta=length(veg$spp)/(area.parc/10000)
      
      #desvio da densidade entre parcelas
      dtadesv = 0
      dtai = 1
      vetor=1
      while(dtai <= nparc)
      {
        length(vetor) <- nparc
        vetor[dtai] = sum(matriz[,dtai])
        dtadesv = sd(vetor)/(area/10000)
        dtai = dtai + 1
      }
      
      #calcula o numero de ind amostrados
      N<-apply(matriz,1,sum)
      
      #calcula densidades
      DA<-apply(matriz,1,sum)/(area.parc/10000)
      DR<-DA/sum(DA)*100
      
      #calcula frequencias
      freq<-(if (length(dim(matriz)) > 1)
      {apply(matriz > 0,1,sum)} else sum(matriz > 0))
      FA<-(freq/nparc)*100
      FR<-(FA/sum(FA))*100
      
      #checa por NAs nos dados e transforma em zeros
      veg[is.na(veg)] <- 0
      
      #determina se existe "caps" ou "daps" e quais colunas est�o
      cols = grep('cap', colnames(veg))
      ncols = length(cols)
      if (ncols>0) param="cap"
      
      cols2 = grep('dap', colnames(veg))
      ncols2 = length(cols2)
      if (ncols2>0) param="dap"
      
      if (param=="dap") cols=cols2
      if (param=="dap") ncols=ncols2
      
      i=1
      veg$areasec=0
      while (i<=ncols)
      {
        if (param=="cap") veg$areasec<-veg$areasec+((pi*(veg[,cols[i]]/pi)^2)/40000)
        if (param=="dap") veg$areasec<-veg$areasec+((pi*veg[,cols[i]]^2)/40000)
        i=i+1
      }
      
      #calcula as dominancias
      DoA<-tapply(veg$areasec, veg$spp, sum)/(area.parc/10000)
      DoR<-DoA/sum(DoA) *100
      
      # area basal por espécie
      AB<-tapply(veg$areasec, veg$spp, sum)
      
      #area basal
      abta=sum(DoA)
      
      #desvio da area basal entre parcelas
      somag<-tapply(veg$areasec, veg$parc, sum)/(area/10000)
      abdesv = sd(somag)
      VI<-(DR+DoR+FR)/3
      fito=data.frame(N=N,AB=AB,DA=DA,DR=DR,DoA=DoA,DoR=DoR,FA=FA,FR=FR,VI=VI)
      
      #calcula os indices de diversidade
      Pi<-N/sum(N)
      Pi<-Pi*log(Pi)
      SW=-sum(Pi)
      S=nrow(fito)
      J=SW/log(S)
      paste("Área basal total por hectare = ",round(abta,digits=2),"\u00B1",round(abdesv,digits=2),"m2/ha")
    })
  output$resumo3 <- renderText(
    {
      inFile <- input$arquivo
      
      if (is.null(inFile))
        return(NULL)
      
      veg<-read.table(inFile$datapath, header = T,
                      sep = ";", dec=",")
      matriz<-as.data.frame.matrix(table(veg$spp, veg$parc))
      area<-as.numeric(input$area)
      
      
      #numero de parcelas
      nparc<-length(levels(as.factor(veg$parc)))
      
      #area total amostrada
      area.parc=(area*nparc)
      
      #densidade
      dta=length(veg$spp)/(area.parc/10000)
      
      #desvio da densidade entre parcelas
      dtadesv = 0
      dtai = 1
      vetor=1
      while(dtai <= nparc)
      {
        length(vetor) <- nparc
        vetor[dtai] = sum(matriz[,dtai])
        dtadesv = sd(vetor)/(area/10000)
        dtai = dtai + 1
      }
      
      #calcula o numero de ind amostrados
      N<-apply(matriz,1,sum)
      
      #calcula densidades
      DA<-apply(matriz,1,sum)/(area.parc/10000)
      DR<-DA/sum(DA)*100
      
      #calcula frequencias
      freq<-(if (length(dim(matriz)) > 1)
      {apply(matriz > 0,1,sum)} else sum(matriz > 0))
      FA<-(freq/nparc)*100
      FR<-(FA/sum(FA))*100
      
      #checa por NAs nos dados e transforma em zeros
      veg[is.na(veg)] <- 0
      
      #determina se existe "caps" ou "daps" e quais colunas est�o
      cols = grep('cap', colnames(veg))
      ncols = length(cols)
      if (ncols>0) param="cap"
      
      cols2 = grep('dap', colnames(veg))
      ncols2 = length(cols2)
      if (ncols2>0) param="dap"
      
      if (param=="dap") cols=cols2
      if (param=="dap") ncols=ncols2
      
      i=1
      veg$areasec=0
      while (i<=ncols)
      {
        if (param=="cap") veg$areasec<-veg$areasec+((pi*(veg[,cols[i]]/pi)^2)/40000)
        if (param=="dap") veg$areasec<-veg$areasec+((pi*veg[,cols[i]]^2)/40000)
        i=i+1
      }
      
      #calcula as dominancias
      DoA<-tapply(veg$areasec, veg$spp, sum)/(area.parc/10000)
      DoR<-DoA/sum(DoA) *100
      
      # area basal por espécie
      AB<-tapply(veg$areasec, veg$spp, sum)
      
      #area basal
      abta=sum(DoA)
      
      #desvio da area basal entre parcelas
      somag<-tapply(veg$areasec, veg$parc, sum)/(area/10000)
      abdesv = sd(somag)
      VI<-(DR+DoR+FR)/3
      fito=data.frame(N=N,AB=AB,DA=DA,DR=DR,DoA=DoA,DoR=DoR,FA=FA,FR=FR,VI=VI)
      
      #calcula os indices de diversidade
      Pi<-N/sum(N)
      Pi<-Pi*log(Pi)
      SW=-sum(Pi)
      S=nrow(fito)
      J=SW/log(S)
      paste("Riqueza = ",S,"esp.")
    })
  
  output$resumo4 <- renderText(
    {
      inFile <- input$arquivo
      
      if (is.null(inFile))
        return(NULL)
      
      veg<-read.table(inFile$datapath, header = T,
                      sep = ";", dec=",")
      matriz<-as.data.frame.matrix(table(veg$spp, veg$parc))
      area<-as.numeric(input$area)
      
      
      #numero de parcelas
      nparc<-length(levels(as.factor(veg$parc)))
      
      #area total amostrada
      area.parc=(area*nparc)
      
      #densidade
      dta=length(veg$spp)/(area.parc/10000)
      
      #desvio da densidade entre parcelas
      dtadesv = 0
      dtai = 1
      vetor=1
      while(dtai <= nparc)
      {
        length(vetor) <- nparc
        vetor[dtai] = sum(matriz[,dtai])
        dtadesv = sd(vetor)/(area/10000)
        dtai = dtai + 1
      }
      
      #calcula o numero de ind amostrados
      N<-apply(matriz,1,sum)
      
      #calcula densidades
      DA<-apply(matriz,1,sum)/(area.parc/10000)
      DR<-DA/sum(DA)*100
      
      #calcula frequencias
      freq<-(if (length(dim(matriz)) > 1)
      {apply(matriz > 0,1,sum)} else sum(matriz > 0))
      FA<-(freq/nparc)*100
      FR<-(FA/sum(FA))*100
      
      #checa por NAs nos dados e transforma em zeros
      veg[is.na(veg)] <- 0
      
      #determina se existe "caps" ou "daps" e quais colunas est�o
      cols = grep('cap', colnames(veg))
      ncols = length(cols)
      if (ncols>0) param="cap"
      
      cols2 = grep('dap', colnames(veg))
      ncols2 = length(cols2)
      if (ncols2>0) param="dap"
      
      if (param=="dap") cols=cols2
      if (param=="dap") ncols=ncols2
      
      i=1
      veg$areasec=0
      while (i<=ncols)
      {
        if (param=="cap") veg$areasec<-veg$areasec+((pi*(veg[,cols[i]]/pi)^2)/40000)
        if (param=="dap") veg$areasec<-veg$areasec+((pi*veg[,cols[i]]^2)/40000)
        i=i+1
      }
      
      #calcula as dominancias
      DoA<-tapply(veg$areasec, veg$spp, sum)/(area.parc/10000)
      DoR<-DoA/sum(DoA) *100
      
      # area basal por espécie
      AB<-tapply(veg$areasec, veg$spp, sum)
      
      #area basal
      abta=sum(DoA)
      
      #desvio da area basal entre parcelas
      somag<-tapply(veg$areasec, veg$parc, sum)/(area/10000)
      abdesv = sd(somag)
      
      #calcula o indice de valor de importancia
      VI<-(DR+DoR+FR)/3
      
      #monta a tabela
      fito=data.frame(N=N,AB=AB,DA=DA,DR=DR,DoA=DoA,DoR=DoR,FA=FA,FR=FR,VI=VI)
      
      #fito
      
      
      #calcula os indices de diversidade
      Pi<-N/sum(N)
      Pi<-Pi*log(Pi)
      SW=-sum(Pi)
      S=nrow(fito)
      J=SW/log(S)
      Hill= exp(SW)
      paste("Índice de Shannon-Wiener (H') = ",round(SW, 2))
    })
  
  
  
  output$resumo5 <- renderText(
    {
      inFile <- input$arquivo
      
      if (is.null(inFile))
        return(NULL)
      
      veg<-read.table(inFile$datapath, header = T,
                      sep = ";", dec=",")
      matriz<-as.data.frame.matrix(table(veg$spp, veg$parc))
      area<-as.numeric(input$area)
      
      
      #numero de parcelas
      nparc<-length(levels(as.factor(veg$parc)))
      
      #area total amostrada
      area.parc=(area*nparc)
      
      #densidade
      dta=length(veg$spp)/(area.parc/10000)
      
      #desvio da densidade entre parcelas
      dtadesv = 0
      dtai = 1
      vetor=1
      while(dtai <= nparc)
      {
        length(vetor) <- nparc
        vetor[dtai] = sum(matriz[,dtai])
        dtadesv = sd(vetor)/(area/10000)
        dtai = dtai + 1
      }
      
      #calcula o numero de ind amostrados
      N<-apply(matriz,1,sum)
      
      #calcula densidades
      DA<-apply(matriz,1,sum)/(area.parc/10000)
      DR<-DA/sum(DA)*100
      
      #calcula frequencias
      freq<-(if (length(dim(matriz)) > 1)
      {apply(matriz > 0,1,sum)} else sum(matriz > 0))
      FA<-(freq/nparc)*100
      FR<-(FA/sum(FA))*100
      
      #checa por NAs nos dados e transforma em zeros
      veg[is.na(veg)] <- 0
      
      #determina se existe "caps" ou "daps" e quais colunas est
      cols = grep('cap', colnames(veg))
      ncols = length(cols)
      if (ncols>0) param="cap"
      
      cols2 = grep('dap', colnames(veg))
      ncols2 = length(cols2)
      if (ncols2>0) param="dap"
      
      if (param=="dap") cols=cols2
      if (param=="dap") ncols=ncols2
      
      i=1
      veg$areasec=0
      while (i<=ncols)
      {
        if (param=="cap") veg$areasec<-veg$areasec+((pi*(veg[,cols[i]]/pi)^2)/40000)
        if (param=="dap") veg$areasec<-veg$areasec+((pi*veg[,cols[i]]^2)/40000)
        i=i+1
      }
      
      #calcula as dominancias
      DoA<-tapply(veg$areasec, veg$spp, sum)/(area.parc/10000)
      DoR<-DoA/sum(DoA) *100
      
      # area basal por espécie
      AB<-tapply(veg$areasec, veg$spp, sum)
      
      #area basal
      abta=sum(DoA)
      
      #desvio da area basal entre parcelas
      somag<-tapply(veg$areasec, veg$parc, sum)/(area/10000)
      abdesv = sd(somag)
      
      #calcula o indice de valor de importancia
      VI<-(DR+DoR+FR)/3
      
      #monta a tabela
      fito=data.frame(N=N,AB=AB,DA=DA,DR=DR,DoA=DoA,DoR=DoR,FA=FA,FR=FR,VI=VI)
      
      #fito
      
      
      #calcula os indices de diversidade
      Pi<-N/sum(N)
      Pi<-Pi*log(Pi)
      SW=-sum(Pi)
      S=nrow(fito)
      J=SW/log(S)
      paste("Equabilidade de Pielou (J) = ",round(J,2))

    })
  output$resumo6 <- renderText(
    {
      inFile <- input$arquivo
      
      if (is.null(inFile))
        return(NULL)
      
      veg<-read.table(inFile$datapath, header = T,
                      sep = ";", dec=",")
      matriz<-as.data.frame.matrix(table(veg$spp, veg$parc))
      area<-as.numeric(input$area)
      
      
      #numero de parcelas
      nparc<-length(levels(as.factor(veg$parc)))
      
      #area total amostrada
      area.parc=(area*nparc)
      
      #densidade
      dta=length(veg$spp)/(area.parc/10000)
      
      #desvio da densidade entre parcelas
      dtadesv = 0
      dtai = 1
      vetor=1
      while(dtai <= nparc)
      {
        length(vetor) <- nparc
        vetor[dtai] = sum(matriz[,dtai])
        dtadesv = sd(vetor)/(area/10000)
        dtai = dtai + 1
      }
      
      #calcula o numero de ind amostrados
      N<-apply(matriz,1,sum)
      
      #calcula densidades
      DA<-apply(matriz,1,sum)/(area.parc/10000)
      DR<-DA/sum(DA)*100
      
      #calcula frequencias
      freq<-(if (length(dim(matriz)) > 1)
      {apply(matriz > 0,1,sum)} else sum(matriz > 0))
      FA<-(freq/nparc)*100
      FR<-(FA/sum(FA))*100
      
      #checa por NAs nos dados e transforma em zeros
      veg[is.na(veg)] <- 0
      
      #determina se existe "caps" ou "daps" e quais colunas est
      cols = grep('cap', colnames(veg))
      ncols = length(cols)
      if (ncols>0) param="cap"
      
      cols2 = grep('dap', colnames(veg))
      ncols2 = length(cols2)
      if (ncols2>0) param="dap"
      
      if (param=="dap") cols=cols2
      if (param=="dap") ncols=ncols2
      
      i=1
      veg$areasec=0
      while (i<=ncols)
      {
        if (param=="cap") veg$areasec<-veg$areasec+((pi*(veg[,cols[i]]/pi)^2)/40000)
        if (param=="dap") veg$areasec<-veg$areasec+((pi*veg[,cols[i]]^2)/40000)
        i=i+1
      }
      
      #calcula as dominancias
      DoA<-tapply(veg$areasec, veg$spp, sum)/(area.parc/10000)
      DoR<-DoA/sum(DoA) *100
      
      # area basal por espécie
      AB<-tapply(veg$areasec, veg$spp, sum)
      
      #area basal
      abta=sum(DoA)
      
      #desvio da area basal entre parcelas
      somag<-tapply(veg$areasec, veg$parc, sum)/(area/10000)
      abdesv = sd(somag)
      
      #calcula o indice de valor de importancia
      VI<-(DR+DoR+FR)/3
      
      #monta a tabela
      fito=data.frame(N=N,AB=AB,DA=DA,DR=DR,DoA=DoA,DoR=DoR,FA=FA,FR=FR,VI=VI)
      
      #fito
      
      
      #calcula os indices de diversidade
      Pi<-N/sum(N)
      Pi<-Pi*log(Pi)
      SW=-sum(Pi)
      Hill=exp(SW)
      S=nrow(fito)
      J=SW/log(S)
      paste("Numero de Hill = ",round(Hill,2))
      
    })
  
  ###TABELA FITOSSOCIOLOGICA
  
  datasetInput_fito = function() {
    inFile <- input$arquivo
    
    if (is.null(inFile))
      return(NULL)
    
    veg<-read.table(inFile$datapath, header = T,
                    sep = ";", dec=",")
    matriz<-as.data.frame.matrix(table(veg$spp, veg$parc))
    area<-as.numeric(input$area)
    
    
    #numero de parcelas
    nparc<-length(levels(as.factor(veg$parc)))
    
    #area total amostrada
    area.parc=(area*nparc)
    
    #densidade
    dta=length(veg$spp)/(area.parc/10000)
    
    #desvio da densidade entre parcelas
    dtadesv = 0
    dtai = 1
    vetor=1
    while(dtai <= nparc)
    {
      length(vetor) <- nparc
      vetor[dtai] = sum(matriz[,dtai])
      dtadesv = sd(vetor)/(area/10000)
      dtai = dtai + 1
    }
    
    #calcula o numero de ind amostrados
    N<-apply(matriz,1,sum)
    
    #calcula densidades
    DA<-apply(matriz,1,sum)/(area.parc/10000)
    DR<-DA/sum(DA)*100
    
    #calcula frequencias
    freq<-(if (length(dim(matriz)) > 1)
    {apply(matriz > 0,1,sum)} else sum(matriz > 0))
    FA<-(freq/nparc)*100
    FR<-(FA/sum(FA))*100
    
    #checa por NAs nos dados e transforma em zeros
    veg[is.na(veg)] <- 0
    
    #determina se existe "caps" ou "daps" e quais colunas est�o
    cols = grep('cap', colnames(veg))
    ncols = length(cols)
    if (ncols>0) param="cap"
    
    cols2 = grep('dap', colnames(veg))
    ncols2 = length(cols2)
    if (ncols2>0) param="dap"
    
    if (param=="dap") cols=cols2
    if (param=="dap") ncols=ncols2
    
    i=1
    veg$areasec=0
    while (i<=ncols)
    {
      if (param=="cap") veg$areasec<-veg$areasec+((pi*(veg[,cols[i]]/pi)^2)/40000)
      if (param=="dap") veg$areasec<-veg$areasec+((pi*veg[,cols[i]]^2)/40000)
      i=i+1
    }
    
    #calcula as dominancias
    DoA<-tapply(veg$areasec, veg$spp, sum)/(area.parc/10000)
    DoR<-DoA/sum(DoA) *100
    
    # area basal por espécie
    AB<-tapply(veg$areasec, veg$spp, sum)
    
    #area basal
    abta=sum(DoA)
    
    #desvio da area basal entre parcelas
    somag<-tapply(veg$areasec, veg$parc, sum)/(area/10000)
    abdesv = sd(somag)
    
    #calcula o indice de valor de importancia
    VI<-(DR+DoR+FR)/3
    
    
    fito<-data.frame(N=N,DA=DA,DR=DR,DoA=DoA,
                     DoR=DoR,FA=FA,FR=FR,VI=VI)
    
    fito<-round(fito, digits = 2)
    
    fito <- fito[order(VI, decreasing = TRUE),]
    #   
    fito
    #   
  }
  # 
  # Imprimindo na tela
  output$fito <- renderTable(rownames = TRUE,striped = F,
                             bordered = T,
                             {
                               datasetInput_fito()
                             })
  
  
  
  # Arquivo para download
  output$downloadTab1 <- downloadHandler(
    filename = function() {
      paste("tabela_fito",  Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.table(datasetInput_fito(), file, row.names = TRUE, dec=",", sep=";")
    }
  )
  
  
  ###ANALISE AMOSTRAGEM
  
  
  ###TABELA DE INTERPOLACAO E EXTRAPOLACAO
  
  datasetInput_inter_extra = function() {
    inFile <- input$arquivo
    
    if (is.null(inFile))
      return(NULL)
    
    veg<-read.table(inFile$datapath, header = T,
                    sep = ";", dec=",")
    matriz<-as.data.frame.matrix(table(veg$parc, veg$spp))
    q<-as.numeric(input$typeInput)
    curva<-iNEXT(t(matriz), q=0, datatype="incidence_raw")
    curva$iNextEst
  }
  
  # Imprimindo na tela
  output$inter_extra <- renderTable(striped = TRUE,
                                    bordered = T,{
                                      datasetInput_inter_extra()
                                    })
  
  
  
  
  # Arquivo para download
  output$downloadTab_diversidade <- downloadHandler(
    filename = function() {
      paste("tabela_inter_extra", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.table(datasetInput_inter_extra(), file, row.names = FALSE, dec=",", sep=";")
    }
  )
  
  
  ##CURVA DE ACUMULACAO
  
  output$myplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$arquivo
    
    if (is.null(inFile))
      return(NULL)
    
    veg<-read.table(inFile$datapath, header = T,
                    sep = ";", dec=",")
    matriz<-as.data.frame.matrix(table(veg$parc, veg$spp))
    q<-as.numeric(input$typeInput)
    curva<-iNEXT(t(matriz), q=0, datatype="incidence_raw")
    c<-as.numeric(input$curvaInput)
    ggiNEXT(curva, type = c)
  })
  
  #EXPORTACAO
  
  plotInput = function() {
    inFile <- input$arquivo
    
    if (is.null(inFile))
      return(NULL)
    
    veg<-read.table(inFile$datapath, header = T,
                    sep = ";", dec=",")
    matriz<-as.data.frame.matrix(table(veg$parc, veg$spp))
    #q<-as.numeric(input$typeInput)
    curva<-iNEXT(t(matriz), q=0, datatype="incidence_raw")
    c<-as.numeric(input$curvaInput)
    ggiNEXT(curva, type = c)
  }
  
  output$downloadPlot = downloadHandler(
    filename = 'curva.jpg',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::jpeg(..., width = 2500, height = 2000,
                        res = 300, units = "px")
      }
      ggsave(file, plot = plotInput(), device = device)
    })
  

  
  ###ESPECIES INDICADORAS



   info <- eventReactive(input$arquivo, {
    inFile <- input$arquivo
     # Instead # if (is.null(inFile)) ... use "req"
     req(inFile)

     # Changes in read.table
    veg <- read.table(inFile$datapath, header = T,
                     sep = ";", dec=",")
     vars <- names(veg)
     # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "columns1","Selecione as Parcelas", choices = vars)
     updateSelectInput(session, "columns","Selecione os Setores", choices = vars)
     
     
     veg
     
   })

   output$indicadoras <- renderTable(rownames = TRUE, striped = F,
                                     bordered = T, digits = 4,
                                     {
     indicadoras <- info()

     #setores<-subset(indicadoras, select = c(input$columns))
     #veg$setores<-setores
     inFile <- input$arquivo
     veg <- read.table(inFile$datapath, header = T,
                       sep = ";", dec=",")
    sub_veg <- subset(indicadoras, select = c(input$columns1,input$columns)) #subsetting takes place here
    matriz_ind<-table(sub_veg[,1], sub_veg[,2])
    matriz_ind<-as.data.frame(as.table(matriz_ind))
    matriz_ind<-matriz_ind[matriz_ind$Freq>0,]
    setores<-matriz_ind$Var2
    matriz<-as.data.frame.matrix(table(veg$parc, veg$spp))
    indicadoras_resultado<-(indval(matriz, setores))
    indicadoras_table<-cbind(indicadoras_resultado$indval,indicadoras_resultado$pval)
    colnames(indicadoras_table)[c(3)]<-"p"
    indicadoras_significativas<-(subset(indicadoras_table, p < 0.05))
    (indicadoras_significativas)


   })


   # Arquivo para download
   output$download_ind <- downloadHandler(
     filename = function() {
       paste("tabela_indicadoras",Sys.Date(), ".csv", sep = "")
     },
     content = function(file) {
       indicadoras <- info()
       inFile <- input$arquivo
       veg <- read.table(inFile$datapath, header = T,
                         sep = ";", dec=",")
       sub_veg <- subset(indicadoras, select = c(input$columns1,input$columns)) #subsetting takes place here
       matriz_ind<-table(sub_veg$parc, sub_veg$exp)
       matriz_ind<-as.data.frame(as.table(matriz_ind))
       matriz_ind<-matriz_ind[matriz_ind$Freq>0,]
       setores<-matriz_ind$Var2
       matriz<-as.data.frame.matrix(table(veg$parc, veg$spp))
       indicadoras_resultado<-(indval(matriz, setores))
       indicadoras_table<-cbind(indicadoras_resultado$indval,indicadoras_resultado$pval)
       colnames(indicadoras_table)[c(3)]<-"p"
       indicadoras_significativas<-(subset(indicadoras_table, p < 0.05))
       write.table(indicadoras_significativas, file, row.names = TRUE, dec=",", sep=";")
     }


   )
  }