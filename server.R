library(readxl)
library(knitr)
library(dplyr)
library(tidyr)
library(sunburstR)
library(fastDummies)
library(caret)
library(verification)
library(plotly)
library(dplyr)
library(questionr)
library(ROCR)
library(plotROC)
library(rpart)
library(rpart.plot)
library(stringr)
library(shinydashboard)
#library(devtools)
library(shiny)
library(gmodels)
library(readr)



library(dashboardthemes)
library(plotly)
library(plyr)
library(sunburstR)
library(DataExplorer)
library(corrr)
library(tidyverse)
library(networkD3)
library(corrplot)
library(ggforce) # for 'geom_arc_bar'
library(ggplot2)
library(yarrr)
library(ggpirate)
library(DescTools)
library(dashboardthemes)
library(ResourceSelection)
qualit=c("Solde","HistCr","Motif","ComEp","EmpDur","StatPer","GarDeb","Log","Emp","Statut")

quanti=c("DurCr","MontCr","ResDur","Age","NbrCr","NbrPer")

semaine=9
Parcours2 <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(Parcours2) <- c("Nom", "Prenom", "Identifiant" , "NiveauAtteint" )

Parcours1 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(Parcours1) <- c("Nom", "Prenom", "Identifiant" ,"Fond","Pro", "PontSup" , "NiveauAtteint" )
STATI <- data.frame(matrix(ncol = 11, nrow = 0))


colnames(STATI) <- c("Nom", "Prenom", "Identifiant","DerniereUtilisation" ,"TempsTotalPasse", "ScoreEvalInitial" , "NiveauInitial"  , "TempsEntrainement" , "NiveauAtteint"  , "Parc1" , "Parc2"  )
#Parcours2=data.frame(Nom, Prenom, Identifiant , NiveauAtteint )
#Parcours1=data.frame(Nom, Prenom, Identifiant ,Fond,Pro, PontSup , NiveauAtteint )



shinyServer(function(input, output,session) {
  
  tags$style(type="text/css", ".tab-content { overflow: visible; }")
  tags$head(
    tags$style(type="text/css", "html {overflow:hidden;}"))
  
  filedata <- reactive({
    inFile <- input$csv_file1
    if (is.null(inFile)){
      return(NULL)}
    
    data <- read_delim(inFile$datapath , 
                        ";", escape_double = FALSE, 
                        trim_ws = TRUE)
    nn=str_remove( data$`Niveau atteint`,"%")
    data$`Niveau atteint`=as.numeric(nn)/100
    
    ni=str_remove( data$`Niveau initial`,"%")
    data$`Niveau initial`=as.numeric(ni)/100
    data
    #colnames(data2) <- c("Solde","DurCr","HistCr","Motif","MontCr","ComEp","EmpDur","StatPer","GarDeb","ResDur","Age","Log","NbrCr","Emp","NbrPer","Statut")
    #colnames(data) <- c("Solde","DurCr","HistCr","Motif","MontCr","ComEp","EmpDur","StatPer","GarDeb","ResDur","Age","Log","NbrCr","Emp","NbrPer","Statut")
    
    
    
    
  })
  
  
  
  
  filedataD <- reactive({
    inFile <- input$csv_fileD
    if (is.null(inFile)){
      return(NULL)}
    data <- read_excel(inFile$datapath )
    data2=data
    colnames(data2) <- c("Solde","DurCr","HistCr","Motif","MontCr","ComEp","EmpDur","StatPer","GarDeb","ResDur","Age","Log","NbrCr","Emp","NbrPer","Statut")
    colnames(data) <- c("Solde","DurCr","HistCr","Motif","MontCr","ComEp","EmpDur","StatPer","GarDeb","ResDur","Age","Log","NbrCr","Emp","NbrPer","Statut")
    
    data2[, 1:16] <- sapply( data2[, 1:16], as.character)
    data2[, 1:16] <- sapply(data2[, 1:16], as.numeric)
    data2$Solde=as.factor(data2$Solde)
    data2$HistCr=as.factor(data2$HistCr)
    data2$Motif=as.factor(data2$Motif)
    data2$ComEp=as.factor(data2$ComEp)
    data2$EmpDur=as.factor(data2$EmpDur)
    data2$StatPer=as.factor(data2$StatPer)
    data2$GarDeb=as.factor(data2$GarDeb)
    data2$Log=as.factor(data2$Log)
    data2$Emp=as.factor(data2$Emp)
    data2$Statut=as.factor(data2$Statut)
    
    levels(data2$Solde)=c("<0","0-200",">=200","pasCC")
    levels(data2$HistCr)=c("aucun","banque remb","remb jus'à maint","retard dans passé ","compte critique")
    levels(data2$Motif)=c("Voit Nouv","Voit d’occ"," fourn","Télévision"
                          ,"App élec","Répar","Educ","Recyc","Projet","Autres")
    levels(data2$ComEp)=c("<100","100-500","500-1000",">1000","PasCE")
    levels(data2$EmpDur)=c("chom","<1an","1-4ans","4-7ans",">7ans")
    levels(data2$StatPer)=c("H:Div",
                            "F:Div/Mar",
                            "H:Célib",
                            "H:Mar/Veuf",
                            "F:Célib")
    levels(data2$GarDeb)=c( "Aucun", "Codemandeur","Garant" )
    levels(data2$Log)=c("Loc","Prop" ,"Grat")
    levels(data2$Emp)=c("Chom/NNqual-NNrés",
                        "NNqual-rés", "Emp/Fonc" ,"Man/Indép/OuvQual")
    levels(data2$Statut)=c("sol","non sol")
    
    data2
  })
  
  output$filedataDd= DT::renderDataTable({
    stage2= filedataD()
    stage2
    
  }, options = list(scrollX = TRUE))
  
  
  
  niv1 <- reactive({
    inFile <- input$csv_file2
    if (is.null(inFile)){
      return(NULL)}
    
    data <- read_delim(inFile$datapath , 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
    data
    
    
  })
  
  
  
  
  
  

  
  
  nivff<-reactive({
    niveau=niv1()
    # STATI= statii()
    # id1=as.factor(feuille1$Identifiant)
    # P1=1
    # P2=1
    # ST=1
    # ID=levels(id1)
    # 
    # 
    # 
    # for (i in 1:length(STATI$Identifiant)) {
    #   for (j in 1:length(niveau$Identifiant)) {
    #     if (STATI$Identifiant[i]==niveau$Identifiant[j]) 
    #     {niveau[j,semaine+5]=STATI$NiveauAtteint[i]
    #     niveau[j,4]=STATI$NiveauInitial[i]  }
    #   }
    #  
    # }
    
    niveau
  })
 
  output$nivf= DT::renderDataTable({
    stage2= nivff()
    stage2
    
  }, options = list(scrollX = TRUE))
  
  
  eff1 <- reactive({
    inFile <- input$csv_file3
    if (is.null(inFile)){
      return(NULL)}
    
    data <- read_delim(inFile$datapath , 
                       ";", escape_double = FALSE, 
                       trim_ws = TRUE)
    data
    
    
  })

  
  effortff<-reactive({
    effort=eff1()
    # STATI= statii()
    # id1=as.factor(feuille1$Identifiant)
    # P1=1
    # P2=1
    # ST=1
    # ID=levels(id1)
    # 
    # 
    # 
    # for (i in 1:length(STATI$Identifiant)) {
    #   for (j in 1:length(niveau$Identifiant)) {
    #     if (STATI$Identifiant[i]==niveau$Identifiant[j]) 
    #     {niveau[j,semaine+5]=STATI$NiveauAtteint[i]
    #     niveau[j,4]=STATI$NiveauInitial[i]  }
    #   }
    #  
    # }
    
    effort
  })
  
  output$effortf= DT::renderDataTable({
    stage2= eff1()
    stage2
    
  }, options = list(scrollX = TRUE))
  
  
  
  
  

  
  
  pcr2<-reactive({
    feuille1=filedata()
    id1=as.factor(feuille1$Identifiant)
    P1=1
    P2=1
    ST=1
    ID=levels(id1)
    
    for (i in ID) {
      for (j in 1:length(feuille1$Identifiant))
      {if ((i==feuille1$Identifiant[j])&& (feuille1$Module[j] =="Orthographe - Supérieur"))
      {Nom2=feuille1$Nom[j]
      Prenom2=feuille1$Prénom[j]
      Identifiant2=feuille1$Identifiant[j]
      if ((feuille1$`Tps d'entraînement`[j]!=0 &&(is.na(feuille1$`Tps d'entraînement`[j])!=TRUE)) ) {    NiveauAtteint2=feuille1$`Niveau atteint`[j] }
      else (NiveauAtteint2=0)
      P2=P2+1
      Parcours2=rbind(Parcours2,list(Nom2,Prenom2,Identifiant2,NiveauAtteint2)) 
      }
        
        
        
      }
    }
    colnames(Parcours2) <- c("Nom", "Prenom", "Identifiant" , "NiveauAtteint" )
    Parcours2
  })
  
  
  
  
  
  pcr1<-reactive({
    feuille1=filedata()
    id1=as.factor(feuille1$Identifiant)
    P1=1
    P2=1
    ST=1
    ID=levels(id1)
    
    for (i in ID) {
      for (j in 1:length(feuille1$Identifiant))
      {
        if ((i==feuille1$Identifiant[j])&&(feuille1$Module[j]=="Orthographe - Les Fondamentaux Campus"))
        {Nom1=feuille1$Nom[j]
        Prenom1=feuille1$Prénom[j]
        Identifiant1=feuille1$Identifiant[j]
        if ((feuille1$`Tps d'entraînement`[j]!=0 &&(is.na(feuille1$`Tps d'entraînement`[j])!=TRUE)) ) {    Fond1=feuille1$`Niveau atteint`[j] }
        else (Fond1=0)
        }
        if ((i==feuille1$Identifiant[j])&&(feuille1$Module[j]=="Orthographe - Pro"))
        { 
          if ((feuille1$`Tps d'entraînement`[j]!=0)&&(is.na(feuille1$`Tps d'entraînement`[j])!=TRUE)) {    Pro1=feuille1$`Niveau atteint`[j] }
          else (Pro1=0)
        }
        if ((i==feuille1$Identifiant[j])&&(feuille1$Module[j]=="Orthographe - Pont supérieur"))
        {if ((feuille1$`Tps d'entraînement`[j]!=0)&&(is.na(feuille1$`Tps d'entraînement`[j])!=TRUE)) {  PontSup1=feuille1$`Niveau atteint`[j] }
          else (PontSup1=0)
          
          
          NiveauAtteint1=0.44*Pro1+0.27*Fond1+0.29*PontSup1
          #NiveauAtteint1=0.45*Pro1+0.25*Fond1+0.30*PontSup1
          Parcours1=rbind(Parcours1,list(Nom1,Prenom1,Identifiant1,Fond1,Pro1,PontSup1,NiveauAtteint1))
          P1=P1+1}
        
        
      }
    }
    colnames(Parcours1) <- c("Nom", "Prenom", "Identifiant" ,"Fond","Pro", "PontSup" , "NiveauAtteint" )
    Parcours1
    
  })
  
  statii<-reactive({
    feuille1=filedata()
    Parcours2= pcr2()
    Parcours1=pcr1()
    id1=as.factor(feuille1$Identifiant)
    P1=1
    P2=1
    ST=1
    ID=levels(id1)
    for (i in 1:length(Parcours1$Identifiant)) {
      NomS=Parcours1$Nom[i]
      PrenomS=Parcours1$Prenom[i]
      IdentifiantS=Parcours1$Identifiant[i]
      NiveauAtteintS=Parcours1$NiveauAtteint[i]
      Parc1S=Parcours1$NiveauAtteint[i]
      for (j in 1:length(feuille1$Identifiant))
      {if ((IdentifiantS==feuille1$Identifiant[j])&& (feuille1$Module[j] =="Orthographe - Tous modules"))
      {DerniereUtilisationS=feuille1$`Dernière utilisation`[j]
      TempsTotalPasseS=feuille1$`Tps total passé`[j]
      ScoreEvalInitialS=feuille1$`Score évaluation initiale`[j]
      NiveauInitialS=feuille1$`Niveau initial`[j]
      TempsEntrainementS=feuille1$`Tps d'entraînement`[j]
      STATI=rbind(STATI,list(NomS,PrenomS,IdentifiantS,DerniereUtilisationS,TempsTotalPasseS,ScoreEvalInitialS,NiveauInitialS,TempsEntrainementS,NiveauAtteintS,Parc1S,0))
      }
      }
    }
    
    
    for (i in 1:length(Parcours2$Identifiant)) {
      NomS=Parcours2$Nom[i]
      PrenomS=Parcours2$Prenom[i]
      IdentifiantS=Parcours2$Identifiant[i]
      NiveauAtteintS=Parcours2$NiveauAtteint[i]
      Parc2S=Parcours2$NiveauAtteint[i]
      for (j in 1:length(feuille1$Identifiant))
      {if ((IdentifiantS==feuille1$Identifiant[j])&& (feuille1$Module[j] =="Orthographe - Tous modules"))
      {DerniereUtilisationS=feuille1$`Dernière utilisation`[j]
      TempsTotalPasseS=feuille1$`Tps total passé`[j]
      ScoreEvalInitialS=feuille1$`Score évaluation initiale`[j]
      NiveauInitialS=feuille1$`Niveau initial`[j]
      TempsEntrainementS=feuille1$`Tps d'entraînement`[j]
      STATI=rbind(STATI,list(NomS,PrenomS,IdentifiantS,DerniereUtilisationS,TempsTotalPasseS,ScoreEvalInitialS,NiveauInitialS,TempsEntrainementS,NiveauAtteintS,0,Parc2S))
      }
      }
    }
    library(operator.tools)
    colnames(STATI) <- c("Nom", "Prenom", "Identifiant","DerniereUtilisation" ,"TempsTotalPasse", "ScoreEvalInitial" , "NiveauInitial"  , "TempsEntrainement" , "NiveauAtteint"  , "Parc1" , "Parc2"  )
    
    for (i in ID) {
      
      for (j in 1:length(feuille1$Identifiant))
      {if ((i==feuille1$Identifiant[j])&& (feuille1$Module[j] =="Orthographe - Tous modules")&&(i %!in% STATI$Identifiant) )
      {NomS=feuille1$Nom[j]
      PrenomS=feuille1$Prénom[j]
      IdentifiantS=feuille1$Identifiant[j]
      STATI=rbind(STATI,list(NomS,PrenomS,IdentifiantS,NA,0,0,0,0,0,0,0))
      colnames(STATI) <- c("Nom", "Prenom", "Identifiant","DerniereUtilisation" ,"TempsTotalPasse", "ScoreEvalInitial" , "NiveauInitial"  , "TempsEntrainement" , "NiveauAtteint"  , "Parc1" , "Parc2"  )
      
      }}
      
    }
    colnames(STATI) <- c("Nom", "Prenom", "Identifiant","DerniereUtilisation" ,"TempsTotalPasse", "ScoreEvalInitial" , "NiveauInitial"  , "TempsEntrainement" , "NiveauAtteint"  , "Parc1" , "Parc2"  )
    STATI
    
    
  })
  

  filedata1 <- reactive({
    inFile <- input$csv_file1
    if (is.null(inFile)){
      return(NULL)}
    data <- read_excel(inFile$datapath ,
                       sheet = "Feuil1", col_types = c("text", 
                                                       "text", "text", "text", "text", "numeric", 
                                                       "text", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "text", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric"))
    #colnames(data) <- c("Solde","DurCr","HistCr","Motif","MontCr","ComEp","EmpDur","StatPer","GarDeb","ResDur","Age","Log","NbrCr","Emp","NbrPer","Statut")
    
    data
  })
  
  
  output$filetable= DT::renderDataTable({
    stage2= filedata()
    stage2
    
  }, options = list(scrollX = TRUE))
  
  output$pcrr2= DT::renderDataTable({
     pcr2()
    
  }, options = list(scrollX = TRUE))
  
  output$pcrr1= DT::renderDataTable({
    pcr1()
    
  }, options = list(scrollX = TRUE))
  
  output$statii1= DT::renderDataTable({
    statii()
    
  }, options = list(scrollX = TRUE))
  
  
  output$Plotly <- renderPlotly({
    
    stage2= filedata()
    
    
    attach(stage2)
    
    a=factor(stage2[[input$var1]])
    b=factor(stage2[[input$var2]])
    x=ggplot(stage2,aes(a,fill=b))+geom_bar(stat="count")+
      coord_flip()+
      xlab(input$var1)+ylab("Nombre des clients")+
      scale_fill_hue(input$var2)+
      geom_text(aes(label=..count..),stat="count",position=position_stack())
    ggplotly(x) 
  }) 
  output$structure111 <- renderDiagonalNetwork({
    
    data2= filedata()
    x=plot_str(data2)
    diagonalNetwork(x)
    
  }) 
  
  output$summary <- renderPrint({
    
    
    stage2= filedata()
    attach(stage2)
    
    
    summary(stage2[[input$var3]])
    
  })
  output$summary1 <- renderPrint({
    
    
    stage2= filedata()

    
    summary(stage2)
    
  })
  output$correlation <-  renderPlot({
    
    
    stage2= filedata1()
    
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(cor( stage2), method="color", col=col(200) ,
             type="upper",number.cex= 0.8,
             addCoef.col = "black", order="hclust")
  })
  output$networkplot <-  renderPlot({
    
    
    stage2= filedata1()
    
    stage2%>%
      correlate() %>%
      network_plot(min_cor = .15, colors = c("red","gray", "blue"))
  })
  output$count <- renderPrint({
    
    
    stage2= filedata()
    
    variable=input$var1
    count(stage2 , vars =variable)
    
  })
  output$plothist1 <- renderPlot({
    
    
    stage2= pcr2()
    
    plot(density(stage2[,4]),main = "densite de niveau atteint",xlab="Niveau atteint")
  })
  output$plotpie <- renderPlot({
    
    
    stage2= pcr2()
    library(ggplot2)
    # Histogramme basique
    ggplot(stage2, aes(x=stage2[,4])) + geom_histogram(color="black", fill="green")+ labs(title="histogramme niveau",x="NIveau atteint", y = "Count")

    # pie1=data.frame(table( stage2[,variable]))
    # df=pie1
    # df <- df %>% 
    #   mutate(end = 2 * pi * cumsum(Freq)/sum(Freq),
    #          start = lag(end, default = 0),
    #          middle = 0.5 * (start + end),
    #          hjust = ifelse(middle > pi, 1, 0),
    #          vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
    # ggplot(df) + 
    #   geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
    #                    start = start, end = end, fill = Var1)) +
    #   geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = Freq/10,
    #                 hjust = hjust, vjust = vjust)) +
    #   coord_fixed() +
    #   scale_x_continuous(limits = c(-1.5, 1.4),  # Adjust so labels are not cut off
    #                      name = "", breaks = NULL, labels = NULL) +
    #   scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
    #                      name = "", breaks = NULL, labels = NULL)+
    #   scale_fill_brewer(palette="Paired")  + labs(fill = "category")
    
  })
  
  
  output$summary2 <- renderPrint({
    
    
    stage2= filedata()
    
    variable=input$var9
    summary(stage2[,variable])    
  })
  
  
  output$boxp1 <- renderPlot({
    
    
    stage2= pcr1()
    
    plot(density(stage2[,7]),main = "densite de niveau atteint",xlab="Niveau atteint")
    # ggplot(stage2, aes(x=stage2[,7])) + 
    #   geom_density()+ geom_vline(aes(xintercept=mean(stage2[,7])),color="blue", linetype="dashed", size=1)+
    #   labs(title="courbe repartition niveau atteint",x="Niveau", y = "Density")
    # 
    #•x=ggplot(stage2, aes_string(variable)) +  geom_boxplot()+ coord_flip()
    
   
    
  })
  output$histdensity <- renderPlot({
    
    
    stage2= pcr1()
    library(ggplot2)
    # Histogramme basique
    ggplot(stage2, aes(x=stage2[,7])) + geom_histogram(color="black", fill="green")+ labs(title="histogramme niveau",x="NIveau atteint", y = "Count")
    
    # stage2= filedata()
    # attach(stage2)
    # variable=input$var9
    # 
    # ggplot(data=stage2, aes_string(variable)) + 
    #   geom_histogram(aes(y =..density..,fill=..count..), 
    #                  bins=15, 
    #                  col="black", 
    #                  alpha=.5) + 
    #   geom_density(alpha=.4,col=4, fill="gray88") +
    #   scale_fill_gradient("Count", low="blue", high="red") + 
    #   labs(title="Histogram ", x=variable, y="density") +theme_minimal()

  })
  output$repartition <- renderPlot({
     stage2= statii()
    #variable=input$var9
    #ggplot(stage2, aes_string(stage2[,8])) + stat_ecdf(geom = "step")
     ggplot(stage2, aes(x=stage2[,9])) + 
       geom_density()+ geom_vline(aes(xintercept=mean(stage2[,9])),color="blue", linetype="dashed", size=1)+
       labs(title="courbe repartition niveau atteint",x="Niveau", y = "Density")

  })
  
  output$crsstab <- renderPlot({
    stage2= statii()
    attach(stage2)
    ggplot(stage2, aes(x=stage2[,9])) + geom_histogram(color="black", fill="green")+ labs(title="histogramme niveau",x="NIveau atteint", y = "Count")
    
   #  variable=input$var10
   #  vari=unlist( stage2[,variable])
   #  #x=as.data.frame.matrix(table(data.frame(vari,Statut)))
   #  #x
   # 
   # CrossTable(vari,Statut, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
   #  
  })
  
  
  
  output$ggbarx <- renderPlot({
    stage2= statii()
    #variable=input$var9
    #ggplot(stage2, aes_string(stage2[,8])) + stat_ecdf(geom = "step")
    ggplot(stage2, aes(x=stage2[,8])) + 
      geom_density()+ geom_vline(aes(xintercept=mean(stage2[,8])),color="blue", linetype="dashed", size=1)+
      labs(title="courbe repartition Temps entrainement",x="Temps entrainement", y = "Count")
    
    
  })
  output$ggbarx1 <- renderPlot({
    stage2= statii()
    #variable=input$var9
    #ggplot(stage2, aes_string(stage2[,8])) + stat_ecdf(geom = "step")
    ggplot(stage2, aes(x=stage2[,8])) + 
      geom_histogram(aes(y=..count..), colour="black", fill="green")+
      geom_density()+ geom_vline(aes(xintercept=mean(stage2[,8])),color="blue", linetype="dashed", size=1)+
      labs(title="courbe repartition Temps entrainement",x="Temps entrainement", y = "Density")
    
    
  })
  output$ggbarx2 <- renderPlot({
    stage2= statii()
    #variable=input$var9
    #ggplot(stage2, aes_string(stage2[,8])) + stat_ecdf(geom = "step")
    library(lubridate)
    ggplot(stage2,aes(TempsEntrainement,NiveauAtteint))+geom_point(color='blue')+   geom_smooth(method='lm')

  })
  output$ggbarx3<- renderPlot({
    stage2= statii()
    #variable=input$var9
    #ggplot(stage2, aes_string(stage2[,8])) + stat_ecdf(geom = "step")
    library(lubridate)
    HMS = seconds.to.hms(stage2$TempsEntrainement) 
    library(kimisc)
    library(dplyr)
    library(stringr)
    ggplot(stage2,aes( HMS,NiveauAtteint))+geom_point(color='blue')+   geom_smooth(method='lm')#+geom_smooth(method='lm',formula=NiveauAtteint~TempsEntrainement)
    
  })
  
  
  
  output$test22 <- renderPrint({
    
    
    stage2= filedata()
    
    variable=input$var10
    XD=table(unlist( stage2[,variable]),unlist( stage2[,"Statut"]))
    chisq.test(XD)
  })
  output$test23 <- renderPrint({
    
    
    stage2= filedata()
    
    variable=input$var10
    XD=table(unlist( stage2[,variable]),unlist( stage2[,"Statut"]))
    chisq.residuals(XD) 
  })
  
  
  output$graphniv <- renderPlotly({
    stage2= nivff()
    niveau1=stage2[,c("Identifiant","S1","S2","S3","S4","S5","S6","S7","S8")]
    surveys_gather <- niveau1 %>%
      gather(key = "semaine", value = "niveaux", -Identifiant)
    
    j=ggplot(data = surveys_gather, aes(x = semaine, y = niveaux,group=Identifiant ,color=Identifiant)) +
      geom_line()
    ggplotly(j,height = 800)
  })
  
  
  output$grapheff <- renderPlotly({
    effort= effortff()
    effort1=effort[,c("Identifiant","S1","S2","S3","S4","S5","S6","S7","S8")]
    surveys_gather <- effort1 %>%
      gather(key = "semaine", value = "temps_entr", -Identifiant)
    
    j=ggplot(data = surveys_gather, aes(x = semaine, y = temps_entr,group=Identifiant ,color=Identifiant)) +
      geom_line()

    ggplotly(j,height = 800)
  })
  
  
  
  
  output$test11 <- renderPrint({
    
    
    stage2= filedata()
    
    variable=input$var11
    vari=unlist( stage2[,variable])
    t.test(vari ~ Statut, data = stage2)
  })
  output$test12 <- renderPrint({
    
    stage2= filedata()
    
    variable=input$var11
    vari=unlist( stage2[,variable])
    summary(aov(vari ~ Statut, data = stage2))
  })
  
  output$logisticf <- renderPrint({
    stage2= filedataD()
    
    full_model <- glm( stage2$Statut~., family=binomial (link=logit), data =stage2 ) 
    # check your model 
    summary(full_model)
  })
  output$logisticn <- renderPrint({
    stage2= filedataD()
    
    full_model <- glm( stage2$Statut~1, family=binomial (link=logit), data =stage2 ) 
    # check your model 
    summary(full_model)
  })
  output$R2Fu <- renderPrint({
    stage2= filedataD()
    full_model <- glm( stage2$Statut~., family=binomial (link=logit), data =stage2 ) 
    null_model <- glm(stage2$Statut ~ 1, family=binomial (link=logit)) 
    R2f <- 1-logLik(full_model)/logLik(null_model) 
    R2f
  })
  output$cstatf <- renderPrint({
    stage2= filedataD()
    full_model <- glm( stage2$Statut~., family=binomial (link=logit), data =stage2 ) 
    Cstat(full_model) 
    
  })
  output$cstatb <- renderPrint({
    stage2= filedataD()
    bestModel=glm(stage2$Statut ~ Solde + DurCr + HistCr + Motif + ComEp + 
                    EmpDur + GarDeb + Age + Log, family=binomial (link=logit),data=stage2) 
    Cstat(bestModel) 
    
  })
  output$logisticb <- renderPrint({
    stage2= filedataD()
    
    bestModel=glm(stage2$Statut ~ Solde + DurCr + HistCr + Motif + ComEp + 
                    EmpDur + GarDeb + Age + Log, family=binomial (link=logit),data=stage2) 
    summary(bestModel)
  })
  output$R2Be <- renderPrint({
    stage2= filedataD()
    bestModel=glm(stage2$Statut ~ Solde + DurCr + HistCr + Motif + ComEp + 
                    EmpDur + GarDeb + Age + Log, family=binomial (link=logit),data=stage2) 
    null_model <- glm(stage2$Statut ~ 1, family=binomial (link=logit)) 
    R2b <- 1-logLik(bestModel)/logLik(null_model) 
    R2b
  })
  output$Hosmer_Lemeshow <- renderPrint({
    stage2= filedataD()
    
    bestModel=glm(stage2$Statut ~ Solde + DurCr + HistCr + Motif + ComEp + 
                    EmpDur + GarDeb + Age + Log, family=binomial (link=logit),data=stage2)
    HL <- hoslem.test(x = bestModel$y, y = fitted(bestModel), g = 10) 
    HL  
  })
  output$pHosmer_Lemeshow <- renderPlot({
    stage2= filedataD()
    
    bestModel=glm(stage2$Statut ~ Solde + DurCr + HistCr + Motif + ComEp + 
                    EmpDur + GarDeb + Age + Log, family=binomial (link=logit),data=stage2)
    HL <- hoslem.test(x = bestModel$y, y = fitted(bestModel), g = 10) 
    plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 
  })
  output$avv <- renderPrint({
    stage2= filedataD()
    
    bestModel=glm(stage2$Statut ~ Solde + DurCr + HistCr + Motif + ComEp + 
                    EmpDur + GarDeb + Age + Log, family=binomial (link=logit),data=stage2) 
    anova(bestModel, test = "Chisq") 
    
  })
  
  output$preddd <- renderPrint({
    stage2= filedataD()
    variable=input$var9
    
    bestModel=glm(stage2$Statut ~ Solde + DurCr + HistCr + Motif + ComEp + 
                    EmpDur + GarDeb + Age + Log, family=binomial (link=logit),data=stage2) 
    datax= list()
    datax$Solde=as.factor( input$Solde1)
    datax$DurCr=as.integer( input$DurCr1)
    datax$HistCr=as.factor(input$HistCr1)
    datax$Motif=as.factor(input$Motif1)
    datax$MontCr=as.integer(input$MontCr1)
    datax$ComEp=as.factor(input$ComEp1)
    datax$EmpDur=as.factor(input$EmpDur1)
    datax$StatPer=as.factor(input$StatPer1)
    datax$GarDeb=as.factor(input$GarDeb1)
    datax$ResDur=as.integer(input$ResDur1)
    datax$Age=as.integer(input$Age1)
    datax$Log=as.factor(input$Log1)
    datax$NbrCr=as.integer(input$NbrCr1)
    datax$Emp=as.factor(input$Emp1)
    datax$NbrPer=as.integer(input$NbrPer1)
    datax$Statut=1 
    fitLog=predict(bestModel,type="response",datax)
    datax$Statut=fitLog[1][1]
    datax$Statut
  })
  
  
  
  
  
  
  
  logisticsummary <- reactive({
    data= filedataD()
    attach(data)
    set.seed(12420)
    if (input$dummy)
    {data=dummy_cols(data)
    data=data[,-c(1,2,5,11,7,3,6,8,9,10,12,13,14,15,4,86,85)]
    #for (i in 3:69)
    #{data[[i]]=as.factor(data[[i]])}
    }
    in.train <- createDataPartition(Statut, p=(input$tr)/100, list=FALSE)
    german_credit.train <- data[in.train,]
    german_credit.test <- data[-in.train,]
    
    credit.glm0 <- glm(Statut ~ ., family = binomial(link = "logit"), german_credit.train)
    
    if(input$var=="AIC"){
      credit.glm.step <- step(credit.glm0, direction = "backward",trace=0)
      
      updateTextInput(session = session,'model','Enter your model',Reduce(paste, deparse(credit.glm.step$formula)))
      
    }
    else if(input$var=="BIC"){
      credit.glm.step.bic <- step(credit.glm0, k = log(nrow(german_credit.train)),trace=0)
      
      updateTextInput(session = session,'model','Enter your model',Reduce(paste, deparse(credit.glm.step.bic$formula)))
      
    }
    
    else if(input$var=="TOUT LE MODELE"){
      
      updateTextInput(session = session,'model','Enter your model',Reduce(paste, deparse(credit.glm0$formula)))
      
    }
    
    else if(input$var=="PROPRE MODELE"){
      updateTextInput(session = session,'model','Enter your model') 
    }
    
    mod=glm(formula = formula(input$model), 
            family = binomial(link = "logit"), data = german_credit.train)
    mod
  })
  
  output$var=renderUI({
    selectInput("var", "",choices=c("AIC","BIC","TOUT LE MODELE","PROPRE MODELE"))
  })
  output$methode=renderUI({
    selectInput("methode", "",choices=c("TOUT LE MODELE","PROPRE MODELE"))
  })
  output$CP=renderUI({
    class=classification() 
    c=printcp(class)
    selectInput("CP", "",choices=as.character(round(c[,1],6)) )
  })
  
  output$logisticsummary <- renderPrint({ 
    mod=logisticsummary()  
    return(summary(mod))
  })
  
  
  auctest <- reactive({
    
    predlogit = roctest()
    AUCLogtest=performance(predlogit, measure = "auc")@y.values[[1]]
    AUCLogtest
    
    
    
  })

  roctrain <- reactive({
    data= filedataD()
    attach(data)
    set.seed(12420)
    
    in.train <- createDataPartition(Statut, p=(input$tr)/100, list=FALSE)
    german_credit.train <- data[in.train,]
    german_credit.test <- data[-in.train,]
    mod=logisticsummary()
    fitLog <- predict(mod,type="response",german_credit.train)
    
    
    predlogit = prediction( fitLog, german_credit.train$Statut)
    predlogit
    
    
    
    
  })
  roctest <- reactive({
    data= filedataD()
    attach(data)
    set.seed(12420)
    
    in.train <- createDataPartition(Statut, p=(input$tr)/100, list=FALSE)
    
    german_credit.test <- data[-in.train,]
    mod=logisticsummary()
    fitLog <- predict(mod,type="response",german_credit.test)
    
    
    predlogit = prediction( fitLog, german_credit.test$Statut)
    predlogit
    
    
    
    
  })
  auctrain <- reactive({
    
    predlogit = roctrain()
    AUCLogtrain=performance(predlogit, measure = "auc")@y.values[[1]]
    AUCLogtrain
    
    
    
  })


  
  output$roc <- renderPlot({
    perf_roc_train=roctrain()
    perflogit <- performance( perf_roc_train, "fpr", "tpr")
    perf_roc_test=roctest()
    perf <- performance( perf_roc_test, "fpr", "tpr")
    plot(perflogit, col="blue", main="Courbe ROC", xlab="1-SpÃ©cificitÃ© (fpr)", ylab="SensibilitÃ© (tpr)",
         bg="white",cex.main=2,cex.lab=1,print.cutoffs.at=seq(0,1,by=0.1),lwd=3) 
    abline(0, 1,col="green",lty=3) #rajout d'une premiÃ¨re bisectrice
    lines(perf@x.values[[1]],perf@y.values[[1]],col="red",lwd=2) 
    text(1,.05,labels=paste("__ train, AUC = ",1-round(auctrain() ,digits=3),sep=""),adj=1,col = "blue")
    text(1,.15,labels=paste("__ test,  AUC = ",1-round(auctest(),digits=3),sep=""),adj=1,col = "red")
    
  })
  output$lift <- renderPlot({
    perf_roc_train=roctrain()
    perflogit <- performance(perf_roc_train, measure = "lift", x.measure = "rpp")
    perf_roc_test=roctest()
    perf <- performance(perf_roc_test, measure = "lift", x.measure = "rpp")
    plot(perflogit, col="blue", main="Courbe de Lift", xlab="RPP", ylab="Sensibilite (tpr)",
         bg="white",cex.main=1,cex.lab=1,lwd=3) 
    
    lines(perf@x.values[[1]],perf@y.values[[1]],col="red",lwd=2) 
    text(1,.25,labels="__ train",adj=1,col = "blue")
    text(1,.15,labels="__ test",adj=1,col = "red")
    
  })
  output$logisticlift <- renderPlot({
    perf_roc_train=roctrain()
    perflogit <- performance(perf_roc_train, measure = "tpr", x.measure = "rpp")
    perf_roc_test=roctest()
    perf <- performance(perf_roc_test, measure = "tpr", x.measure = "rpp")
    plot(perflogit, col="blue", main="Courbe de Lift", xlab="RPP", ylab="Sensibilite (tpr)",
         bg="white",cex.main=1,cex.lab=1,lwd=3) 
    
    lines(perf@x.values[[1]],perf@y.values[[1]],col="red",lwd=2) 
    text(1,.25,labels="__ train",adj=1,col = "blue")
    text(1,.15,labels="__ test",adj=1,col = "red")
    
  })
  
  


  

  
})                       
