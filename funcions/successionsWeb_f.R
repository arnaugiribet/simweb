#Simulació.

simularSuccessionsWeb<-function(input){
  library('tidyr')
  
  #alguns paràmetres----
  tarifa<-data.frame(
    limSuperior=c(50,150,400,800,Inf)*1000,
    tipus=c(7,11,17,24,32)/100)

  coeficientsMultiplicadors<-data.frame(
    limSuperiorPatrimoniPreexistent=c(500,2000,4000,Inf)*1000,
    GP1=c(1,1.1,1.15,1.2),
    GP2=c(1,1.1,1.15,1.2),
    GP3=c(1.5882,1.5882,1.5882,1.5882),
    GP4=c(2,2,2,2)
  ) %>% pivot_longer(
    !limSuperiorPatrimoniPreexistent, 
    names_to='GP', 
    names_prefix = "GP",
    values_to='coeficient'
  )
  
  taulaBonificacioGP1<-data.frame(
    `LimSuperiorBI`=c(100,200,300,500,750,1000,1500,2000,2500,3000,Inf)*1000,
    `Tipus`=c(99,97,95,90,80,70,60,50,40,25,20)/100,
    check.names = F)
  
  taulaBonificacioGP2<-data.frame(
    `LimSuperiorBI`=c(100,200,300,500,750,1000,1500,2000,2500,3000,Inf)*1000,
    `Tipus`=c(60,55,50,45,40,35,30,25,20,10,0)/100,
    check.names = F)
  
  #càlculs previs a la simulació----
  grupParentiu<-
    fifelse(
      (input$relacioParentiu %in% c('A','B') & input$edat < 21),
      '1',
      fifelse(
        (input$relacioParentiu %in% c('C','D')) | (input$relacioParentiu %in% c('A','B') & input$edat >= 21),
        '2',
        fifelse(
          (input$relacioParentiu=='E'),
          '3',
          fifelse(input$relacioParentiu=='F',
                  '4',
                  NA_character_)
          )
        )
      )
  
  coefMultiplicador<-
    coeficientsMultiplicadors %>% 
    filter(limSuperiorPatrimoniPreexistent>=input$valorPatrimoniPreexistent) %>% 
    filter(GP==grupParentiu) %>% 
    arrange(limSuperiorPatrimoniPreexistent)  %>% 
    first %>% 
    pull(coeficient)
  
  #base imposable / valor de Participacio de l'Adquirent----
  baseImposable<-(input$valorBens*input$pctParticipacio)
  
  #reduccions----
  
  #parentiu
  reduccioParentiu<-
    fifelse(
      grupParentiu=='1', #tots descendents<21
      pmin(100000+(21-input$edat)*12000,
           196000),
      fifelse(
        grupParentiu=='2',
        fifelse(input$relacioParentiu %in% c('A','C'), #fill>=21 o cònjuge/parella estable
                100000,
                fifelse(input$relacioParentiu == 'B', #resta descendents>=21
                        50000,
                        30000)),
        fifelse(grupParentiu=='3',
                8000,
                0)
      )
    )
  
  #discapacitat
  reduccioDiscapacitat<-
    fifelse(
      input$grauDiscapacitat=='0',
      0,
      fifelse(input$grauDiscapacitat=='33',
              275000,
              fifelse(input$grauDiscapacitat=='65',
                      650000,
                      NA_integer_)
              )
    )
  
  #gent gran
  reduccio75Anys<-
    fifelse(
      reduccioDiscapacitat>0,
      0,
      fifelse(
        input$edat>=75 & grupParentiu=='2',
        275000,
        0)
    )
  
  #habitatge habitual
  
  ##es calcula el límits prorratejat màxim
  limSuperiorTotal<-500000 #quan és l'habitatge sencer que tenia el difunt i es transmet per herència
  limSuperiorIndividual<-pmax(180000,limSuperiorTotal*input$pctParticipacio)
  
  ##calcula la deducció (no limitada) prorratejada
  reduccioTeoricaHH<-input$valorHabitatgeHabitual*0.95
  reduccioProrratejadaTeoricaHH<-reduccioTeoricaHH*input$pctParticipacio
  
  ##finalment es limita
  reduccioHabitatgeHabitual<-fifelse(reduccioProrratejadaTeoricaHH>limSuperiorIndividual,
                                     limSuperiorIndividual,
                                     reduccioProrratejadaTeoricaHH)
  
  reduccioHabitatgeHabitual<-fifelse(grupParentiu %in% c('3','4') & input$edat<=65,
                                     0,
                                     reduccioHabitatgeHabitual)

  #assegurances
  reduccioAssegurancesVida<-fifelse(grupParentiu %in% c('1','2'),
                              pmin(25000,input$valorAssegurancesVida*input$pctParticipacio),
                              0)
  
  #activitats economiques
  reduccioActivitatsEconomiques<-fifelse(grupParentiu %in% c('1','2','3'),
                                    pmin(input$valorActivitatsEconomiques)*0.95*input$pctParticipacio,
                                    0)
  
  reduccions<-reduccioParentiu+reduccioDiscapacitat+reduccio75Anys+reduccioHabitatgeHabitual+reduccioAssegurancesVida+reduccioActivitatsEconomiques

  #base liquidable----
  baseLiquidable<-pmax(0,baseImposable-reduccions)
  
  #quota integra i tributaria----
  quotaIntegra<-calcula_qi(baseLiquidable, limSup=tarifa$limSuperior, valor=tarifa$tipus)
  quotaTributaria<-quotaIntegra*coefMultiplicador
  
  #bonificacions----
  pctBonificacio<-
    fifelse(
      input$relacioParentiu == 'C',
      0.99,
      fifelse(
        grupParentiu=='1',
        calcula_qi(baseImposable,
                   limSup = taulaBonificacioGP1$LimSuperiorBI,
                   valor = taulaBonificacioGP1$Tipus)/baseImposable,
        fifelse(grupParentiu=='2',
                calcula_qi(baseImposable,
                           limSup = taulaBonificacioGP2$LimSuperiorBI,
                           valor = taulaBonificacioGP2$Tipus)/baseImposable,
                0)
      )
    
  )
  
  bonificacio<-quotaTributaria*pctBonificacio
  
  #quota a ingressar----
  quotaIngressar<-pmax(quotaTributaria-bonificacio,0)
  
  #Valor de participació net de l'adquirent
  valorParticipacioNet<-baseImposable-quotaIngressar
  
  #tipus mig
  tipus<-quotaIngressar/baseImposable
  
  #resultats----
  resultats<-data.frame(Concepte=c("Valor de participació de l'adquirent (Base Imposable)",
                                   'Reducció per Parentiu',
                                   'Reducció per Discapacitat',
                                   'Reducció per a Persones de 75 anys o més',
                                   "Reducció per Adquisició d'Habitatge Habitual",
                                   'Reducció per Activitats Econòmiques',
                                   "Reducció per Assegurances",
                                   "Reduccions Totals",
                                   'Base Liquidable',
                                   'Quota Íntegra',
                                   'Quota Tributària',
                                   'Bonificació',
                                   'Quota a Ingressar',
                                   'Tipus Efectiu (Quota a Ingressar / Base Imposable)',
                                   "Valor de participació de l'adquirent després de l'Impost"),
                        Valor=c(baseImposable,
                                reduccioParentiu,
                                reduccioDiscapacitat,
                                reduccio75Anys,
                                reduccioHabitatgeHabitual,
                                reduccioActivitatsEconomiques,
                                reduccioAssegurancesVida,
                                reduccions,
                                baseLiquidable,
                                quotaIntegra,
                                quotaTributaria,
                                bonificacio,
                                quotaIngressar,
                                tipus,
                                valorParticipacioNet))

  return(resultats)
}
