
successionsV2ModuleUI <- function(id) {
  ns <- NS(id)

  relacionsParentiu<-list('Fill/a'='A',
                          'Altre descendent: net, besnet, rebesnet o quadrinet'='B',
                          'Cònjuge o parella estable'='C',
                          'Progenitor o altre ascendent'='D',
                          'Col·lateral de 2n o 3r grau, o ascendent/descendent per afinitat'='E',
                          'Col·lateral de 4t grau o més'='F')
  grausDiscapacitat<-list('No'='0',
                          'Igual o superior al 33%'='33',
                          'Igual o superior al 65%'='65')
  #Disseny
  tagList(
    card_header('Simulador de Successions - versió 2') %>% h2,
    card_body(
      tabsetPanel(
        id = ns("tabs1"),
        type = c("tabs"),

        nav_panel(title = "Simulador",
                  br(),
                  column(2),
                  column(8,
                         #Valor Herència
                         box(
                           title = "Valor de l'Herència",
                           status = 'primary',
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           width = 12,
                           collapsed = F,
                           
                           shinyjs::hidden(div(
                             id = ns("hiddenParticipacio"),
                             numericInput(ns('pctParticipacio'), label='',value=1), #fix i amagat
                           )),
                           column(12,
                           strong("Introdueix el valor dels béns rebuts de l'herència."),
                           currencyInput(
                             ns("valorBens"),
                             "",
                             value = 0,
                             format = 'euro',
                             align = 'left',
                             width = '200px'
                           )),
                           column(7,
                                  strong("Dins d'aquests béns hi ha l'habitatge habitual?"),
                                  selectInput(
                                    ns("habitatgeHabitual"),
                                    "",
                                    choices = c('No','Sí'),
                                    selected = 'No',
                                    width = '200px'
                                  )),
                           column(5,
                                  conditionalPanel(
                                    condition = paste0("input['",ns("habitatgeHabitual"),"'] == 'Sí' "),
                                    strong("Quin valor té?"),
                                    currencyInput(
                                      ns("valorHabitatgeHabitual"),
                                      "",
                                      value = 0,
                                      format = 'euro',
                                      align = 'left',
                                      width = '200px'
                                    ))),
                           column(7,
                                  strong("Dins d'aquests béns hi ha béns i drets afectes a activitats econòmiques?"),
                                  selectInput(
                                    ns("activitatsEconomiques"),
                                    "",
                                    choices = c('No','Sí'),
                                    selected = 'No',
                                    width = '200px'
                                  )),
                           column(5,
                                  conditionalPanel(
                                    condition = paste0("input['",ns("activitatsEconomiques"),"'] == 'Sí' "),
                                    strong("Quin valor tenen?"),
                                    currencyInput(
                                      ns("valorActivitatsEconomiques"),
                                      "",
                                      value = 0,
                                      format = 'euro',
                                      align = 'left',
                                      width = '200px'
                                    ))),
                           column(7,
                                  strong("Tenia el difunt alguna assegurança de vida contractada?"),
                                  selectInput(
                                    ns("assegurancesVida"),
                                    "",
                                    choices = c('No','Sí'),
                                    selected = 'No',
                                    width = '200px'
                                  )),
                           column(5,
                                  conditionalPanel(
                                    condition = paste0("input['",ns("assegurancesVida"),"'] == 'Sí' "),
                                    strong("Quina quantitat tenia assegurada?"),
                                    currencyInput(
                                      ns("valorAssegurancesVida"),
                                      "",
                                      value = 0,
                                      format = 'euro',
                                      align = 'left',
                                      width = '200px'
                                    ))),
                           column(7),
                           column(5,
                                  conditionalPanel(
                                    condition = paste0("input['",ns("valorAssegurancesVida"),"']",
                                                       " + input['",ns("valorActivitatsEconomiques"),"']",
                                                       " + input['",ns("valorHabitatgeHabitual"),"']",
                                                       " > input['",ns("valorBens"),"'] "),
                                    div("La suma dels valors no pot ser superior als béns totals", style = "color: #FF0000"))),
                           column(12,
                                  strong("Valor de participació de l'adquirent (Base Imposable)"),
                                  currencyInput(ns("valorParticipacioAdquirent"),
                                                "",
                                                value=NULL,
                                                format = 'euro',
                                                align = 'left',
                                                width = '200px') %>% disabled())
                         ),
                         
                         #Dades Personals
                         box(
                           title = "Dades Personals",
                           status = 'primary',
                           collapsible = TRUE,
                           solidHeader = TRUE,
                           width = 12,
                           collapsed = F,

                           column(12,
                                  strong("Quin és el parentiu amb el difunt?"),
                                  selectInput(
                                    ns("relacioParentiu"),
                                    "",
                                    choices = relacionsParentiu,
                                    selected = NULL,
                                    width = '200px'
                                  )),
                           column(12,
                           strong("Indica la teva edat."),
                           autonumericInput(
                             ns("edat"),
                             "",
                             value = NULL,
                             width = '200px',
                             decimalPlaces=0,
                             align='left'
                           ),
                           strong("Quin és el valor del teu patrimoni preexistent?"),
                           currencyInput(
                             ns("valorPatrimoniPreexistent"),
                             "",
                             value = 0,
                             format = 'euro',
                             align = 'left',
                             width = '200px'
                           ),
                           strong("Tens reconegut algun grau de discapacitat?"),
                           selectInput(
                             ns("grauDiscapacitat"),
                             "",
                             choices = grausDiscapacitat,
                             selected = '0',
                             width = '200px'
                           ))
                         ),
                         
                         #Botó calcula
                         column(12,
                                actionButton(ns("calcula"), "Calcula", icon("paper-plane"), 
                                           style="color: #fff;
                                           background-color: #BF0000;
                                           border-color: #BF0000;
                                           font-size: 18px;
                                           font-family: 'Source Sans Pro', sans-serif", 
                                           block=F,width='100px')),
                         column(12,br()), #espai en blanc

                         #Resultats
                         
                         shinyjs::hidden(div(
                           id = ns("hiddenbox1"),
                           box(
                             title = "Quota a ingressar",
                             status = "primary",
                             width=12,
                             solidHeader = TRUE,
                             collapsible = F,
                             
                             column(12,
                                    fluidRow(
                                      column(3,
                                             br(), strong('Quota Tributària')),
                                      column(4,
                                             div(style = "margin-top:-5px"),
                                             currencyInput(ns("resultatsQuotaTributaria"),
                                                           "",
                                                           value=NULL,
                                                           format = 'euro',
                                                           align = 'left',
                                                           width = '200px') %>% disabled()
                                             )
                                      )
                                    ),
                             column(12,
                                    fluidRow(
                                      column(3,
                                             br(),
                                             strong('Bonificació')),
                                      column(4,
                                             div(style = "margin-top:-5px"),
                                             currencyInput(
                                               ns("resultatsBonificacio"),
                                               "",
                                               value = NULL,
                                               format = 'euro',
                                               align = 'left',
                                               width = '200px') %>% disabled()
                                             )
                                      )
                                    ), 
                             column(12,
                                    fluidRow(
                                      column(3,
                                             br(), strong('Quota a Ingressar')),
                                      column(4,
                                             div(style = "margin-top:-5px"),
                                             currencyInput(ns("resultatsQuotaAIngressar"),
                                                           "",
                                                           value=NULL,
                                                           format = 'euro',
                                                           align = 'left',
                                                           width = '200px') %>% disabled()
                                      )
                                    )
                             ),
                             column(12,
                                    fluidRow(
                                      column(3,
                                             br(),
                                             strong('Tipus Efectiu (Quota a Ingressar / Base Imposable)')),
                                      column(4,
                                             div(style = "margin-top:0px"),
                                             currencyInput(
                                               ns("resultatsTipusEfectiu"),
                                               "",
                                               value = NULL,
                                               format = 'percentageEU2dec',
                                               align = 'left',
                                               width = '200px') %>% disabled()
                                      )
                                    )
                             ), 
                             column(12,
                                    fluidRow(
                                      column(3,
                                             br(),
                                             strong("Valor de Participació de l'Adquirent després de l'Impost")),
                                      column(4,
                                             div(style = "margin-top:0px"),
                                             currencyInput(
                                               ns("resultatsValorDespresImpost"),
                                               "",
                                               value = NULL,
                                               format = 'euro',
                                               align = 'left',
                                               width = '200px') %>% disabled()
                                      )
                                    )
                             ), 
                             
                             
                             # DTOutput("taulaResumPetita") %>% withSpinner(),
                             
                           )
                         ),
                         br(),
                         shinyjs::hidden(div(
                           id = ns("hiddenbox2"),
                           box(
                             title = "Liquidació completa de l'impost",
                             status = "primary",
                             width=12,
                             solidHeader = TRUE,
                             collapsible = T,
                             collapsed=T,
                            
                             DTOutput(ns("taulaResumGran")) %>% withSpinner()
                           ))
                         )
                         
                         )
                         )
                  )
        )
      )
    )
  }

successionsV2ModuleServer <- function(id) {
  moduleServer(
    id,
    function(input,output,session){
      
      ##### CARREGA FUNCIONS ===============
      source('funcions/successionsWeb_f.R')
      
      #actualitza el valor de participació de l'adquirent
      valorParticipacioAdquirent<-reactive({
        return(input$valorBens*input$pctParticipacio)
      })
      
      observeEvent(valorParticipacioAdquirent(),{
        updateCurrencyInput(inputId="valorParticipacioAdquirent",value=valorParticipacioAdquirent())
      })
      
      #mostra caixes de resultats en calcular
      observeEvent(input$calcula, {
        req(iv$is_valid())
        shinyjs::show(id = "hiddenbox1")
        shinyjs::show(id = "hiddenbox2")
      })
      
      #calcula
      resultats<-eventReactive({
        input$calcula
      },{
        req(iv$is_valid())
        simularSuccessionsWeb(input)
      })
      
      #actualitza caselles de resultats en haver calculat
      observeEvent(resultats(),{
        updateCurrencyInput(inputId="resultatsQuotaTributaria",value=resultats() %>% 
                              filter(Concepte=='Quota Tributària') %>% pull(Valor))
        updateCurrencyInput(inputId="resultatsBonificacio",value=resultats() %>% 
                              filter(Concepte=='Bonificació') %>% pull(Valor))
        updateCurrencyInput(inputId="resultatsQuotaAIngressar",value=resultats() %>% 
                              filter(Concepte=='Quota a Ingressar') %>% pull(Valor))
        updateCurrencyInput(inputId="resultatsTipusEfectiu",value=resultats() %>% 
                              filter(Concepte=='Tipus Efectiu (Quota a Ingressar / Base Imposable)') %>% pull(Valor))
        updateCurrencyInput(inputId="resultatsValorDespresImpost",value=resultats() %>% 
                              filter(Concepte=="Valor de participació de l'adquirent després de l'Impost") %>% pull(Valor))
      })
      
      output$taulaResumPetita <- renderDataTable({
        req(resultats())
        taula<-resultats() %>% 
          filter(Concepte %in% c('Quota Tributària',
                                 'Bonificació',
                                 'Quota a Ingressar',
                                 'Tipus Efectiu (Quota a Ingressar / Base Imposable)',
                                 "Valor de participació de l'adquirent després de l'Impost"))
        datatable(taula,
                  rownames=F,
                  selection='none',
                  options=list(dom='t', pageLength = -1, ordering=F,scrollX=T,language=catalan)) %>% 
          formatejaTaulaDT(columnesCurrency=2)
      }
      )
      
      
      output$taulaResumGran <- renderDataTable({
        req(resultats())
        taula<-resultats() %>% filter(Concepte != 'Tipus Efectiu (Quota a Ingressar / Base Imposable)')
        datatable(taula,
                  rownames=F,
                  selection='none',
                  options=list(dom='t', pageLength = -1, ordering=F,scrollX=T,language=catalan)) %>% 
          formatejaTaulaDT(columnesCurrency=2)
      }
      )
      
      
      
      #deixar valors addicionals a 0 quan se selecciona NO
      
      observeEvent(input$habitatgeHabitual, {
        if(input$habitatgeHabitual == 'No'){
          updateCurrencyInput(inputId = "valorHabitatgeHabitual", value = 0)
        }
      })
      observeEvent(input$activitatsEconomiques, {
        if(input$activitatsEconomiques == 'No'){
          updateCurrencyInput(inputId = "valorActivitatsEconomiques", value = 0)
        }
      })
      observeEvent(input$assegurancesVida, {
        if(input$assegurancesVida == 'No'){
          updateCurrencyInput(inputId = "valorAssegurancesVida", value = 0)
        }
      })
      
      ##### NORMES: valors que han de ser positius, percentatges entre 0 i 100, etc. ===============
      
      #funció per a comprovar que la suma d'algunes caselles no superen el valor d'una altra casella
      lte_reactive <- function(casellesSumar=list(), valorMaxim, missatge){
        #es fa així perquè les normes han de ser reactives
        sumaTotal<-sum(unlist(casellesSumar))
        
        if(sumaTotal > valorMaxim){
          paste0(missatge)
        }
      }
      
      iv <- InputValidator$new()
      
      iv$add_rule("valorHabitatgeHabitual", sv_gte(0,"No pot ser un valor negatiu."))
      iv$add_rule("valorHabitatgeHabitual", function(x) {lte_reactive(casellesSumar=list(input$valorHabitatgeHabitual,
                                                                                         input$valorActivitatsEconomiques,
                                                                                         input$valorAssegurancesVida),
                                                                      valorMaxim=input$valorBens,
                                                                      missatge="")})
      iv$add_rule("valorActivitatsEconomiques", sv_gte(0,"No pot ser un valor negatiu."))
      iv$add_rule("valorActivitatsEconomiques", function(x) {lte_reactive(casellesSumar=list(input$valorHabitatgeHabitual,
                                                                                             input$valorActivitatsEconomiques,
                                                                                             input$valorAssegurancesVida),
                                                                          valorMaxim=input$valorBens,
                                                                          missatge="")})
      iv$add_rule("valorAssegurancesVida", sv_gte(0,"No pot ser un valor negatiu."))
      iv$add_rule("valorAssegurancesVida", function(x) {lte_reactive(casellesSumar=list(input$valorHabitatgeHabitual,
                                                                                        input$valorActivitatsEconomiques,
                                                                                        input$valorAssegurancesVida),
                                                                     valorMaxim=input$valorBens,
                                                                     missatge="")})
      
      iv$add_rule("edat", sv_required(message = "Cal introduir un número enter.")) #quan no hi ha valor
      iv$add_rule("edat", sv_gte(0, "Cal introduir un número enter."))
      
      iv$add_rule("valorBens", sv_required(message = "Cal introduir un valor.")) #quan no hi ha valor
      iv$add_rule("valorBens", sv_gt(0, "Cal introduir un valor positiu."))
      
      iv$enable()
      
    }
    
  )
}
  
  