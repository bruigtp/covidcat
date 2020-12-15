library(dplyr) 
library(shiny)
library(shinyWidgets)
library(openxlsx)
library(leaflet) 
library(leaflet.extras)
library(plotly)
library(htmlwidgets)
library(shinycssloaders)
library(shinyFeedback)
library(gtools)
library(zoo)

#--------------
#Logos for downloading dataset:
df <- data.frame(
  val = c(".Rda",".csv",".xlsx")
)

df$img = c(
  sprintf("<img src='Rdata.png' width=30px><div class='jhr'>%s</div></img>", df$val[1],width="100%"),
  sprintf("<img src='csv.png' width=30px><div class='jhr'>%s</div></img>", df$val[2],width="100%"),
  sprintf("<img src='xlsx.png' width=30px><div class='jhr'>%s</div></img>", df$val[3],width="100%")
)

#List of ABS:
nom_abs<-openxlsx::read.xlsx("llista_abs.xlsx",1)
nom_abs<-mixedsort(nom_abs[,2])
#--------------

#------------USER INTERFACE (UI)---------------

ui <-tagList(
  useShinyFeedback(),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
    tags$style(type = "text/css", "#mapa {height: calc(100vh - 45px) !important;
               width: 100%;
                position: fixed;
                top: 50px;
                left: 0;}
                                         .jhr{
    display: inline;
    vertical-align: middle;
    padding-left: 10px;
                                         }
                        #down{
                        margin-top:30px;
                        }

               "),
    tags$script(type="text/javascript", src = "code.js")
  ), 
  navbarPage( 
    windowTitle = "COVIDCAT",
  title=div(
  tags$span(style="font-size:30px;margin-left:10px;margin-top:-10px","COVIDCAT"),
  img(
    src = "covid.png",
    height = 50,
    width = 70,
    style = "margin-top:-15px"
  )
),
                 id="navbar",
                 selected="Mapa",
  # tabsetPanel(id="panel",
    tabPanel("Mapa",  
             leafletOutput("mapa",width="100%"),
             absolutePanel(
               top=60, left = 80, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;background-color:rgba(255,255,255,0.8);
                box-shadow: 0 0 15px rgba(0,0,0,0.2);
                border-radius: 5px;padding: 6px 8px;",
               # titlePanel("Covid-19 a Catalunya"),
               tags$b(tags$span(style="font-size:30px","Covid-19 a Catalunya")),
               br(),
               tags$b(tags$i(tags$span(style="font-size:16px","Clica en una ABS del mapa per veure'n l'evolució de casos"))),
               br(),
               br(),
               pickerInput(
                 inputId = "periode_map",
                 label = tags$span(style="font-size:16px","Selecciona el període a visualitzar"), 
                 choices = c("1a onada (<23 Juny)"=1, "2a onada (>23 Juny)"=2),
                 selected="2"
               ),
               pickerInput(
                 inputId="ia",
                 label=tags$span(style="font-size:16px","Selecciona la incidència a mostrar"),
                 choices = c("Incidència ac. total"=1, "Incidència ac. a 7 dies"=2, "Incidencia ac. a 14 dies"=3),
                 selected="1"
               ),
               selectInput("choose_ratio",label="Selecciona l'output d'interés",choices=c("Incidència ac. suavitzada per 100.000 habitants"=1,"Excés de risc (%)"=2,"Probabilitat > Global CAT"=3)),
               pickerInput(
                 inputId = "subgrup_map",
                 label = tags$span(style="font-size:16px","Per subgrup"), 
                 choices = list(
                   " "   = c("Cap subgrup seleccionat"=1),
                   Sexe = c("Dona"=2, "Home"=3)
               ),
               options=pickerOptions(dropupAuto=FALSE)),
               tags$i(textOutput("update"))
  ),
  absolutePanel(
    id="info",
    bottom=20, right = 160,
    draggable = FALSE, width = "25%", style = "z-index:500;background-color:rgba(255,255,255,0.8);
                box-shadow: 0 0 15px rgba(0,0,0,0.2);
                border-radius: 5px;
                padding: 6px 8px;",
    actionButton('coll_btn',"-", "data-toggle"='collapse', "data-target"='#coll',
                 style="opacity: .80; color: #fff; background-color:  #AF292E; border-color: #a153e5; float:right; font-size:80%;padding-top:2px;padding-bottom:2px;"),
    htmlOutput("label_ratio"),
    tags$div(id = 'coll',  class="collapse in",
    htmlOutput("info")
    )
  ),
absolutePanel(
  id="dades",
  bottom=5, left = 80,
  actionButton("down_modal","Descarrega les dades",icon=icon("download"))
)
),
  tabPanel("Gràfic",
    wellPanel(
      fluidRow(
      column(3,
      pickerInput(
        inputId = "abs",
        label = "Selecciona l'ABS", 
        choices = list(
          " "   = c("Total"=1),
          ABS=nom_abs 
        ),
        options = list(
          `live-search` = TRUE)
      )
      ),
      column(2,
      pickerInput(
        inputId = "periode_map2",
        label = "Selecciona el període a visualitzar", 
        choices = c("1a onada (<23 Juny)"=1, "2a onada (>23 Juny)"=2),
        selected="2"
      )
      ),
      column(2,
             pickerInput(
               inputId = "ia2",
               label = "Selecciona la incidència a mostrar", 
               choices = c("Incidència ac. total"=1, "Incidència ac. a 7 dies"=2,"Incidència ac. a 14 dies"=3),
               selected="1"
             )
      ),
      column(3,
      selectInput("choose_ratio2",label="Selecciona l'output d'interés",choices=c("Incidència ac. per 100.000 habitants"=1,"Excés de risc (%)"=2,"Probabilitat > Global CAT"=3)),
      tags$i(textOutput("ultims_dies")),
      uiOutput("log_scale")
      ),
      column(2,
      pickerInput(
        inputId = "subgrup_map2",
        label = "Per subgrup", 
        choices = list(
          " "   = c("Cap subgrup seleccionat"=1),
          Sexe = c("Dona"=2, "Home"=3)
        ),
        options=pickerOptions(dropupAuto=FALSE))
      )
    )
    ),
    tabsetPanel(
    id="plots",
    tabPanel(
    "Evolució",
    plotlyOutput("plot",width="100%") %>% withSpinner(),
    ),
    tabPanel(
    "Ranking",
    plotlyOutput("plot_rank",width="100%") %>% withSpinner()
    )
  )
  ),
  tabPanel("Mètodes",
    fluidRow(
    column(6,
    wellPanel(titlePanel("Informació"), 
              HTML("<p> COVIDCAT versió Beta 1.0 (27/11/2020) </p>
              <dt>Versió beta 1.0 (27/11/2020):</dt>
<dd> Hem afegit la probabilitat de IA14>250. Hem afegit també el gràfic del ranking de les ABS per cada un dels indicadors. Per últim, hem afegit un botó de descàrrega dels resultats de l'últim dia.<dd>
              <h4> Autors: </h4>
              Pau Satorra(1), Marc Marí-Dell'Olmo(2), Aurelio Tobias(3), Cristian Tebé(1).
              (Igual contribució)
              <ol>
              <li> Unitat de Bioestadística, Institut d'Investigació Biomèdica de Bellvitge (<a href='http://www.idibell.cat/en'>IDIBELL</a>) </li>
              <li> Agència de Salut Pública de Barcelona (<a href='https://www.aspb.cat/'>ASPB</a>),
              CIBER Epidemiología y Salud Pública (<a href='https://www.ciberesp.es/'>CIBERESP</a>), Institut d'Investigació Biomèdica (<a href='http://www.recercasantpau.cat/'>IIB Sant Pau</a>)</li>
              <li> Instituto de Diagnóstico Ambiental y Estudios del Agua (<a href='https://www.idaea.csic.es'>IDAEA</a>), Consejo Superior de Investigaciones Científicas (<a href='https://www.csic.es'>CSIC</a>) </li>
              </ol>
                <h4> Contacte: </h4>
                <b> E-mail:</b> ctebe@idibell.cat
                   ")
    )
    ),
    column(6,
           wellPanel(titlePanel("Referències"), 
               HTML("<ul>
                    <li>Dades obertes de Catalunya. Registre de casos de COVID-19 realitzats a Catalunya. Segregació per sexe i àrea bàsica de salut (ABS). Disponible a: 
                    <p><a href='https://analisi.transparenciacatalunya.cat/ca/Salut/Registre-de-casos-de-COVID-19-realitzats-a-Catalun/xuwf-dxjd'>https://analisi.transparenciacatalunya.cat/ca/Salut/Registre-de-casos-de-COVID-19-realitzats-a-Catalun/xuwf-dxjd</a></p>
                    </li>
                    <li> Moraga, Paula. (2019). <a href='https://www.paulamoraga.com/book-geospatial'><i> Geospatial Health Data: Modeling and Visualization with R-INLA and Shiny </i></a>. Chapman & Hall/CRC Biostatistics Series </li>
                    <li> FRANCE | COVID <a href='https://guillaumepressiat.shinyapps.io/covidfrance/'>https://guillaumepressiat.shinyapps.io/covidfrance/</a> [Accedit a 05/10/2020]</li> 
                    </ul>")
           )
           )
  ),
  fluidRow(
    column(12,
    wellPanel(titlePanel("Mètodes"), 
              br(),
        withMathJax(HTML("Utilitzem els següents indicadors epidemiològics:
                    <ul>
                    <br>
                    <li> La Incidència ac. de casos per 100.000 \\(\\text{IA}_i\\) d'una àrea és la raó del nombre de casos acumulats entre el total de la població per 100.000 habitants:
                    $$\\text{IA}_i=\\frac{Y_i}{N_i} \\times 100000$$
                    on \\(Y_i\\) són els casos observats a l'àrea i \\(N_i\\) la seva població. </li>
                    <br>
                    <li> La Incidència ac. suavitzada de casos per 100.000 d'una àrea és la modelització de la incidència de casos acumulats tenint en compte el veinatge de l'àrea. Sigui \\(\\theta_i\\) aquesta incidència suavitzada a estimar de l'àrea \\(i\\) proposem el següent model: 
                    $$Y_i \\mid \\theta_i \\sim Poisson(N_i \\times \\theta_i)$$ 
                    $$ \\log{\\theta_i} = \\beta_0 + u_i + v_i$$ 
                    on \\(\\beta_0\\) és el intercept, \\(u_i\\) l'efecte espacial estructurat, \\(u \\mid u_{i-1} \\sim N(\\hat{u}_{\\gamma_i},\\frac{1}{\\tau_u n_{{\\gamma_i}}})\\) (Conditionally autoregressive model (CAR)) i \\(v_i\\) l'efecte no estructurat, \\(v_i \\sim N(0,1/\\tau_v)\\).<br>
                    A partir d'aquest model, estimem la distribució a posteriori de la incidència amb la que obtenim la estimació puntual amb els seus intervals de credibilitat. A més a més, per a la IA14 ens permet estimar la probablitat que aquesta superi el llindar de 500. </li>
                    <br>
                    <li> Els casos esperats \\(E_i\\) d'una àrea són el nombre de casos que tindria l'àrea si es comportés com la població general per cada un dels estrats resultat de creuar els diferents grups d'edat amb el sexe. El calculem com: 
                    $$E_i=\\sum_{j=1}^m r_j n_j^{(i)}$$
                    on \\(r_j\\) és el nombre de casos observats de cada estrat \\(j\\) a Catalunya per la població d'aquest estrat i \\(n_j^{(i)}\\) és la població de l'estrat \\(j\\) en l'àrea \\(i\\). </li> 
                    <br>
                    <li> La raó de Incidència ac. estandarditzada (SIR) d'una àrea és el nombre de casos acumulats observats entre els esperats: 
                    $$\\text{SIR}_i=\\frac{Y_i}{E_i}$$ </li>
                    <br>
                    <li> La raó de Incidència ac. estandarditzada suavitzada d'una àrea és una modelització del SIR tenint en compte el veinatge de l'àrea. Sigui \\(\\theta_i\\) aquesta raó de incidència suavitzada a estimar de l'àrea \\(i\\) proposem el següent model: 
                    $$Y_i \\mid \\theta_i \\sim Poisson(E_i \\times \\theta_i)$$ 
                    $$ \\log{\\theta_i} = \\beta_0 + u_i + v_i$$ 
                    on \\(\\beta_0\\) és el intercept, \\(u_i\\) l'efecte espacial estructurat, \\(u \\mid u_{i-1} \\sim N(\\hat{u}_{\\gamma_i},\\frac{1}{\\tau_u n_{{\\gamma_i}}})\\) (Conditionally autoregressive model (CAR)) i \\(v_i\\) l'efecte no estructurat, \\(v_i \\sim N(0,1/\\tau_v)\\).<br>
                    A partir d'aquest model estimarem la distribució a posteriori de la raó de incidència suavitzada. Definirem la probabilitat que la incidència d'una àrea superi la del global de Catalunya com la probabilitat que aquesta raó de incidència suavitzada estigui per sobre de 1. 
                    </li>
                    <br>
                    <li> L'excés de risc \\(\\text{ER}_i\\) d'una àrea \\(i\\) és l'augment percentual de la raó de incidència ac. estandarditzada suavitzada respecte de 1:
                    $$\\text{ER}_i=(\\theta_i-1)*100$$ 
                    on \\(\\theta_i\\) és la raó de incidència ac. estandarditzada suavitzada.</li>
                                </ul>
                                <br>
                         Totes les anàlisis es van fer amb el paquet estadístic R version 3.6.3 (2020-02-29) per a Windows.
"))
    )   
    )
  )
  )
)
)

#----------------------------------------------

#------------------SERVER---------------------

server <- function(input, output,session){

  react<-reactiveVal(NULL)

  observe({    
    if(!is.null(input$periode_map) & !is.null(input$ia)){
      val<-switch(paste0(input$periode_map,input$ia),
             "11"=1,
             "12"=2,
             "13"=3,
             "21"=4,
             "22"=5,
             "23"=6
             )
      react(val)
    }
  })

  react2<-reactiveVal(NULL)
  
observe({    
  if(!is.null(input$periode_map2) & !is.null(input$ia2)){
      val<-switch(paste0(input$periode_map2,input$ia2),
                  "11"=1,
                  "12"=2,
                  "13"=3,
                  "21"=4,
                  "22"=5,
                  "23"=6
      )
      react2(val)
    }
  })


  load("shapefileT.Rda")

  load("sum_casos.Rda")
  load("dat_evo.Rda")
  load("tot_evo.Rda")
  output$update<-renderText({
   paste0("Actualitzat a ",Sys.Date(),". Els casos reportats daten fins a ",max(dat_evo$data))
  })

  output$label_ratio<-renderText({
    ratio<-c("Incidència acumulada suavitzada per 100.000 habitants","Excés de risc (%)","Probabilitat > Global CAT","Probabilitat IA14>500","Probabilitat IA14>250")[as.numeric(input$choose_ratio)]
    sprintf("<strong> %s <strong>",ratio)
  })
  observeEvent(input$coll_btn,{
    if(input$coll_btn%%2==0){
      updateActionButton(session,
                         "coll_btn",
                         label="-")
    }else{
      updateActionButton(session,
                         "coll_btn",
                         label="+")
    }
  })
  output$info<-renderText({
    if(!is.null(input$choose_ratio)){
      
      periode<-c("en la primera onada", "en la primera onada els últims 7 dies","en la primera onada els últims 14 dies","en la segona onada", "en la segona onada els últims 7 dies","en la segona onada els últims 14 dies")[react()]

      info<-NULL

      info[[1]]<-sprintf("Representa el número casos diagnosticats acumulats en una Àrea Bàsica de Salut (ABS) per cada 100.000 habitants %s.
La suavització de la incidència l'obtenim tenint en compte per una banda l'estructura poblacional de Catalunya per grups d'edat i sexe, i per l'altra la incidència de les ABS que li fan frontera.",periode)

      info[[2]]<-sprintf("Representa el %% de casos diagnosticats acumulats en una Àrea Bàsica de Salut (ABS) per cada 100.000 habitants %s en relació el número casos diagnosticats acumulats a Catalunya per cada 100.000 habitants %s.",periode,periode)

      info[[3]]<-sprintf("Els models Bayesians també ens permeten fer inferència. Podem estimar la probabilitat que un determinat ABS presenti una incidència acumulada per cada 100.000 habitants %s major que el conjunt de Catalunya. La probabilitat presenta un rang de valors entre 0 i 1. Com més proper a 1 més segurs estem que aquell ABS presenta una incidència acumulada per cada 100.000 habitants %s MAJOR que el conjunt de Catalunya i a la inversa.",periode,periode,periode)

      info[[4]]<-sprintf("Els models Bayesians també ens permeten fer inferència. Podem estimar la probabilitat que un determinat ABS presenti una incidència acumulada per cada 100.000 habitants %s major que un determinat valor. La probabilitat presenta un rang de valors entre 0 i 1. Com més proper a 1 més segurs estem que aquell ABS presenta una incidència acumulada per cada 100.000 habitants %s MÉS GRAN de 500 casos per cada 100.000 habitants i a la inversa.<br>
Una IA14>250 és considera una situació d'alt risc.",periode,periode,periode)

      info[[5]]<-info[[4]]
     info[[as.numeric(input$choose_ratio)]]

    }
  })

  #-----------Download------------------

  observeEvent(input$down_modal,{
    showModal(modalDialog(
      title=HTML(paste0("Descarregar els diferents indicadors epidemiològics de Catalunya per ABS a dia <br><i>",max(dat_evo$data),"*</i>")),
      fluidRow(
        column(3,
      pickerInput("type_dat",label="Format de baixada",choices=df$val,choicesOpt =list(content=df$img),width="fit")
      ),
      column(1,
      downloadButton("down","Descarregar")
      )
      ),
      footer = tagList(tags$span(style="font-size:12px;margin:33px;",tags$i("*Si es vol la sèrie històrica ficar-se en contacte amb els autors per definir els termes de la col·laboració")),
                       modalButton("Surt"))
    ))
  })

  output$down<-downloadHandler(
    filename = function(){
      paste0("covidcat",gsub("-","",max(dat_evo$data)), input$type_dat)
    },
    content = function(ff){
      shiny::withProgress(
        message = "Descargant",
        detail="(l'operació podria tardar uns segons)",
        value = 0.3,
        {
          covidcat<-sum_casos[,!grepl("_d",names(sum_casos)) & !grepl("_h",names(sum_casos))] %>%
            subset(!reactive%in%c(1,2,3)) %>%
            mutate(tax=casos*100000/total,
                   p=1-p,p2=1-p2,
                   reactive=factor(reactive,labels=c("2aonada","7dies","14dies")),
                   risc=(RR-1)*100,riscL=(LL-1)*100,riscU=(UL-1)*100) %>%
            mutate_at(vars(contains('stax')),~.x*100000) %>%
            dplyr::select(ABS,total,casos,ia=tax,esp,ia_suav=stax,ia_suav_ici=staxL,ia_suav_ics=staxU,sir=ratio,sir_suav=RR,sir_suav_ici=LL,sir_suav_ics=UL,exc_risc=risc,exc_risc_ici=riscL,exc_risc_ics=riscU,p_mes_global=p,p_mes_500=p2,reactive) %>%
            pivot_wider(names_from = reactive, values_from = c(casos,ia,esp,ia_suav,ia_suav_ici,ia_suav_ics,sir,sir_suav,sir_suav_ici,sir_suav_ics,exc_risc,exc_risc_ici,exc_risc_ics,p_mes_global,p_mes_500)) %>%
            dplyr::select(-p_mes_500_2aonada,-p_mes_500_7dies)
          incProgress(0.2)
      if (input$type_dat== ".Rda") save(covidcat,file=ff)
      if (input$type_dat== ".csv") write.csv(covidcat,ff)
      if (input$type_dat== ".xlsx") openxlsx::write.xlsx(covidcat,ff)
          incProgress(0.5)
        }
      )
    }
  )
  
  #-----------------------
  
  #----------------MAP----------------------

  res_abs2<-reactive({
    if(!is.null(input$subgrup_map) & !is.null(react())){
        dat<-sum_casos %>% subset(reactive==react()) %>%
        mutate(tax=casos*100000/total,tax_h=casos_h*100000/total_h,tax_d=casos_d*100000/total_d) %>%
          #Excés de risc:
          mutate(risc=(RR-1)*100,riscL=(LL-1)*100,riscU=(UL-1)*100,risc_h=(RR_h-1)*100,riscL_h=(LL_h-1)*100,riscU_h=(UL_h-1)*100,risc_d=(RR_d-1)*100,riscL_d=(LL_d-1)*100,riscU_d=(UL_d-1)*100) %>%
          mutate_at(vars(contains('stax')),~.x*100000) %>%
          mutate(p=1-p,p2=1-p2,p3=1-p3,p_h=1-p_h,p_d=1-p_d) %>% 
          mutate(pmap=factor(cut(p,breaks=c(0,0.8,0.9,1),right=FALSE,include.lowest = TRUE)),
                 pmap_h=factor(cut(p_h,breaks=c(0,0.8,0.9,1),right=FALSE,include.lowest = TRUE)),
                 pmap_d=factor(cut(p_d,breaks=c(0,0.8,0.9,1),right=FALSE,include.lowest = TRUE)),
                 pmap2=factor(cut(p2,breaks=c(0,0.8,0.9,1),right=FALSE,include.lowest = TRUE)),
                 pmap3=factor(cut(p3,breaks=c(0,0.8,0.9,1),right=FALSE,include.lowest = TRUE))
                 )
    }
  })

  rang<-reactive({
    if(!is.null(input$choose_ratio)){
      dat<-res_abs2()
      if(input$choose_ratio=="1"){
        min<-min(c(dat$stax,dat$stax_h,dat$stax_d),na.rm=T)
        max<-max(c(dat$stax,dat$stax_h,dat$stax_d),na.rm=T)
      }
      if(input$choose_ratio=="2"){
        min<-min(c(dat$risc,dat$risc_h,dat$risc_d),na.rm=T)
        max<-max(c(dat$risc,dat$risc_h,dat$risc_d),na.rm=T)
      }
      if(input$choose_ratio%in%c("3","4","5")){
        min<-0
        max<-1
      }

      c(min,max)
    }
  })

  rmap<-reactive({
    if(!is.null(input$subgrup_map) & !is.null(input$choose_ratio) & !is.null(react())){

      dat<-res_abs2()

      ratios<-c("stax","risc","pmap","pmap2","pmap3")
      s<-c("","_d","_h")[as.numeric(input$subgrup_map)]

      dat$mratio<-dat[,paste0(ratios[as.numeric(input$choose_ratio)],s)]

      order<-shapefileT@data$ABS
      shapefileT@data<-merge(shapefileT@data,dat,by="ABS") %>%
        arrange(OBJECTID)

      titol_per<-c("1a onada","1a onada els últims 7 dies","1a onada els últims 14 dies","2a onada","2a onada els últims 7 dies","2a onada els últims 14 dies")[react()]
      titol_sub<-c("Tota la població","Dones","Homes")[as.numeric(input$subgrup_map)]
      if(input$subgrup_map=="1" & input$choose_ratio%in%c("4","5")){
      labels <- sprintf("<strong> %s </strong> <i>%s. %s </i> <br/> Observats: %s <br/> IA: %s <br/> IA suavitzada: %s (%s, %s) <br/> SIR: %s <br/>SIR suavitzat: %s (%s, %s)<br/>Excés de risc: %s%% (%s%%, %s%%) <br/> Probabilitat ABS > CAT: %s <br/> Probabilitat IA14 > 500: %s <br/> Probabilitat IA14 > 250: %s",
                          shapefileT$ABS,titol_per,titol_sub, shapefileT@data[,paste0("casos",s)],round(shapefileT@data[,paste0("tax",s)],2), round(shapefileT@data[,paste0("stax",s)],2),  round(shapefileT@data[,paste0("staxL",s)],2), round(shapefileT@data[,paste0("staxU",s)],2), round(shapefileT@data[,paste0("ratio",s)], 2),
                          round(shapefileT@data[,paste0("RR",s)], 2), round(shapefileT@data[,paste0("LL",s)], 2), round(shapefileT@data[,paste0("UL",s)], 2),round(shapefileT@data[,paste0("risc",s)],2),round(shapefileT@data[,paste0("riscL",s)],2),round(shapefileT@data[,paste0("riscU",s)],2),round(shapefileT@data[,paste0("p",s)],2),round(shapefileT@data[,"p2"],2),round(shapefileT@data[,"p3"],2))
      }else{
      labels <- sprintf("<strong> %s </strong> <i>%s. %s </i> <br/> Observats: %s <br/> IA: %s <br/> IA suavitzada: %s (%s, %s) <br/> SIR: %s <br/>SIR suavitzat: %s (%s, %s)<br/>Excés de risc: %s%% (%s%%, %s%%) <br/> Probabilitat ABS > CAT: %s",
                        shapefileT$ABS,titol_per,titol_sub, shapefileT@data[,paste0("casos",s)],round(shapefileT@data[,paste0("tax",s)],2), round(shapefileT@data[,paste0("stax",s)],2),  round(shapefileT@data[,paste0("staxL",s)],2), round(shapefileT@data[,paste0("staxU",s)],2), round(shapefileT@data[,paste0("ratio",s)], 2),
                        round(shapefileT@data[,paste0("RR",s)], 2), round(shapefileT@data[,paste0("LL",s)], 2), round(shapefileT@data[,paste0("UL",s)], 2),round(shapefileT@data[,paste0("risc",s)],2),round(shapefileT@data[,paste0("riscL",s)],2),round(shapefileT@data[,paste0("riscU",s)],2),round(shapefileT@data[,paste0("p",s)],2))
      }
      labels<-ifelse(is.na(shapefileT@data[,paste0("casos",s)]),paste0("<strong>",shapefileT$ABS," </strong> <br/> No es reporten casos"),labels)%>%
        lapply(htmltools::HTML)

      shapefileT$labels<-labels

      shapefileT
      }
  })

  output$mapa<-renderLeaflet({
    #Map with basic characteristics:
    provider<-"OpenStreetMap.Mapnik"

    map<-leaflet(options = leafletOptions(zoomControl = FALSE))%>%
      addProviderTiles(provider=provider)%>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topleft' }).addTo(this)
    }") %>%
      addResetMapButton() %>%
      setView(1.5209,41.5912,zoom=8)

    map
  })

observe({
  if(!is.null(input$ia) & !is.null(input$subgrup_map) & !is.null(input$choose_ratio)){
    if(!(input$ia!="3" & input$choose_ratio%in%c("4","5")) & !(input$subgrup_map!="1" & input$choose_ratio%in%c("4","5"))){

     
      if(input$choose_ratio%in%c("3","4","5")){
        pal<-colorFactor(palette=c("white","orange","red"),levels=c("[0,0.8)","[0.8,0.9)","[0.9,1]")) 
        values <- levels(rmap()$mratio)
      }else{
        pal <- colorNumeric(palette = "YlOrRd", domain =rang())
        values <- seq(rang()[1],rang()[2],by=diff(rang())/(nrow(res_abs2())-1))
      }
      title<-c("IA suavitzada","Excés de risc (%)","Prob. > Global CAT","Prob. IA14>500","Prob. IA14>250")[as.numeric(input$choose_ratio)]

        leafletProxy("mapa",data=rmap()) %>%
          clearGroup("polygons") %>%
          removeControl("legend") %>%
          addPolygons(group="polygons",weight = 1, color="grey" , smoothFactor = 0.5,highlightOptions = highlightOptions(color = "white", weight = 4,bringToFront = TRUE),label=~labels,fillColor= ~pal(mratio),fillOpacity = 0.8,layerId=~ABS)  %>%
          addLegend(layerId="legend",pal = pal, values =values, opacity = 0.5, title = title, position = "bottomright")%>%
          addSearchFeatures(
            targetGroups = 'polygons',
            options = searchFeaturesOptions(
              zoom=12,position="topright",autoCollapse=TRUE,hideMarkerOnCollapse=T,textPlaceholder="Busca una ABS...")
          )
    }
    }
  })

  observeEvent(input$mapa_shape_click,{
    updateNavbarPage(session,inputId="navbar",selected="Gràfic")
  })

  observeEvent(input$mapa_shape_click,{
    updatePickerInput(
      session,
      "abs",
      selected=input$mapa_shape_click$id
    )
  })
  observeEvent(c(input$ia,input$subgrup_map),{
    if(input$ia=="3" & input$subgrup_map=="1"){
      updateSelectInput(
        session,
        "choose_ratio",
        choices=c("Incidència ac. suavitzada per 100.000 habitants"=1,"Excés de risc (%)"=2,"Probabilitat > Global CAT"=3,"Probabilitat IA14>500"=4,"Probabilitat IA14>250"=5),
        selected=input$choose_ratio
      )
    }else{
        updateSelectInput(
          session,
          "choose_ratio",
          choices=c("Incidència ac. suavitzada per 100.000 habitants"=1,"Excés de risc (%)"=2,"Probabilitat > Global CAT"=3),
          selected=ifelse(input$choose_ratio%in%c("4","5"),"1",input$choose_ratio)
        )
      }
  })
  
  #-----------------------------
  
  #----Syncronize graph ui elements with those of map----
  
  observeEvent(input$periode_map,{
    updatePickerInput(
      session,
      "periode_map2",
      selected=input$periode_map
    )
  })
  
  observeEvent(input$ia,{
    updatePickerInput(
      session,
      "ia2",
      selected=input$ia
    )
  })

    observeEvent(c(input$abs,input$choose_ratio,input$ia2),{
    if(input$abs!="1"){
      if(input$ia2=="3" & input$subgrup_map2=="1"){
        updateSelectInput(
          session,
          "choose_ratio2",
          choices=c("Incidència ac. suavitzada per 100.000 habitants"=1,"Excés de risc (%)"=2,"Probabilitat > Global CAT"=3,"Probabilitat IA14>500"=4,"Probabilitat IA14>250"=5),
          selected=input$choose_ratio
        )
      }else{
        updateSelectInput(
          session,
          "choose_ratio2",
          choices=c("Incidència ac. suavitzada per 100.000 habitants"=1,"Excés de risc (%)"=2,"Probabilitat > Global CAT"=3),
          selected=ifelse(input$choose_ratio%in%c("4","5"),"1",input$choose_ratio)
        )
      }
    }else{
      if(input$ia2=="3" & input$subgrup_map2=="1"){
        updateSelectInput(
          session,
          "choose_ratio2",
          choices=c("Incidència ac. per 100.000 habitants"=1,"Excés de risc (%)"=2,"Probabilitat > Global CAT"=3,"Probabilitat IA14>500"=4,"Probabilitat IA14>250"=5),
          selected=input$choose_ratio
        )
      }else{
        updateSelectInput(
          session,
          "choose_ratio2",
          choices=c("Incidència ac. per 100.000 habitants"=1,"Excés de risc (%)"=2,"Probabilitat > Global CAT"=3),
          selected=ifelse(input$choose_ratio%in%c("4","5"),"1",input$choose_ratio)
        )
      }
    }
  })
  observeEvent(input$choose_ratio,{
      updateSelectInput(
        session,
        "choose_ratio2",
        selected=input$choose_ratio
      )
  })
  observeEvent(input$subgrup_map,{
    updatePickerInput(
      session,
      "subgrup_map2",
      selected=input$subgrup_map
    )
  })
  
  #-------------------
  
  #--------Syncronize map ui elements with those of graph (reverse of previous)-----------------
  
  observeEvent(input$periode_map2,{
    updatePickerInput(
      session,
      "periode_map",
      selected=input$periode_map2
    )
  })
  observeEvent(input$ia2,{
    updatePickerInput(
      session,
      "ia",
      selected=input$ia2
    )
  })

  observeEvent(input$choose_ratio2,{
      updateSelectInput(
        session,
        "choose_ratio",
        selected=input$choose_ratio2
      )
  })
  observeEvent(input$subgrup_map2,{
    updatePickerInput(
      session,
      "subgrup_map",
      selected=input$subgrup_map2
    )
  })
  
  #-------------
  
  #---------PLOTS-----------------
  
  output$ultims_dies<-renderText({
    if(!is.null(input$choose_ratio2) & !is.null(input$ia2)){
      text<-NULL
      ratio<-c("de la pròpia incidència","del propi excés de risc","de la probabilitat de la probabilitat de la incidència","de la probabilitat de la incidència","de la probabilitat de la incidència")[as.numeric(input$choose_ratio2)]
      if(input$ia2=="2"){
        text<-paste("Es tracta de l'evolució",ratio,"a 7 dies en els últims 7 dies")
      }
      if(input$ia2=="3"){
        text<-paste("Es tracta de l'evolució",ratio,"a 14 dies en els últims 14 dies")
      }
      text
    }
  })


  output$log_scale<-renderUI({
    if(!is.null(input$choose_ratio2) & !is.null(input$ia2) & !is.null(input$plots)){
      if(input$choose_ratio2=="1" & !input$ia2%in%c("2","3") & input$plots=="Evolució"){
    prettySwitch(
      inputId = "log_scale",
      label = "Escala logarítmica"
    )
      }
    }
  })

  revo<-reactive({
    if(!is.null(input$periode_map2) & !is.null(react2())){
      dat_evo %>% subset(reactive==react2()) %>%
        mutate(tax=casos*100000/total,tax_h=casos_h*100000/total_h,tax_d=casos_d*100000/total_d) %>%
        #Risk excess:
        mutate(risc=(RR-1)*100,riscL=(LL-1)*100,riscU=(UL-1)*100,risc_h=(RR_h-1)*100,riscL_h=(LL_h-1)*100,riscU_h=(UL_h-1)*100,risc_d=(RR_d-1)*100,riscL_d=(LL_d-1)*100,riscU_d=(UL_d-1)*100)%>%
        mutate_at(vars(contains('stax')),~.x*100000) %>%
        mutate(p=1-p,p2=1-p2,p3=1-p3,p_h=1-p_h,p_d=1-p_d)
    }
  })

  tot_revo<-reactive({
    if(!is.null(input$periode_map2) & !is.null(react2())){
      tot_evo %>% subset(reactive==react2()) %>%
        mutate_at(vars(contains('Ttax')),~.x*100000) %>%
        as.data.frame()
    }
  })

  output$plot<-renderPlotly({
    if(!is.null(input$abs) & !is.null(input$choose_ratio2) & !is.null(input$subgrup_map2) & !is.null(input$ia2)){
      if(!(input$abs=="1" & input$choose_ratio2!="1")){

      s<-c("","_d","_h")[as.numeric(input$subgrup_map2)]

      if(input$abs!="1"){

          title<-c("IA suavitzada","Excés de risc (%)","Probabilitat ABS > CAT","Probabilitat IA14>500","Probabilitat IA14>250")[as.numeric(input$choose_ratio2)]
        if(input$ia2%in%c("2","3")){
          stitle<-c("a 7 dies","a 14 dies")[c("2","3")==input$ia2]
          title<-paste(title,stitle)
          }

        dat<-revo()

        ratios<-c("stax","risc","p","p2","p3")

        dat$mratio<-dat[,paste0(ratios[as.numeric(input$choose_ratio2)],s)]

        dat<-dat %>% subset(ABS==input$abs) %>% as.data.frame()

        if(input$choose_ratio2%in%c("1","2")){
          dat$mratioU<-dat[,paste0(ratios[as.numeric(input$choose_ratio2)],"U",s)]
          dat$mratioL<-dat[,paste0(ratios[as.numeric(input$choose_ratio2)],"L",s)]
          labels<-sprintf("%s <br> %s <br> %s: %s [%s,%s]",
                          dat$ABS,dat$data,title,round(dat$mratio,2),round(dat$mratioL,2),round(dat$mratioU,2))
        }else{
          labels<-sprintf("%s <br> %s <br> %s: %s",
                          dat$ABS,dat$data,title,round(dat$mratio,2))
        }

      }else{
          title<-c("IA")
          if(input$ia2%in%c("2","3")){
            stitle<-c("a 7 dies","a 14 dies")[c("2","3")==input$ia2]
            title<-paste(title,stitle)
          }

        dat<-tot_revo()

        dat$mratio<-dat[,paste0("Ttax",s)]
        dat$mratioU<-dat[,paste0("TtaxU",s)]
        dat$mratioL<-dat[,paste0("TtaxL",s)]

        labels<-sprintf("Total <br> %s <br> %s: %s [%s,%s]",
                       dat$data,title,round(dat$mratio,2),round(dat$mratioL,2),round(dat$mratioU,2))

      }

      if(all(is.na(dat[,paste0("casos",s)]))){
        showToast("warning","No es reporten casos en aquest període per la ABS seleccionada")
      }else{
        dat$rollmeans<-rollapply(dat$mratio,width=7,by=1,by.column=TRUE,partial=TRUE,FUN=function(x) mean(x, na.rm=TRUE),fill=NA)

        plot<-plot_ly(dat,x=~data,y=~rollmeans,type="scatter",mode="lines",hoverinfo="none",line=list(color="transparent"),showlegend=FALSE) %>%
          add_trace(dat,x=~data,y=~mratio,mode="markers",text = labels,hoverinfo = 'text',marker=list(color="rgb(31, 119, 180)"),line=NULL) %>%
          layout(yaxis=list(rangemode="tozero"))

        if(input$choose_ratio2=="1"){
          plot<-plot %>%
            add_trace(y=~mratioU,mode="lines",fillcolor="rgba(31,119,180,0.2)", line = list(color = 'transparent')) %>%
            add_trace(y=~mratioL,mode="lines",fillcolor="rgba(31,119,180,0.2)",fill = 'tonexty', line = list(color = 'transparent'))
        }
        if(input$choose_ratio2=="2"){
          plot<-plot %>%
            add_trace(y=~get(paste0("riscU",s)),mode="lines",fillcolor="rgba(31,119,180,0.2)", line = list(color = 'transparent')) %>%
            add_trace(y=~get(paste0("riscL",s)),mode="lines",fillcolor="rgba(31,119,180,0.2)",fill = 'tonexty', line = list(color = 'transparent'))
        }
        if(!is.null(input$log_scale) & input$choose_ratio=="1"){
          if(input$log_scale){
          plot<-plot %>% layout(yaxis = list(type = "log",dtick=1,title=sprintf("Log(%s)",title)))
        }else{
          plot<-plot %>% layout(yaxis=list(title=title))
        }
        }else{
          plot<-plot %>% layout(yaxis=list(title=title))
        }
        
        if(input$ia2=="3" & input$choose_ratio2=="1"){
          plot<-plot %>% 
            layout(shapes=list(
                     list(type="line",y0=500,y1=500,xref="paper",x0=0,x1=1,line=list(color="rgba(128,0,128,0.5)",dash="dash")),
                     list(type="line",y0=250,y1=250,xref="paper",x0=0,x1=1,line=list(color="rgba(255,0,0,0.5)",dash="dash")),
                     list(type="line",y0=150,y1=150,xref="paper",x0=0,x1=1,line=list(color="rgba(255,165,0,0.5)",dash="dash")),
                     list(type="line",y0=25,y1=25,xref="paper",x0=0,x1=1,line=list(color="rgba(0,128,0,0.5)",dash="dash"))
                   )
            )
        }
        
        plot %>%
          layout(xaxis=list(title="Data")
          ) %>%
          plotly::config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
            format = "png",
            width = 830,
            height = 500
          ))

      }
      }else{
        showToast("warning","No es pot mostrar el indicador seleccionat pel total de Catalunya. Siusplau, seleccioneu una ABS.")
    }
    }
  })

  #Equivalent of res_abs2 but reactive to the inputs of the plot section:
  
  res_abs2b<-reactive({
    if(!is.null(input$subgrup_map2) & !is.null(input$ia2) & !is.null(react2())){
      sum_casos %>% subset(reactive==react2()) %>%
        mutate(tax=casos*100000/total,tax_h=casos_h*100000/total_h,tax_d=casos_d*100000/total_d) %>%
        #Risk excess
        mutate(risc=(RR-1)*100,riscL=(LL-1)*100,riscU=(UL-1)*100,risc_h=(RR_h-1)*100,riscL_h=(LL_h-1)*100,riscU_h=(UL_h-1)*100,risc_d=(RR_d-1)*100,riscL_d=(LL_d-1)*100,riscU_d=(UL_d-1)*100) %>%
        mutate_at(vars(contains('stax')),~.x*100000) %>%
        mutate(p=1-p,p2=1-p2,p3=1-p3,p_h=1-p_h,p_d=1-p_d)
    }
  })

  rangb<-reactive({
    if(!is.null(input$subgrup_map2) & !is.null(input$choose_ratio2)){
      dat<-res_abs2b()
      if(input$choose_ratio2=="1"){
        min<-min(c(dat$stax,dat$stax_h,dat$stax_d),na.rm=T)
        max<-max(c(dat$stax,dat$stax_h,dat$stax_d),na.rm=T)
      }
      if(input$choose_ratio2=="2"){
        min<-min(c(dat$risc,dat$risc_h,dat$risc_d),na.rm=T)
        max<-max(c(dat$risc,dat$risc_h,dat$risc_d),na.rm=T)
      }
      if(input$choose_ratio2%in%c("3","4","5")){
        min<-0
        max<-1
      }

      c(min,max)
    }
  })

  output$plot_rank<-renderPlotly({
    if(!is.null(input$subgrup_map2) & !is.null(input$ia2) & !is.null(input$choose_ratio2) & !is.null(rangb())){

      title<-c("IA suavitzada","Excés de risc (%)","Probabilitat ABS > CAT","Probabilitat IA14>500","Probabilitat IA14>250")[as.numeric(input$choose_ratio2)]
      if(input$ia2%in%c("2","3")){
        stitle<-c("a 7 dies","a 14 dies")[c("2","3")==input$periode_map2]
        title<-paste(title,stitle)
      }

      pal <- colorNumeric(palette = "YlOrRd", domain =rangb())
      s<-c("","_d","_h")[as.numeric(input$subgrup_map2)]
      ratios<-c("stax","risc","p","p2","p3")

      dat<-res_abs2b()
      dat$mratio<-dat[,paste0(ratios[as.numeric(input$choose_ratio2)],s)]
      dat<-dat%>%
        arrange(mratio) %>%
        mutate(ABS=factor(ABS,levels=ABS))

      if(input$choose_ratio2%in%c("1","2")){
        dat$mratioL<-dat[,paste0(ratios[as.numeric(input$choose_ratio2)],"L",s)]
        dat$mratioU<-dat[,paste0(ratios[as.numeric(input$choose_ratio2)],"U",s)]
        text<-with(dat,paste0(ABS,"<br>",title,": ",round(mratio,2)," [",round(mratioL,2),",",round(mratioU,2),"]"))
      }else{
        dat$mratioL<-NA
        dat$mratioU<-NA
        text<-with(dat,paste0(ABS,"<br>",title,": ",round(mratio,2)))
      }

      dat<-dat %>%
        mutate(ci=mratioU-mratio,
               ciminus=mratio-mratioL,
               color=pal(mratio)
        )

        y<-tot_revo()$Ttax[tot_revo()$data==max(tot_revo()$data)]
        plot<-plot_ly(data=dat,y=~mratio,x=~ABS,type='scatter',
                text=~text,hoverinfo='text',
                marker = list(color = ~color),
                error_y = list(symmetric=FALSE,array=~ci,arrayminus=~ciminus,color="rgba(190,190,190,1)"),showlegend=FALSE)%>%
          layout(yaxis=list(zeroline = TRUE,
                            showgrid=FALSE,
                            title=title),
                 xaxis=list(title="ABS",
                            zeroline=TRUE,
                            showticklabels=FALSE))



        if(input$abs!="1"){
          xa<-input$abs
          ya<-dat$mratio[dat$ABS==input$abs]
          a <- list(
              x = xa,
              y = ya,
              text = xa,
              xref = "x",
              yref = "y",
              showarrow = TRUE,
              arrowhead = 7,
              arrowsize = .5,
              ax = 0,
              ay = -40
            )

            plot<-plot %>%
            layout(annotations = list(a))

        }
        if(input$choose_ratio2=="1"){
          plot<-plot %>%
            layout(
              shapes=list(type="line",y0=y,y1=y,x0=head(dat$ABS,1),x1=tail(dat$ABS,1),line=list(color="rgb(0,0,0,1)",dash="dash")),
              annotations=list(text="IA CAT",
                               xref="paper",
                               yref="y",
                               x=0.07,
                               y=y+(max(dat$mratio)-min(dat$mratio))/30,
                               showarrow=F
                               ))
        }
        plot%>%
          config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
            format = "png",
            width = 900,
            height = 500
          ))
      }
  })
  
  #-------------------------------------

}

#----------------------------------

shinyApp(ui, server)

