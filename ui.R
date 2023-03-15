library(shiny)

shinyUI(
  dashboardPage(
    dashboardHeader(title=HTML("Les inégalités dans le milieu scolaire"),# Titre de l'application
                    disable= FALSE,
                    titleWidth = 380,
                    tags$li(class="dropdown",tags$a(href="https://twitter.com/education_gouv", icon("twitter"), "Twitter", style="color:white")),
                    tags$li(class="dropdown",tags$a(href="https://www.education.gouv.fr/",  "Site Officiel", style="color:white")),
                    tags$li(class="dropdown",tags$a(href="https://www.instagram.com/education_gouv/?hl=fr",icon("instagram") , "Instagram", style="color:white")),
                    dropdownMenu(type="message", messageItem(from="Notification",message="Bienvenue sur notre application WEB!",icon=icon("envelope-open")))
    ), 
    dashboardSidebar( # Menu de l'application
      width = 220,
      sidebarMenu(
        menuItem("Accueil", tabName = "Accueil", icon = icon("home")),
        menuItem(HTML("Inégalités \n socio-économiques"), tabName = "social", icon = icon("money-bills")),
        menuItem("Inégalités territoriales",tabName = "geo", icon = icon("flag")),
        menuItem("Inégalités de genre", tabName = "genre",icon=icon("venus-mars")),
        menuItem("Bases de données",tabName = "BDD",icon = icon("database"))
      )),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "Accueil",
                tabsetPanel(
                  tabPanel("Informations générales",
                           fluidRow(style = 'margin:6px;'),
                           fluidRow(
                             box(title = "L'éducation nationale", status = "primary", solidHeader = TRUE,
                                 p("L'Éducation nationale en France est le ministère chargé de l'organisation et de la gestion du système éducatif national. 
                                        Il est responsable de la politique éducative, de la mise en œuvre des programmes scolaires, de la formation des enseignants et de la gestion des établissements scolaires publics.
                                        Le ministère de l'Éducation nationale est dirigé par un ministre nommé par le Président de la République, sur proposition du Premier ministre. 
                                        Le ministre est assisté d'un secrétaire d'État chargé de l'Enseignement supérieur et de la Recherche.
                                        L'Éducation nationale en France est chargée de garantir l'égalité des chances pour tous les élèves et de promouvoir la réussite scolaire."
                                 )),
                             valueBoxOutput("educ_nationale")),
                           fluidRow(
                             box(title = "Les inégalités dans le milieu scolaire",status = "warning",solidHeader = TRUE,collapsible = TRUE,
                                 HTML("Ces inégalités peuvent prendre différentes formes : 
                                    <ul><li>Inégalités socio-économiques : Les élèves issus de milieux défavorisés ont souvent moins accès aux ressources et aux opportunités éducatives que les élèves issus de milieux plus aisés.</li>
                                    <li>Inégalités territoriales : Les établissements scolaires ne sont pas tous égaux en termes de qualité des infrastructures, de la taille des classes, des programmes éducatifs proposés, de la qualité des enseignants.</li>
                                    <li>Inégalités de genre : Les filles et les garçons ne bénéficient pas toujours des mêmes opportunités éducatives. </li>
                                    <li>Inégalités liées aux origines culturelles et linguistiques : Les élèves issus de l'immigration peuvent rencontrer des difficultés linguistiques et culturelles, qui peuvent nuire à leur réussite scolaire.</li></ul>
                                    ")
                             ),
                             box(title = "Qu'est ce que l'OCDE?", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                 p("L'OCDE (Organisation de Coopération et de Développement Économiques) est une organisation internationale qui rassemble 38 pays membres, dont la plupart sont des pays développés.
                                      Les pays membres de l'OCDE représentent ensemble plus de 60% de l'économie mondiale et ils collaborent sur de nombreux sujets économiques, sociaux et environnementaux pour favoriser la croissance et le développement durable.
                                      ")
                             )
                           )
                  ),
                  tabPanel("Les grandes lois sur l'école",
                           fluidRow(style='margin:6px;'),
                           fluidRow(
                             valueBoxOutput("loi_Falloux"),
                             valueBoxOutput("progres_filles"),
                             valueBoxOutput("gratuite")
                           ),
                           fluidRow(
                             valueBoxOutput("sep_eglise_etat"),
                             valueBoxOutput("filles_bac"),
                             valueBoxOutput("prog_identique_F_G")
                           ),
                           fluidRow(
                             valueBoxOutput("ecole_obl_16"),
                             valueBoxOutput("mixite"),
                             valueBoxOutput("creation_bac_pro")
                           )
                  ),
                  tabPanel("Personnages importants (dans les pays de l'OCDE)",
                           fluidRow(style='margin:6px;'),
                           fluidRow(
                             box(title="En France",h1("Jules FERRY"),imageOutput("Jules_FERRY"),status = "primary"),
                             box(title="En Allemagne",h1("Martin LUTHER"),imageOutput("Martin_LUTHER"),status="success")
                           ),
                           fluidRow(
                             box(title ="En Espagne",h1("Francisco Giner de los Ríos"),imageOutput("Francisco_Giner_de_los_Ríos"),status="warning"),
                             box(title= "En Turquie", h1("Mustafa Kemal Atatürk"),imageOutput("Mustafa_Kemal_Atatürk"),status = "info")
                           ),
                           fluidRow(
                             box(title = "Au Japon", h1("Deux principaux acteurs :"), h2("Gouvernement Japonais"),h2("Mori Arinori"),imageOutput("Mori_Arinori"),status = "danger"),
                             box(title = "Au Canada",p("L'éducation au Canada est gérée au niveau provincial et territorial plutôt qu'au niveau fédéral. Cela signifie que chaque province et territoire a mis en place son propre système d'éducation, et que l'histoire et l'évolution de l'éducation nationale varient d'une région à l'autre."))
                           )
                  )    
                )),
        tabItem(tabName = "social",
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput("nom_departement",label="Choisissez un département :",
                                   choices=list("AIN","AISNE","ALLIER","ALPES-DE-HTE-PROVENCE","ALPES-MARITIMES","ARDECHE","ARDENNES","ARIEGE","AUBE","AUDE","AVEYRON","BAS-RHIN","BOUCHES-DU-RHONE","CALVADOS","CANTAL","CHARENTE","CHARENTE-MARITIME",
                                                "CHER","CORREZE","CORSE-DU-SUD", "COTE D'OR","COTES D'ARMOR","CREUSE","DEUX-SEVRES","DORDOGNE","DOUBS","DROME","Ensemble de l'acadÃ©mie","ESSONNE",
                                                "EURE","EURE-ET-LOIR","FINISTERE","GARD","GERS" ,"GIRONDE","GUADELOUPE","GUYANE","HAUT-RHIN","HAUTE-CORSE" ,"HAUTE-GARONNE","HAUTE-LOIRE","HAUTE-MARNE","HAUTE-SAONE","HAUTE-VIENNE","HAUTE SAVOIE","HAUTES-ALPES","HAUTES-PYRENEES","HAUTS-DE-SEINE","HERAULT","ILLE-ET-VILAINE" ,"INDRE","INDRE-ET-LOIRE","ISERE","JURA","LA REUNION","LANDES","LOIR-ET-CHER","LOIRE","LOIRE-ATLANTIQUE","LOIRET","LOT","LOT-ET-GARONNE","LOZERE","MAINE-ET-LOIRE","MANCHE","MARNE","MARTINIQUE", "MAYENNE","MAYOTTE","MEURTHE-ET-MOSELLE","MEUSE","MORBIHAN","MOSELLE","NIEVRE","NORD","OISE","ORNE","PARIS","PAS-DE-CALAIS","PUY-DE-DOME","PYRENEES-ATLANTIQUES","PYRENEES-ORIENTALES","RHONE","SAONE-ET-LOIRE","SARTHE","SAVOIE","SEINE-ET-MARNE","SEINE-SAINT-DENIS","SEINE MARITIME","SOMME","TARN","TARN-ET-GARONNE","TERRITOIRE DE BELFORT","VAL-D'OISE","VAL-DE-MARNE","VAR","VAUCLUSE","VENDEE","VIENNE","VOSGES","YONNE","YVELINES")),
                    radioButtons(inputId="annee",label = "Choisissez une année :",choices=c(2014,2015,2016,2017,2018,2019,2020,2021)),
                    selectizeInput("origine_sociale",label="Origine Sociale",
                                   choices = list("Agriculteurs exploitants","Artisans, commerçants, chefs d'entreprise","Autres personnes sans activité professionnelle",
                                                  "Cadres, professions intellectuelles supérieures","Cadres, professions intellectuelles supérieures : professeurs et assimilés",
                                                  "dont instituteurs et assimilés","dont professeurs et assimilés","Employés","Ensemble","Indéterminé","Ouvriers",
                                                  "Professions intermédiaires","Professions intermédiaires : instituteurs et assimilés","Retraités")),
                    width = 3
                  ),
                  mainPanel(
                  tabsetPanel(
                    tabPanel("Origines sociales",
                             style='margin:6px;',
                             h1("L'impact de l'origine sociale sur la scolarité"),
                             
                             h2(paste0("Des chiffres clés")),
                             fluidRow(
                               valueBoxOutput("bac_origine_sociale") ,
                               valueBoxOutput("bac_sans_emploi") ,
                               valueBoxOutput("bac_cadre")
                             ) ,
                             fluidRow(style="margin:6px;",
                             withSpinner(
                              plotOutput("treemap_college"),
                              type = 1)),
                             fluidRow(style="margin:6px;",
                                      withSpinner(
                                        plotOutput("camembert_lycee"),
                                        type = 1)),
                             # Modifier la mise en page de ce graphique : 
                             fluidRow(style="margin:6px;",
                               withSpinner(
                                 plotOutput("reussite_bac_PCS"),
                                 type = 1))
                             # J ARRIVE PAS A L AFFICHER : je vais voir ça plus tard
                             # fluidRow(
                             #   p("Ce graphique nous permet de distinguer la répartition des PCS selon le secteur d'enseignement."),
                             #   p("Nous pouvons directement nous rendre compte des disparités sociales entre les collèges puisque la classe sociale majoritaire dans les collèges publics est défavorisée alors que dans ceux privés, elle correspond à une classe aisée.")
                             # )

                             
                          ),
                    tabPanel("Privé ou public ?",
                             style='margin:6px;',
                             h1("L'impact du privé et du public sur la scolarité"),
                             
                             h2("Taux de réussite au Diplôme National du Brevet"),
                             fluidRow(style="margin:6px;",
                             withSpinner(
                               dataTableOutput("reussite_secteur"),
                               type = 1)),
                             
                             h2("Professions et Catégories Sociales selon le secteur d'enseignement"),
                             fluidRow(style="margin:6px;",
                             withSpinner(
                               amChartsOutput("amchartComparaisonPCS"),
                               type = 1))
                    )
                  )
                  )
                                  
        )),
        
        ### Inégalités territoriales
        tabItem(tabName = "geo",
                fluidPage(
                  titlePanel("Les inégalités territoriales"),
                  tabsetPanel(
                    tabPanel("Condition d'apprentissage", # evolution du nombre d'enseignant par élève et le taux reussite
                             fluidRow(
                               selectizeInput("Pays_mobilite",label="Pays",
                                              choices = list("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                                             "France","G20","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                                             "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA"))
                             ),
                             fluidRow(
                               box(title="Evolution nombre d'enseignant par élèves",
                                   withSpinner(
                                     plotOutput("evol_enseignant_eleves"),
                                     type = 1)
                               )),

                               box(title = "Carte du nombre d'élèves",status = "primary",width ="50000px",solidheader = TRUE,
                                   radioButtons(inputId="annee_carte",label = "Choisissez une année :",choices=c(2014,2015,2016,2017,2018,2019,2020,2021)),
                                   fluidRow(style="margin:6px;")
                                   # plotOutput("carte_evol_enseignant")
                               )
                    ),
                    tabPanel("Taux de réussite DNB par département",
                             fluidRow(style="margin:6px;",
                             radioButtons(inputId="annee_geo",label = "Choisissez une année",inline = TRUE,choices=c(2014,2015,2016,2017,2018,2019,2020,2021)),
                             withSpinner(
                               plotOutput("carte_reussite_DNB"),
                               type = 1))
                    ),
                    
                    tabPanel("Taux de scolarisation en France",
                             fluidRow(style="margin:6px;",
                             h2("Taux de scolarisation selon les régions en France"),
                             box(status = "primary",width ="50000px",solidheader = TRUE, 
                             withSpinner(
                               plotOutput("taux_scolarisation_FR"),
                               type = 1))
                             )
                    ),
                    
                    tabPanel("Etudiants en mobilité internationale",
                             selectizeInput("Pays_mobilite",label="Pays",
                                            choices = list("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                                           "France","G20","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                                           "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")),
                             withSpinner(
                               plotOutput("mobilite"),
                               type = 1)
                    )
                  )
                )),
        
        
        
        
        ### Inégalités de genre 
        
        tabItem(tabName = "genre",
                fluidPage(
                  tabsetPanel(
                    tabPanel("Répartition des bacs",
                             fluidRow(style='margin:6px;'),
                             box(title = "Répartition des baccalauréats selon le genre des élèves",status = "primary",width ="50000px",solidheader = TRUE, 
                                 withSpinner(
                                   plotOutput("repartition_bac"),
                                   type = 1))
                    )
                  )
                )
                
        ),
        
        ### Sources 
        
        # A MODIFIER A PARTIR D'ICI !!!!
        tabItem(tabName = "BDD",
                fluidPage(
                  radioButtons(inputId = "affichage_table", label = "Choisissez une table à afficher", inline = TRUE,
                               choices = c("enseignants_eleves", "taux_obtention", "segregation")),
                  dataTableOutput('enseignants_eleves'),
                  dataTableOutput('taux_obtention'),
                  dataTableOutput('segregation')
                  # dataTableOutput('tx_scolarisation'),
                  # dataTableOutput('mobilite'),
                  # dataTableOutput('fr_scolarisation_dpt'),
                  # dataTableOutput('fr_scolarisation_reg'),
                  # dataTableOutput('fr_reuss_bac'),
                  # dataTableOutput('fr_dnb_etab'),
                  # dataTableOutput('fr_bours_dpt'),
                  # dataTableOutput('fr_bac_acad')
                )
        )
        
        
))))