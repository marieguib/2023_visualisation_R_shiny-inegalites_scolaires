source("global.R",local=T)


library(shiny)

shinyServer(function(input, output) {
  
  ### Accueil
  
  # Value-Box
  output$educ_nationale <- renderValueBox({
    education_nationale
  })
  
  output$loi_Falloux <- renderValueBox({
    Loi_Falloux_Box
  })
  
  output$progres_filles <- renderValueBox({
    Progres_filles
  })
  
  output$gratuite <- renderValueBox({
    Gratuite
  })
  
  output$sep_eglise_etat <- renderValueBox({
    Separation_eglise_etat
  })
  
  output$filles_bac <- renderValueBox({
    Bac_filles
  })
  
  output$prog_identique_F_G <- renderValueBox({
    Prog_identiques
  })
  
  output$ecole_obl_16 <- renderValueBox({
    Ecole_obligatoire_16
  })
  
  output$mixite <- renderValueBox({
    Mixite_filles_garcons
  })
  
  output$creation_bac_pro <- renderValueBox({
    Creations_bacs_pros
  })
  
  ## Image personnages important
  
  output$Jules_FERRY <- renderImage({
    list(src="www/jules_FERRY.jpg",alt="Jules_FERRY",width=300,height=400,style='position : relative')
  },deleteFile=FALSE)
  
  
  output$Martin_LUTHER <- renderImage({
    list(src="www/Martin_LUTHER.jpg",alt="Martin_LUTHER",width=300,height=400,style='position : relative')
  },deleteFile=FALSE)
  
  output$Mustafa_Kemal_Atatürk <- renderImage({
    list(src="www/Mustafa_Kemal_Atatürk.jpg",alt="Mustafa_Kemal_Atatürk",width=300,height=400,style='position : relative')
  },deleteFile=FALSE)
  
  output$Francisco_Giner_de_los_Ríos <- renderImage({
    list(src="www/Francisco_Giner_de_los_Ríos.jpg",alt="Francisco_Giner_de_los_Ríos",width=300,height=400,style='position : relative')
  },deleteFile=FALSE)
  
  output$Mori_Arinori <- renderImage({
    list(src="www/Mori_Arinori.jpg",alt="Mori_Arinori",width=300,height=400,style='position : relative')
  },deleteFile=FALSE)
  
  
  ### Inégalités socio-économiques
  
  # Values-box
  output$bac_origine_sociale <- renderValueBox({
    valueBox(
      paste(baccalaureat,"%"),
      subtitle = " d'admis au baccalauréat quelque soit la classe sociale",
      icon = icon('graduation-cap'),
      color = "green"
    )
  })
  
  output$bac_sans_emploi <- renderValueBox({
    valueBox(
      paste(round(baccalaureat_sans_emploi$Pct_admis_baccalaureat),"%"),
      subtitle = "d'admis au baccalauréat avec des parents sans activite professionnelle",
      icon = icon('graduation-cap'),
      color = "red"
    )
  })
  
  output$bac_cadre <- renderValueBox({
    valueBox(
      paste(round(baccalaureat_cadre$Pct_admis_baccalaureat),"%"),
      subtitle = "d'admis au baccalauréat avec des parents cadres ou en professions intellectuelles supérieures",
      icon = icon('graduation-cap'),
      color = "green"
    )
  })
  
  
  # Classes sociales au college
  output$treemap_college <- renderPlot({
    
    df <- data.frame(
      group = c("très favorisé","favorisé","moyenne","défavorisé"),
      value = c(mean(fr_indicateur_segreg_college$proportion_tfav[fr_indicateur_segreg_college$nom_dep==input$nom_departement&fr_indicateur_segreg_college$annee==input$annee]),
                mean(fr_indicateur_segreg_college$proportion_fav[fr_indicateur_segreg_college$nom_dep==input$nom_departement&fr_indicateur_segreg_college$annee==input$annee]),
                mean(fr_indicateur_segreg_college$proportion_moy[fr_indicateur_segreg_college$nom_dep==input$nom_departement&fr_indicateur_segreg_college$annee==input$annee]),
                mean(fr_indicateur_segreg_college$proportion_defav[fr_indicateur_segreg_college$nom_dep==input$nom_departement&fr_indicateur_segreg_college$annee==input$annee]))
    )
    p <- treemap(df,
                 index="group",
                 vSize="value",
                 type="index",
                 border.col = "black",
                 palette = "Blues",
                 title="Répartition des classes sociales au collège")
    p
    
  })
  
  # PCS au lycee
  output$camembert_lycee <- renderPlot({
    pie <- ggplot(df_PCS)+
      aes(x="",y="Pct_admis_baccalaureat",fill=Origine_sociale)+
      geom_bar(width = 1,stat="identity")+
      xlab("")+ylab("")+
      coord_polar("y",start=0)+
      theme_minimal()+
      ggtitle("Répartition des PCS au lycée")+
      theme(plot.title = element_text(hjust = 0.5))
    pie
    
  })
  
  # Classes sociales au college prive / public
  output$amchartComparaisonPCS <- renderAmCharts({
    comp_college_PU_PR
  })
  
  # Commentaire amchartComparaisonPCS
  output$comm_amchartComparaisonPCS <- renderText({commg_amchartComparaisonPCS})
  
  # Table reussite DNB selon le secteur privé / public 
  output$reussite_secteur <- renderDataTable({
    taux_reussite_secteur
  })
  
  # Reussite bac selon PCS au lycee
  output$reussite_bac_PCS <- renderPlot({
    df_courbe_reussite_bac <- fr_reussite_bac |> dplyr::filter(Origine_sociale==input$origine_sociale)
    # amPlot(Annee~`Pourcentage d'admis au baccalaureat general`,data = df_courbe_reussite_bac, type="l")
    ggplot(df_courbe_reussite_bac)+
      geom_line(aes(x = Annee,y=`Pourcentage d'admis au baccalaureat general`,color="baccalauréat général"))+
      geom_line(aes(x = Annee,y=`Pourcentage d'admis au baccalaureat technologique`,color="baccalauréat technologique"))+
      geom_line(aes(x = Annee,y=`Pourcentage d'admis au baccalaureat professionnel`,col="baccalauréat professionnel"))+
      labs(xlab = "Année", ylab = "Pourcentage d'admis",color="Bacs:",title="Réussite par bac selon la PCS")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Boutton de téléchargement
  # 
  # output$downloadPlot <- downloadHandler(
  #   filename = function(){
  #     paste("graphique_",Sys.Date(),".png",sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = output$reussite_bac_PCS())
  #   }
  # )

  # Créer une réaction au clic sur le graphique
  # observeEvent(input$plotClick, {
  #   filename <- paste("graphique_", Sys.Date(), ".png", sep="")
  #   ggsave(filename, plot = output$reussite_bac_PCS)
  #   file.rename(filename, paste0("./", filename))
  # })
  
  output$telechargement <- downloadHandler(
    filename = function(file) {
      paste("graphique_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = output$reussite_bac_PCS,device="png")
    }
    
  )

  ### Inegalités territoriales
  
  # Evolution du nombre d'enseignants par élèves
  # d <- enseignant_par_eleves |>
  #   group_by(TIME,LOCATION) |>
  #   mutate(nb_moy = mean(VALUE))
  # 
  # donnees_reactives <- reactive({
  #   variables_a_tracer <- input$Pays_mobilite
  #   donnees_a_tracer <- d[d$LOCATION==variables_a_tracer,]
  #   return(donnees_a_tracer)
  # })
  # 
  # observe({
  #   donnees <- donnees_reactives()
  #   
  #   graphique <- rAmCharts::AmChart() |> 
  #     rAmCharts::add_xy_chart(data = donnees, x = Annee, y = donnees$nb_moy)
  #   
  #   rAmCharts::amChartsOutput("evol_enseignant_eleves")
  #   rAmCharts::renderAmCharts()
  # })
  
  output$evol_enseignant_eleves <- renderPlot({
    d <- enseignant_par_eleves |>
      dplyr::filter(LOCATION==input$Pays_enseignant) |> 
      group_by(TIME) |>
      mutate(nb_moy = mean(VALUE))

    m <- ggplot(d)+
      aes(x=TIME,y=nb_moy)+
      geom_line()+
      labs(xlabs = "Année", ylab="Nombre moyen d'enseignant par élèves", title = paste("Evolution du nombre d'enseignant par élèves en ",input$Pays_enseignant))+
      theme(plot.title = element_text(hjust = 0.5))
    m

  })
  
  output$evol_enseignant_eleves_2 <- renderPlot({
    d <- enseignant_par_eleves |>
      dplyr::filter(LOCATION==input$Pays_enseignant2) |> 
      group_by(TIME) |>
      mutate(nb_moy = mean(VALUE))
    
    m <- ggplot(d)+
      aes(x=TIME,y=nb_moy)+
      geom_line()+
      labs(xlabs = "Année", ylab="Nombre moyen d'enseignant par élèves", title = paste("Evolution du nombre d'enseignant par élèves en",input$Pays_enseignant2))+
      theme(plot.title = element_text(hjust = 0.5))
    m
    
  })
  
  output$comparaison_evol_enseignant_eleves <- renderText({
    global_comparaison_evol_enseignant_eleves
  })
  
  # Carte du nombre d'enseignant par élèves
  # output$carte_evol_enseignant <- renderLeaflet({
  #   
  #   # INTRODUIRE UN REACTIVE
  #   enseignant_par_eleves <- enseignant_par_eleves |> 
  #     dplyr::filter(LOCATION==input$Pays_mobilite) |> 
  #     group_by(TIME) |> 
  #     mutate(nb_moy = mean(VALUE))
  #   
  #   world2 <- merge(x=world,y=enseignant_par_eleves,by.x="sov_a3",by.y="ACRONYME_PAYS")
  #   
  #   world3 <- world2 |> 
  #     dplyr::filter(TIME==input$annee_carte)
  #   
  #   coords <- st_coordinates(world3)
  #   longitude <- coords[,"X"]
  #   latitude <- coords[,"Y"]
  # 
  #   pal <- colorNumeric(scales::seq_gradient_pal(low = "yellow", high = "red",
  #                                                space = "Lab"), domain = world3$nb_moy)
  #   
  #   
  #   map <- leaflet() |>
  #     addTiles() |>
  #     setView(lng=0,lat=30,zoom=2) |>
  #     addPolygons(data = world3,color=~pal(nb_moy),
  #                 fillOpacity = 0.6) |>
  #                 # stroke = TRUE,weight=1,
  #                 # popup=~paste(as.character(NOM_DEPT),
  #                 #              as.character(t_prev),
  #                 #              sep=" : "),
  #                 # highlightOptions = highlightOptions(color = "black",
  #                 #                                     weight = 3,
  #                 #                                     bringToFront = TRUE)) %>%
  #     addLayersControl(options=layersControlOptions(collapsed = FALSE))
  # 
  #   map
  #   
  #                         
  #                        
  # })

  
  # Carte taux de réussite DNB par département
  output$carte_reussite_DNB <- renderPlot({
     dpt4 <- dpt3 |>
       dplyr::filter(Session==input$annee_geo) |>
       group_by(Session,geometry) |>
       as_tibble() |>
       st_as_sf()
     dpt4


   carte <- ggplot(dpt4)+
     aes(fill=reussite)+
     geom_sf()
   carte
  })
  
  # Carte PCS majoritaire par département
  
  
  
  # Carte taux de scolarisation
  output$taux_scolarisation_FR <- renderPlot({
    carte_tx_scolarisation
  })
  
  
  # Etudiants en mobilite internationale
  # output$mobilite <- renderPlot({
  #   
  #   etudiant_mobilite <- etud_mobilite |> 
  #     dplyr::filter(LOCATION==input$Pays_mobilite)
  #   
  #   ggplot(etudiant_mobilite)+
  #     aes(x=TIME,y=VALUE)+
  #     geom_line()+
  #     labs(xlab = "Année", ylab = "Nombre moyen d'étudiants en mobilité internationale", title = paste("Evolution du nombre d'étudiants en mobilité internationale en",input$Pays_mobilite))+
  #     theme(plot.title = element_text(hjust = 0.5))
  # })
  
  output$mobilite <- renderAmCharts({
    etudiant_mobilite <- etud_mobilite |> 
           dplyr::filter(LOCATION==input$Pays_mobilite)
      
    amPlot(VALUE~TIME,data =etudiant_mobilite, type="l",main = "Evolution du nombre d'étudiants en mobilité internationale")
  })
  
  
  
  
  ### Inegalités de genre

  # Repartition des bacs 
  output$repartition_bac <- renderPlot ({voies
  })
  
  
  ### Sources

  # Affichage des bases de données
  selected_df <- reactive({
    DT::datatable(data=head(liste_df[[input$affichage_table]]))
  })
  
  output$table <- renderDataTable({
    selected_df()
  })

  
})