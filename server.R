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
    list(src="data/jules_FERRY.jpg",alt="Jules_FERRY",width=300,height=400,style='position : relative')
  },deleteFile=FALSE)
  
  
  output$Martin_LUTHER <- renderImage({
    list(src="data/Martin_LUTHER.jpg",alt="Martin_LUTHER",width=300,height=400,style='position : relative')
  },deleteFile=FALSE)
  
  output$Mustafa_Kemal_Atatürk <- renderImage({
    list(src="data/Mustafa_Kemal_Atatürk.jpg",alt="Mustafa_Kemal_Atatürk",width=300,height=400,style='position : relative')
  },deleteFile=FALSE)
  
  output$Francisco_Giner_de_los_Ríos <- renderImage({
    list(src="data/Francisco_Giner_de_los_Ríos.jpg",alt="Francisco_Giner_de_los_Ríos",width=300,height=400,style='position : relative')
  },deleteFile=FALSE)
  
  output$Mori_Arinori <- renderImage({
    list(src="data/Mori_Arinori.jpg",alt="Mori_Arinori",width=300,height=400,style='position : relative')
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
    fr_reussite_bac |> filter(Origine_sociale==input$origine_sociale) |> 
      ggplot()+
      geom_line(aes(x = Annee,y=`Pourcentage d'admis au baccalaureat general`),col="red")+
      geom_line(aes(x = Annee,y=`Pourcentage d'admis au baccalaureat technologique`),col="blue")+
      geom_line(aes(x = Annee,y=`Pourcentage d'admis au baccalaureat professionnel`),col="black")
  })
  


  ### Inegalités territoriales
  
  # Evolution du nombre d'enseignants par élèves
  output$evol_enseignant_eleves <- renderPlot({
    d <- enseignant_par_eleves |>
      filter(LOCATION==input$Pays_mobilite) |>
      group_by(TIME) |>
      mutate(nb_moy = mean(VALUE))
    
    m <- ggplot(d)+
      aes(x=TIME,y=nb_moy)+
      geom_line()
    m
    
  })
  
  # Carte du nombre d'enseignant par élèves
  # output$carte_evol_enseignant <- renderLeaflet({
  #   
  #   # INTRODUIRE UN REACTIVE
  #   enseignant_par_eleves <- enseignant_par_eleves |> 
  #     filter(LOCATION==input$Pays_mobilite) |> 
  #     group_by(TIME) |> 
  #     mutate(nb_moy = mean(VALUE))
  #   
  #   world2 <- merge(x=world,y=enseignant_par_eleves,by.x="sov_a3",by.y="ACRONYME_PAYS")
  #   
  #   world3 <- world2 |> 
  #     filter(TIME==input$annee_carte)
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
       filter(Session==input$annee_geo) |>
       group_by(Session,geometry) |>
       as_tibble() |>
       st_as_sf()
     dpt4


   carte <- ggplot(dpt4)+
     aes(fill=reussite)+
     geom_sf()
   carte
  })
  
  # Carte taux de scolarisation
  output$taux_scolarisation_FR <- renderPlot({
    carte_tx_scolarisation
  })
  
  
  # Etudiants en mobilite internationale
  output$mobilite <- renderPlot({
    ggplot(etud_mobilite)+
      aes(x=TIME,y=VALUE,col=LOCATION==input$Pays_mobilite)+
      geom_line()
    
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