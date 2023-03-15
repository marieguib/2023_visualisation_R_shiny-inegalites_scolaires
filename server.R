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
                 title="Répartition des clases sociales au collège")
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

  # Carte taux de réussite DNB par département
  # output$taux_scolarisation_FR <- renderPlot({
  #   carte_tx_scolarisation
  # })
  
  # Carte taux de réussite DNB par département
  output$carte_tx_reussite <- renderPlot({
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
  output$enseignants_eleves <- renderDataTable({DT::datatable(data=head(enseignant_par_eleves))})
  output$taux_obtention <- renderDataTable({DT::datatable(data=head(taux_obtention_diplome))})
  output$segregation <- renderDataTable({DT::datatable(data=head(fr_indicateur_segreg_college))})
  # output$tx_scolarisation <- renderDataTable({DT::datatable(data=head(taux_scolarisation))})
  # output$mobilite <- renderDataTable({DT::datatable(data=head(etud_mobilite))})
  # output$fr_scolarisation_dpt <- renderDataTable({DT::datatable(data=head(fr_taux_scolarisation_dpt))})
  # output$fr_scolarisation_reg <- renderDataTable({DT::datatable(data=head(fr_taux_scolarisation_reg))})
  # output$fr_reuss_bac <- renderDataTable({DT::datatable(data=head(fr_reussite_bac))})
  # output$fr_dnb_etab <- renderDataTable({DT::datatable(data=head(fr_dnb_etablissement))})
  # output$fr_bours_dpt <- renderDataTable({DT::datatable(data=head(fr_boursiers_dpt))})
  # output$fr_bac_acad <- renderDataTable({DT::datatable(data=head(fr_bac_academie))})
  
})