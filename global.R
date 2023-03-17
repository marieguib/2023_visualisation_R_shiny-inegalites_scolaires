### Environnement de travail ---
library(readxl)
library(readr)
library(tidyverse)
library(leaflet)
library(sf)
library(rAmCharts)
library(DT)
library(BH)
library(treemap)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(dashboardthemes)
library(shinycssloaders)
library(rmapshaper)
library(rnaturalearthdata)
library(rnaturalearth)


### Importations des bases de données ---


enseignant_par_eleves <-  read.csv("data/Enseignant_par_eleves.csv",sep=",",header=TRUE,stringsAsFactor=TRUE)
enseignant_par_eleves <- enseignant_par_eleves[,c(1,6:7)]
colnames(enseignant_par_eleves) <- c("LOCATION","TIME","VALUE")
enseignant_par_eleves <- enseignant_par_eleves |> 
  dplyr::filter(TIME>=2014,TIME<=2021)
enseignant_par_eleves$ACRONYME_PAYS <- enseignant_par_eleves$LOCATION
enseignant_par_eleves$ACRONYME_PAYS <- as.factor(enseignant_par_eleves$ACRONYME_PAYS)
enseignant_par_eleves$LOCATION <- as.factor(enseignant_par_eleves$LOCATION)
levels(enseignant_par_eleves$LOCATION) <- c("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                            "France","G20","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                            "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")
summary(enseignant_par_eleves)

taux_obtention_diplome <- read.csv("data/Taux_obtention_diplome.csv",sep=",",header=TRUE,stringsAsFactor=TRUE)
taux_obtention_diplome <- taux_obtention_diplome[,c(2,4,6,8:9,12,15,17)]
taux_obtention_diplome <- na.omit(taux_obtention_diplome)
taux_obtention_diplome <- taux_obtention_diplome |> 
  dplyr::filter(YEAR>=2014,YEAR<=2021) |> 
  dplyr::filter(Indicateur == "Taux d’obtention d’un diplôme")
summary(taux_obtention_diplome)


fr_indicateur_segreg_college <- read.csv("data/Fr_indicateur_segregation_sociale_colleges.csv",sep=";",dec=".",header=TRUE,stringsAsFactor=TRUE)
fr_indicateur_segreg_college <- fr_indicateur_segreg_college[,c(2,4:6,8,9,11:25,27,28)]
colnames(fr_indicateur_segreg_college) <- c("annee","nom_academie","dep","nom_dep",
                                            "nb_college_PU","nb_college_PR",
                                            "proportion_tfav","proportion_fav","proportion_moy","proportion_defav",
                                            "proportion_tfav_PU","proportion_fav_PU","proportion_moy_PU","proportion_defav_PU",
                                            "proportion_tfav_PR","proportion_fav_PR","proportion_moy_PR","proportion_defav_PR",
                                            "indice_entropie_total","indice_entropie_PU","indice_entropie_PR",
                                            "contrib_college_PU","contrib_college_PR")
fr_indicateur_segreg_college$nom_dep <- as.factor(fr_indicateur_segreg_college$nom_dep)
summary(fr_indicateur_segreg_college)


taux_scolarisation <- read.csv("data/Taux_scolarisation_petite_enfance.csv",sep=",",header=TRUE,dec=".",stringsAsFactors = T)
taux_scolarisation <- bind_cols(taux_scolarisation[,c(1,6:7)])
summary(taux_scolarisation)


etud_mobilite <- read.csv("data/Pct_etudiants_en_mobilite.csv",sep=",",header=TRUE,dec=".",stringsAsFactors = T)
etud_mobilite <- bind_cols(etud_mobilite[,c(1,6:7)])
colnames(etud_mobilite) <- c("LOCATION","TIME","VALUE")
etud_mobilite <- etud_mobilite |> 
  dplyr::filter(TIME>=2014,TIME<=2021)
levels(etud_mobilite$LOCATION) <- c("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                    "France","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                    "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","OEU","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")
summary(etud_mobilite)


fr_taux_scolarisation_dpt <- read_excel("data/Fr-taux_scolarisation.xlsx",sheet=1)
fr_taux_scolarisation_dpt[,c("Numero_dpt","Departement")] <- lapply(fr_taux_scolarisation_dpt[,c("Numero_dpt","Departement")],factor)
summary(fr_taux_scolarisation_dpt)


fr_taux_scolarisation_reg <- read_excel("data/Fr-taux_scolarisation.xlsx",sheet=2)
fr_taux_scolarisation_reg$Numero <- factor(fr_taux_scolarisation_reg$Numero)
fr_taux_scolarisation_reg$Region <- factor(fr_taux_scolarisation_reg$Region)
summary(fr_taux_scolarisation_reg)

# PENSER A SUPPRIMER LES PCS AVEC DONT
fr_reussite_bac <- read_delim("data/Fr-reussite_bac_origine_sociale.csv",delim=";",show_col_types = FALSE)
fr_reussite_bac$Origine_sociale <- factor(fr_reussite_bac$Origine_sociale)
fr_reussite_bac <- fr_reussite_bac|> 
  dplyr::filter(Annee>=2014,Annee<=2021) |> 
  dplyr::filter(Origine_sociale!="Professions intermediaires : instituteurs et assimiles")

fr_reussite_bac <- fr_reussite_bac[!grepl("^dont", fr_reussite_bac$Origine_sociale), ]
summary(fr_reussite_bac)


fr_dnb_etablissement <- read_delim("data/Fr-dnb-par-etablissement.csv",delim=";",show_col_types = FALSE)
fr_dnb_etablissement <- fr_dnb_etablissement[,c(1,3,5,8:9,12:20)]
fr_dnb_etablissement[,c(2:7)] <- lapply(fr_dnb_etablissement[,c(2:7)],factor)
fr_dnb_etablissement <- fr_dnb_etablissement|> 
  dplyr::filter(Session>=2014,Session<=2021)
summary(fr_dnb_etablissement)


fr_boursiers_dpt <- read_delim("data/Fr-boursiers-par-departement.csv",delim=";",show_col_types = FALSE)
fr_boursiers_dpt <- fr_boursiers_dpt[,c(1,4:7)]
fr_boursiers_dpt[,c(2:4)] <- lapply(fr_boursiers_dpt[,c(2:4)],factor)
summary(fr_boursiers_dpt)


fr_bac_academie <- read_delim("data/Fr-bac_par_academie.csv",delim=";",show_col_types = FALSE)
fr_bac_academie <- fr_bac_academie[,c(1,3,5,8:21)]
fr_bac_academie[,c(2:3)] <- lapply(fr_bac_academie[,c(2:3)],factor)
summary(fr_bac_academie)


liste_df = list("OCDE : Enseignants par élèves"=enseignant_par_eleves,
                "OCDE : Taux d'obtention d'un diplôme"=taux_obtention_diplome,
                "France : Indicateur de ségrégation sociale"=fr_indicateur_segreg_college,
                "OCDE : Taux de scolarisation "=taux_scolarisation,
                "OCDE : Etudiants en mobilité internationale"=etud_mobilite,
                "France : Taux de scolarisation par département"=fr_taux_scolarisation_dpt,
                "France : Taux de scolarisation par région"=fr_taux_scolarisation_reg,
                'France : Réussite par baccalauréat'=fr_reussite_bac,
                "France : Obtention du brevet par établissement"=fr_dnb_etablissement,
                "France : Nombre de boursiers par établissement"=fr_boursiers_dpt,
                "France : Obtention du baccalauréat par académie"=fr_bac_academie)


### Importations des fonds de carte ---

# Départements de France
# dpt <- read_sf("data/dpt") 
# ggplot(dpt)+
#   geom_sf()

# Monde : pays de l'OCDE


### Acceuil --- 
# Value-box

education_nationale <- valueBox(
  "1932" , "L'instruction publique devient l'éducation nationale (renommé par le gouvernement d'Edouard Herriot",
  icon = icon("school"), color = "green"
)


Loi_Falloux_Box <- valueBox(
  1850, "Loi Falloux incite à ouvrir des écoles pour filles",
  icon = icon("list"),color = "purple"
)

Progres_filles <- valueBox(
  1880, "Les filles ont le droit d'aller au collège et au lycée", 
  icon = icon("list"), color ="green"
)

Gratuite <- valueBox(
  "1881-1882", "Gratuité de l'enseignement public par la loi Jules FERRY. L'Enseignement devient laïque et obligatoire",
  icon = icon("thumbs-up", lib = "glyphicon"), color = "yellow"
)

Separation_eglise_etat <- valueBox(
  "1905", "Séparation de l'Eglise et de l'Etat",
  icon = icon("bolt"), color = "red"
)

Bac_filles <- valueBox(
  "1923", "Les filles ont le droit de passer le baccalauréat",
  icon = icon("briefcase"), color = "maroon"
)


Prog_identiques <- valueBox(
  "1924", "Programmes du collège et du lycée identiques pour les filles et les garçons",
  icon = icon("briefcase"), color = "blue"
)

Ecole_obligatoire_16 <- valueBox(
  "1959", "Ecole obligatoire jusqu'à 16 ans",
  icon = icon("school"), color = "fuchsia"
)

Mixite_filles_garcons <- valueBox(
  "1969", "Mixité : Garçons et filles réunis au sein des mêmes établissements",
  icon = icon("children"), color = "teal"
)

Creations_bacs_pros <- valueBox(
  "1992", "Création des bacs professionnels",
  icon = icon("graduation-cap"), color = "olive"
)





### 
### Graphiques 

# Comparaison des PCS entre le college prive et public
PCS <- c("Tres favorise","favorise","moyenne","defavorise")
val_college_PU <- c(mean(fr_indicateur_segreg_college$proportion_tfav_PU),
                    mean(fr_indicateur_segreg_college$proportion_fav_PU),
                    mean(fr_indicateur_segreg_college$proportion_moy_PU),
                    mean(fr_indicateur_segreg_college$proportion_defav_PU))
val_college_PR <- c(mean(fr_indicateur_segreg_college$proportion_tfav_PR),
                    mean(fr_indicateur_segreg_college$proportion_fav_PR),
                    mean(fr_indicateur_segreg_college$proportion_moy_PR),
                    mean(fr_indicateur_segreg_college$proportion_defav_PR))

df <- data.frame(PCS, college_prive=val_college_PR,college_public=val_college_PU)
comp_college_PU_PR <- amBarplot(x = "PCS", y = c("college_prive", "college_public"),groups_color = c("#87cefa", "#c7158"), legend=TRUE,data = df,title="Comparaison des PCS entre le collège prive et public")



# Voies professionnelles des élèves
df_voies <- data.frame(Sexe=fr_bac_academie$Sexe,Voie=fr_bac_academie$Voie)
table(df_voies)
df <- data.frame(
  Sexe=c("Filles","Garcons"),
  Bac_General = c(89,89),
  Bac_Professionnel = c(2847,3894),
  Bac_Technologique = c(714,774)
)

df1 <- df |> pivot_longer(cols=c(Bac_General, Bac_Professionnel,Bac_Technologique),
                          names_to = "Bac",
                          values_to = "Valeur")
voies <- ggplot(df1) +
  aes(x=Sexe, y=Valeur, fill=Bac) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="Sexe",y="Nombre d'eleves",title="Nombre d'eleves selon la voie professionnelle en 2021")+
  scale_fill_brewer(palette="Blues")+
  theme(plot.title = element_text(hjust = 0.45))


# ---- Carte : Taux de réussite DNB par département -----
dpt <- sf::read_sf("data/dpt")
# Jointure entre dpt et fr_dnb_etablissement pour récupérer les multipolygons associés aux dpt
dpt2 <- merge(x=fr_dnb_etablissement,y=dpt,by.x="Libellé_département",by.y="NOM_DEPT")
dpt2
# VOIR AVEC INNER JOIN
dpt3 <- dpt2 |>
  select(Session,`Code département`,Inscrits,Admis,geometry) |>
  mutate(reussite = Admis/Inscrits*100)

# ---- Carte : PCS majoritaire selon le département ------
# fichier fr_segregation_sociale
# colone proportiontfav, proportion_fav, proportion_fav et proportion_defav
# Essai avec une seule année (pas de choix année)
# pour chaque dpt :
# max entre proportiontfav proportion_fav proportion_fav proportion_defav
# représenter 
# Carte interactive leaflet
dpt_pcs_maj <- merge(x=fr_indicateur_segreg_college,y=dpt, by.x = "nom_dep", by.y = "NOM_DEPT")

dpt_pcs_maj2 <- dpt_pcs_maj |> 
  select(nom_dep,annee,proportion_tfav,proportion_fav,proportion_moy, proportion_defav,geometry) |> 
  group_by(nom_dep,annee) |> 
  pivot_longer(cols=c(proportion_tfav,proportion_fav,proportion_moy, proportion_defav),
               names_to = "Classe_sociale",
               values_to = "Valeur") 
View(dpt_pcs_maj2)
  # mutate(PCS_maj = max(proportion_tfav,proportion_fav,proportion_moy,proportion_defav))

# IDEE : faire un pivot_longer 

# Carte Taux de scolarisation en france
regions <- read_sf("data/regions-20180101-shp/")
regions1 <- ms_simplify(regions)
regions2 <- merge(x=regions1,y=fr_taux_scolarisation_reg,by.x="code_insee",by.y = "Numero")
format(object.size(regions2),units="Mb")

carte_tx_scolarisation <- ggplot(regions2)+geom_sf()+
  geom_sf(aes(fill=`Total premier degre`))+
  coord_sf(xlim = c(-5.5,10),ylim=c(41,51))+
  scale_fill_continuous(low="yellow",high="red")+
  labs(title = "Taux de scolarisation (en maternelle et primaire) en France")+
  theme_void()


# # Carte enseignants par élèves : OCDE
# world <- ne_countries(returnclass = "sf")
# # head(world)
# world <- st_transform(world, "+proj=longlat +datum=WGS84")
# 
# # Carte vide :
# map <- leaflet() |> 
#   addTiles() |> 
#   setView(lng=0,lat=30,zoom=2)
# map




# Taux de réussite DNB selon le secteur (etablissement)
taux_reussite_public <- fr_dnb_etablissement |> 
  select(`Secteur d'enseignement`,Admis,Inscrits) |> 
  dplyr::filter(`Secteur d'enseignement`=="PUBLIC") |> 
  summarise(moy_reussite_public = round(mean(Admis/Inscrits),3))
colnames(taux_reussite_public) <- "Collèges publics"

taux_reussite_prive <- fr_dnb_etablissement |> 
  select(`Secteur d'enseignement`,Admis,Inscrits) |> 
  dplyr::filter(`Secteur d'enseignement`=="PRIVE") |> 
  summarise(moy_reussite_prive= round(mean(Admis/Inscrits),3))
colnames(taux_reussite_prive) <- "Collèges privés"

taux_reussite_secteur <- cbind(taux_reussite_prive,taux_reussite_public)
taux_reussite_secteur


# Moyenne du pct d'admis au baccalauréat selon la PCS 
df_PCS <- data.frame(
  value = aggregate(`Pourcentage d'admis au baccalaureat`~ Origine_sociale,data=fr_reussite_bac,mean)
)
colnames(df_PCS) <- c("Origine_sociale","Pct_admis_baccalaureat")
df_PCS <- df_PCS |> dplyr::filter(Origine_sociale!="Ensemble")
df_PCS


# Values-Box 
baccalaureat_cadre <- df_PCS |> 
  dplyr::filter(Origine_sociale=='Cadres, professions intellectuelles superieures')
baccalaureat_cadre

baccalaureat_sans_emploi <-df_PCS |> 
  dplyr::filter(Origine_sociale=='Autres personnes sans activite professionnelle')
baccalaureat_sans_emploi
  
baccalaureat <- round(sum(df_PCS$Pct_admis_baccalaureat)/nrow(df_PCS))
baccalaureat


# Commentaires graphiques ---

# Reussite bac selon PCS selon secteur au college
commg_amchartComparaisonPCS <- HTML("Ce graphique nous permet de distinguer la répartition des PCS selon le secteur d'enseignement.
Nous pouvons directement nous rendre compte des disparités sociales entre les collèges puisque la classe sociale majoritaire dans les collèges publics est défavorisée alors que dans ceux privés, elle correspond à une classe aisée.")

global_comparaison_evol_enseignant_eleves <- HTML("Ces deux graphiques permettent la comparaison entre deux pays.")