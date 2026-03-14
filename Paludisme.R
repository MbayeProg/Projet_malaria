###### I_. TRAITEMENT DE LA BASE DE DONNEES PALUDISME ----

############ I-0._ IMPORTATION DES LIBRAIRIES ----

# LISTE DES LIBRAIRIES
packages <- c('readxl',    # data reader (.xls ; .xlsx )
              'tidyverse', # data wrangling (contenant: dplyr, ggplot2, tidyr, etc.)
              'gtsummary', #
              'plotly',    # data viz (graphiques interactifs)
              'cowplot',   # data viz (combinaison de plusieurs graphiques)
              'GGally',    # data viz (Creer des matrices graphiques et viz multivariee)
              'corrplot',  # data viz (viz matrices de correlation)
              'labelled',  # data labelled (gerer les etiquettes de variables et de valeurs)
              'explore',   # EDA (Exploratory Data Analysis)
              'naniar',    # missing data (Gerer et viz les donnees manquantes)
              'janitor',   # data cleaning ( Nettoyer et Preparer les donnees)
              'caret',     # ML (Apprentissage automatique ML: Classification, regression etc)
              'recipes',   # Pre-processing (Pour le pre-processing des donnees)
              'auditor',   # REC Curve ( Pour levaluation des modeles: courbes ROC, etc)
              'magrittr'   # Fournit le caractere de liaison de fonctions %>%
)

# IMPORTATION DES LIBRAIRIES
for (pkg in packages) {
  library(pkg, character.only = TRUE)
}



#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________





   #####   I-1._ IMPORTATION DE LA BASE DE DONNEES ----
#____________________________________________________________#

data_palu <- read_excel("Base Donnees Paludisme 2008_2022 par Région.xlsx")

### STRUCTURE DE LA BASE
data_palu |> glimpse()

"data_palu |> str()"



#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________



      #### II_. NETTOYAGE ET SIMPLIFICAION DES DONNEES ----
#______________________________________________________________#

## DUPLICATION DE LA BASE 
df <- data_palu
"df |> glimpse()"


### RECUPERATION DES NOMS DE COLONNES
description <- (colnames(df))

### RENOMMAGE DES NOMS DE COLONNES EN v1,v2,v3,...etc DE La base DF
colnames(df) <- paste("V", 1: length(names(df)), sep = "")

# LABELISATION DES COLONNES DE LA BASE DF
var_label(df) <- description


###### Verification des donnees manquantes et des doublons (naniar)
# Visualisation des donnees manquantes
df |> gg_miss_var()

# Verification des doublons
table(duplicated(df))


### SIMPLIFICATION DES VALEURS ET CORRECTIONS
df |> describe_all() |>
  print(n=22)

unique(df$V2)
df <- df |> mutate(V2 = str_to_upper(V2))
var_label(df) <- description   # recuperation de labelle de v2: regions

unique(df$V4)

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________




            #### III_. EXPLORATION DES DONNEES ----
#______________________________________________________________#

    ## ANALYSE UNIVARIEE:

#* *STATISTIQUES DESCROPTIVES DES DONNEES*
df |> describe()    # Description de la base
df |> summary()     # Description des variables
#_____________

#* *GRAPHIQUES DES DONNEES*
### Frequence des regions
dev.new(width= 10, height= 7)
df |> ggplot(data=df,
             mapping = aes(x=V2)
) + 
  geom_bar( col = "#616a6b",
            fill = "#fad7a0",
  ) +
  labs( x = "Régions",
        y = "Fréquences",
        title = "Distribution des régions"
  ) +
  theme_minimal()

#________
### Frequence  des mois
dev.new(width= 10, height= 7)
df |> ggplot(data=df,
             mapping = aes(x=V4)
) + 
  geom_bar(  col = "#616a6b",
             fill = "#fad7a0",
             # bin= 15
  ) +
  labs( x = "Mois",
        y = "Frequences",
        title = "Distribution des mois"
  ) +
  theme_minimal()
#________
### Frequence des annees
dev.new(width= 10, height= 7)
df |> ggplot(data=df,
             mapping = aes(x=V3)
) + 
  geom_bar(  col = "orange",
             fill = "#616a6b",
             # bin= 15
  ) +
  labs( x = "Années",
        y = "Frequences",
        title = "Fréquence_2 des années"
  ) +
  theme_minimal()


#______________________________________
#* * Correction des donnees manquantes pour l'annee 2008*

# Copie de la base df avant elimination
df1 <- df

# Elimination de l'annee 2008 presentant des donnees manquantes
df <- df |>
  filter(df$V3 != 2008)
#______________________________________




#____________________________
    ## ANALYSE BIVARIEE:


# VERIFICATION DES LIENS (CORRELATIONS) ENTRE V5, V6, V7, V8, V9
# COURBES DE CORRELATIONS
dev.new(width= 10, height= 7)
df |> select(V5, V6, V7, V8, V9) |>
  ggpairs() +
  labs(
    title = "GRAPHIQUE DE CORRELATION DES VARIABLES (V5:V9)")

# GRAPHIQUES CIRCULAIRES DES CORRELATIONS
dev.new(width= 10, height= 7)
df |> select(V5, V6, V7, V8, V9) |>
  cor() |>
  corrplot::corrplot(type = "upper",
                     order = "hclust",
                     tl.col = "black",
                     tl.srt = 45)



#__________________________
# a.   GRAPHIQUE EN FONCTION DES REGIONS
#__________________________
### DISTRIBUTION DE LA POPULATION MOYENNE DE 2009 A 2022 PAR REGION
dev.new(width= 10, height= 7)

df |> select(V2, V5) |>
  mutate(V2 = as.factor(V2)) |>
  group_by(V2) |>
  summarise(moyen = mean(V5)) |>
  
  ggplot(aes( x = V2,
              y = moyen)
  ) +
  geom_bar( col= "#616a6b",
            stat = "identity",
            fill = "#fad7a0") +
  coord_flip() +
  labs( x = "Régions",
        y = "Population moyenne entre 2009 et 2022",
        title = "Distribution de la population moyenne entre 2009 et 2022 par région",
  ) +
  theme_minimal()

#__________________________
# GRAPHIQUE COMBINE DU PROCESSUS LARGE DE DIAGNOSTIQUE

dev.new(width= 10, height= 7)

df |> select(V2, V6:V9) |>
  mutate(V2 = as.factor(V2)) |>
  group_by(V2) |>
  summarise(moyen6 = mean(V6), moyen7 = mean(V7),
            moyen8 = mean(V8), moyen9 = mean(V9)) |>
  mutate(V2 = fct_reorder(V2, -moyen7)) |>
  
  # Transformation en format long
  pivot_longer(cols = c(moyen6, moyen7, moyen8, moyen9),
               names_to = "indicateur",
               values_to = "valeur"
               ) |>
  ggplot(aes(x = V2, y=valeur, fill = indicateur)
  ) +
  geom_col(position = position_dodge(width = 0.6),
           color = "black",
           width = 0.8
  ) +
  # coord_flip() +
  scale_fill_manual(name = "Indicateurs",
                    values = c(moyen6 = "#f6ddcc", moyen7 = "#a9cce3",
                               moyen8 = "#f7dc6f", moyen9 = "#d98880"
                    ),
                    labels = c(moyen6 = "Cons. Toutes causes",
                               moyen7 = "Cas suspects",
                               moyen8 = "TDR réalises",
                               moyen9 = "Cas positifs"
                    )
  ) +
  labs(x = "Régions",
       y = "Moyenne des distributions",
       title = "Diagramme récapitulatif des études sur le Paludisme par région"
  ) +
  theme_minimal()

#___________________________
# GRAPHIQUE DU PROCESSUS DE DIAGNOSTIQUE

dev.new(width= 10, height= 7)

df |> select(V2, V7:V9) |>
  mutate(V2 = as.factor(V2)) |>
  group_by(V2) |>
  summarise(moyen7 = mean(V7), moyen8 = mean(V8), moyen9 = mean(V9)) |>
  mutate(V2 = fct_reorder(V2, -moyen7)) |>
  
  # Transformation en format long
  pivot_longer(cols = c(moyen7, moyen8, moyen9),
               names_to = "indicateur",
               values_to = "valeur"
  ) |>
  ggplot(aes(x = V2, y=valeur, fill = indicateur)
  ) +
  geom_col(position = position_dodge(width = 0.6),
           color = "black",
           width = 0.8
  ) +
  # coord_flip() +
  scale_fill_manual(name = "Indicateurs",
                    values = c(moyen7 = "#a9cce3",
                               moyen8 = "#f7dc6f",
                               moyen9 = "#d98880"
                    ),
                    labels = c(moyen7 = "Cas suspects",
                               moyen8 = "TDR réalises",
                               moyen9 = "Cas positifs"
                    )
  ) +
  labs(x = "Régions",
       y = "Moyenne des distributions",
       title = "Diagramme récapitulatif des études strictes sur le Paludisme par région"
  ) +
  theme_minimal()


# Graphique du rapport cas de malaria – tests TDR par région :

dev.new(width= 10, height= 7)

df |> select(V2, V8, V9) |>
  mutate(V2 = as.factor(V2)) |>
  group_by(V2) |>
  summarise(moyen8 = mean(V8), moyen9 = mean(V9), moyen = moyen9/moyen8) |>
  mutate(V2 = fct_reorder(V2, moyen)) |>
  
  ggplot(aes( x = V2,
              y = moyen)
  ) +
  geom_bar( col= "#616a6b",
            stat = "identity",
            fill = "#d98880") +
  coord_flip() +
  labs(x = "Régions",
       y = "TPP",
       title = "Distribution des TPP en fonction des région"
  ) +
  theme_minimal()


#_______________________
#   b. GRAPHIQUE LARGE EN FONCTION DES MOIS
#_________________
# Graphique combine

#mois_fr = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août",
#         "Septembre", "Octobre", "Novembre", "Décembre")

dev.new(width= 10, height= 7)
df |> select(V4, V6, V7, V8, V9) |>
  mutate(V4 = factor(V4, levels = mois_fr)) |>
  group_by(V4) |>
  summarise(moyen6= mean(V6), moyen7 = mean(V7), moyen8 = mean(V8), moyen9 = mean(V9)) |>
  # Transformation en format long
  pivot_longer(cols = c(moyen6, moyen7, moyen8, moyen9),
               names_to = "indicateur",
               values_to = "valeur"
  ) |>
  
  ggplot(aes(x = V4, y=valeur, fill = indicateur)
  ) +
  
  geom_col(position = position_dodge(width = 0.6),
           color = "black",
           width = 0.8
  ) +
  # coord_flip() +
  
  scale_fill_manual(name = "Indicateurs",
                    values = c(moyen6 = "#f6ddcc",
                               moyen7 = "#a9cce3",
                               moyen8 = "#f7dc6f",
                               moyen9 = "#d98880"),
                    labels = c(moyen6 = "Cons. Toutes causes",
                               moyen7 = "Cas suspects",
                               moyen8 = "TDR realises",
                               moyen9 = "Cas positifs")
  ) +
  
  labs(x = "Mois",
       y = "Moyenne des distributions",
       title = "Diagramme récapitulatif des études sur le Paludisme par mois"
  ) +
  theme_minimal()

#_______________________
#   b. GRAPHIQUE DIAGNOSTIK EN FONCTION DES MOIS
#_________________

# Graphique combine
dev.new(width= 10, height= 7)
df |> select(V4, V7, V8, V9) |>
  mutate(V4 = factor(V4, levels = mois_fr)) |>
  group_by(V4) |>
  summarise(moyen7 = mean(V7), moyen8 = mean(V8), moyen9 = mean(V9)) |>

  
  # Transformation en format long
  pivot_longer(cols = c(moyen7, moyen8, moyen9),
               names_to = "indicateur",
               values_to = "valeur"
  ) |>
  ggplot(aes(x = V4, y=valeur, fill = indicateur)
  ) +
  geom_col(position = position_dodge(width = 0.6),
           color = "black",
           width = 0.8
  ) +
  # coord_flip() +
  scale_fill_manual(name = "Indicateurs",
                    values = c(moyen7 = "#a9cce3",
                               moyen8 = "#f7dc6f",
                               moyen9 = "#d98880"
                    ),
                    labels = c(# moyen6 = "Cons. Toutes causes",
                               moyen7 = "Cas suspects",
                               moyen8 = "TDR realises",
                               moyen9 = "Cas positifs"
                    )
  ) +
  labs(x = "Mois",
       y = "Moyenne des distributions",
       title = "Diagramme récapitulatif des études strictes sur le Paludisme par mois"
  ) +
  theme_minimal()

#_______________
# Graphique des diagnostiques en fonction des annees
dev.new(width= 10, height= 7)
df |> select(V3, V7, V8, V9) |>
  mutate(V3 = as.factor(V3)) |>
  group_by(V3) |>
  summarise(moyen7 = mean(V7), moyen8 = mean(V8), moyen9 = mean(V9)) |>
  mutate(V3 = fct_reorder(V3, -moyen7)) |>
  
  # Transformation en format long
  pivot_longer(cols = c(moyen7, moyen8, moyen9),
               names_to = "Indicateur",
               values_to = "valeur"
  ) |>
  ggplot(aes(x = V3, y=valeur, fill = Indicateur)
  ) +
  geom_col(position = position_dodge(width = 0.6),
           color = "black",
           width = 0.8
  ) +
  # coord_flip() +
  scale_fill_manual(name = "Indicateurs",
                    values = c(moyen7 = "#a9cce3",
                               moyen8 = "#f7dc6f",
                               moyen9 = "#d98880"
                    ),
                    labels = c(moyen6 = "Cons. Toutes causes",
                               moyen7 = "Cas suspects",
                               moyen8 = "TDR realises",
                               moyen9 = "Cas positifs"
                    )
  ) +
  labs(x = "Années",
       y = "Moyenne des distributions",
       title = "Diagramme récapitulatif en fonction des années"
  ) +
  theme_minimal()

# TPP en fonction des annees
dev.new(width= 10, height= 7)

df |> select(V4, V9) |>
  mutate(V3 = as.factor(V4)) |>
  group_by(V4) |>
  summarise(moyen = mean(V9)) |>
  # mutate(V4 = fct_reorder(V4, moyen)) |>
  ggplot(aes( x = V4,
              y = moyen)
  ) +
  geom_bar( col= "#616a6b",
            stat = "identity",
            fill = "#d98880") +
  # coord_flip() +
  labs(x = "Mois",
       y = "Nombre de cas de paludisme",
       title = "Distribution des TPP en fonction des mois"
  ) +
  theme_minimal()





#_______________________________________________________________________________
#_______________________________________________________________________________




                ###. IV_. DATA FEATURES ENGINEERING ----
#_________________________________________________________________#

df_prep1 <- df |> 
  mutate(
    date = ymd(paste(V3, V4, "01", sep = "-")),
    
    mois = month(date),
    
    periode = case_when(mois %in% 3:6 ~ "P1",
                        mois %in% c(7,8,9,10) ~ "P2",
                        mois %in% c(1,2,11,12) ~ "P1"
    ),
      periode = factor(periode, levels = c("P3","P2", "P1")),
    
    #___
    localite = ifelse(V2 %in% c("KOLDA",
                                "TAMBACOUNDA",
                                "KEDOUGOU"), V2,
                      "Others Regions") |>
      
      factor(levels = c( "KEDOUGOU", "TAMBACOUNDA","Others Regions", "KOLDA"))
  ) |> 
  
  select(localite, periode, V6, V7, V9,V10)  
####################

df_prep1 <- df |> 
  mutate(
    # Création de la date complète
    date = ymd(paste(V3, V4, "01", sep = "-")),
    
    # Extraction du mois
    mois = month(date),
    
    # Classification des périodes selon ta demande
    periode = case_when(
      mois %in% c(3, 4, 5, 6) ~ "P1",         # Mars à Juin
      mois %in% c(7, 8, 9, 10) ~ "P2",        # Juillet à Octobre
      mois %in% c(11, 12, 1, 2) ~ "P3"        # Novembre à Février
    ),
    # Définir l’ordre des niveaux avec P1 comme référence
    periode = factor(periode, levels = c("P1", "P2", "P3")),
    
    # Reclassification des localités
    localite = ifelse(
      V2 %in% c("KOLDA", "TAMBACOUNDA", "KEDOUGOU"),
      V2,
      "Others Regions"
    ),
    localite = factor(localite, levels = c("KEDOUGOU", "TAMBACOUNDA", "KOLDA", "Others Regions"))
  ) |> 
  select(localite, periode, V6, V7, V9, V10)

#_______________________________________________________________________________
#_______________________________________________________________________________


                    ####. IV_.MODELISATION ----
#______________________________________________________________#

# MODELE LINEAIRE
model_palu <- lm(log(V9+1) ~., data = df_prep1)
summary(model_palu)

library(ggplot2)
library(gridExtra)

# 1. Préparer les données (exemple fictif basé sur votre analyse)
donnees <- data.frame(
  mois = factor(c("Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Sept", "Oct", "Nov", "Déc", "Jan", "Fév"),
                levels = month.abb),
  cas_reels = c(1500, 1200, 900, 800, 2000, 3500, 4200, 5000, 3000, 1800, 1000, 800), # Données simulées
  periode = c("P1", "P1", "P1", "P1", "P3", "P3", "P3", "P3", NA, NA, NA, NA) # Périodes correspondantes
)

# 2. Créer le graphique à double axe
plot_dual <- ggplot(donnees, aes(x = mois)) +
  
  # Premier axe (gauche) : Cas réels
  geom_col(aes(y = cas_reels, fill = "Cas réels de paludisme"), 
           alpha = 0.6, width = 0.6) +
  scale_y_continuous(
    name = "Nombre de cas (réels)",
    sec.axis = sec_axis(~./1000, name = "Coefficient du modèle")
  ) +
  
  # Deuxième axe (droite) : Coefficients du modèle
  geom_point(
    data = subset(donnees, !is.na(periode)),
    aes(y = case_when(
      periode == "P1" ~ 723 * 10,
      periode == "P3" ~ 1701 * 10
    ), color = periode),
    size = 4, shape = 18
  ) +
  scale_color_manual(
    name = "Coefficients (x10)",
    values = c("P1" = "red", "P3" = "darkgreen"),
    labels = c("P1 (+723)", "P3 (+1701)")
  ) +
  
  # Mise en forme
  labs(
    title = "Comparaison des cas réels et des effets saisonniers du modèle",
    x = "Mois",
    caption = "Note : Les coefficients sont multipliés par 10 pour mise à l'échelle"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title.y.right = element_text(color = "darkgreen"),
    axis.title.y.left = element_text(color = "steelblue")
  )

print(plot_dual)
# Verification de la redondance des variables explicatives
vif(model_palu)

# Precision du modele
dev.new(width=10, height=7)
plot(model_palu)
shapiro.test(residuals(model_palu))
dev.off()

dev.new(width=10, height=7)
pdf("diagnostic_model_palu.pdf", width=10, height=7)
plot(model_palu)
dev.off()

# Mesure de la colinéarité ..............................
mc <- model_palu |> performance::check_collinearity()
print(mc)

# VIZ 
dev.new(width= 10, height=7)
plot(mc)






