
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

# INSTALLATION DES LIBRAIRIES
for (pkg in packages) {
  install.packages(pkg, dependencies = TRUE)
}