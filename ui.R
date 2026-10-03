# Interface Shiny — lancer depuis la racine avec shiny::runApp(".")
library(shiny)

fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f6f8fa; color: #203040; }
    .container-fluid { max-width: 1400px; }
    .tab-content { background: white; padding: 22px; border: 1px solid #e2e7eb; }
    .well { background: #edf3f7; border: 0; }
    .note { color: #526372; margin-bottom: 18px; }
  "))),
  titlePanel("Maladie coronarienne : explorer les données UCI"),
  p(class = "note", "Quatre centres, des données incomplètes et une première analyse du diagnostic à Cleveland."),
  tabsetPanel(
    tabPanel("1 · Données",
      fluidRow(
        column(3,
          selectInput("source_apercu", "Provenance", choices = c("Toutes" = "toutes")),
          numericInput("n_apercu", "Nombre de premières lignes", 6, min = 1, max = 100),
          helpText("Ce filtre concerne uniquement cet onglet. Le diagnostic original et sa version binaire sont conservés.")
        ),
        column(9,
          h4("Aperçu du tableau fusionné"), textOutput("dimensions"),
          DT::DTOutput("apercu"),
          h4("Effectifs par provenance"), tableOutput("effectifs")
        )
      ),
      h4("Dictionnaire des variables"), DT::DTOutput("dictionnaire"),
      h4("Profils identiques"),
      p("Comparaison des 14 variables UCI originales. Les NA aux mêmes positions sont considérés identiques. Aucune ligne n'est supprimée ; un profil identique ne prouve pas l'identité du patient."),
      DT::DTOutput("doublons")
    ),
    tabPanel("2 · Valeurs manquantes",
      p("Bilan des quatre sources UCI. Les ? sont lus comme NA. Les zéros n'ont pas été recodés ici : la règle discutée pour Kaggle reste distincte."),
      plotOutput("carte_na", height = "630px"),
      fluidRow(
        column(6, h4("Toutes provenances réunies"), tableOutput("na_global_ui")),
        column(6, h4("Bilan par provenance"), DT::DTOutput("na_sources_ui"))
      ),
      h4("Détail par variable et provenance"), DT::DTOutput("na_detail")
    ),
    tabPanel("3 · Cleveland",
      p("Question : le nombre de vaisseaux visualisés apporte-t-il une information sur le diagnostic ?"),
      p(class = "note", "Diagnostic binaire : 0 = absence et 1 = présence selon le critère UCI. Le code 0 ne signifie pas l'absence de tout problème cardiaque."),
      h4("Observations disponibles"), tableOutput("bilan_cleveland"),
      h4("Effectifs croisés, y compris les valeurs manquantes"), tableOutput("croisement"),
      plotOutput("proportions", height = "430px"),
      tableOutput("table_proportions"),
      p(class = "note", "Les points décrivent une association brute. Les intervalles de Wilson à 95 % indiquent l'incertitude sur chaque proportion, sans ajustement pour comparaisons multiples."),
      hr(), h4("Association ajustée : comparaison de deux régressions logistiques"),
      p("Modèle réduit : les 12 autres variables explicatives. Modèle complet : ajout de nb_vaisseaux comme catégorie. Les deux modèles utilisent exactement les mêmes cas complets de Cleveland."),
      p("Les variables catégorielles sont traitées comme des facteurs. Les variables continues ont un effet linéaire sur le logit. Aucun terme d'interaction n'est ajouté."),
      actionButton("calcul_modeles", "Calculer la comparaison", class = "btn-primary"),
      verbatimTextOutput("resultat_modeles"),
      p(class = "note", "Le test du rapport de vraisemblance évalue l'apport de nb_vaisseaux à l'ajustement. Il ne démontre ni causalité ni gain prédictif hors échantillon. Les modèles restent exploratoires." )
    )
  ),
  br(), tags$small("Source : Janosi, Steinbrunn, Pfisterer et Detrano (1989), UCI Heart Disease, DOI 10.24432/C52P4X. Données sous CC BY 4.0.")
)
