ui <- navbarPage(

  title = "Heart Disease Explorer",

  # ======================
  # ACCUEIL
  # ======================

  tabPanel(
    "Accueil",

    fluidPage(

      h1("Comprendre et prédire les maladies cardiaques"),

      p(
        "Quels facteurs cliniques sont associés à la présence
        d'une maladie cardiaque et peut-on prédire cette présence
        à partir de ces facteurs ?"
      )

    )
  ),


  # ======================
  # EXPLORATION
  # ======================

  tabPanel(
    "Explorer les données",

    fluidPage(

      h2("Qui sont les patients ?"),

      p(
        "Exploration de la population, des distributions
        et de la qualité des données."
      )

    )
  ),


  # ======================
  # ANALYSE STATISTIQUE
  # ======================

  tabPanel(
    "Comprendre les facteurs",

    fluidPage(

      h2("Quels facteurs sont associés à la maladie ?"),

      p(
        "Analyse des relations entre les variables cliniques
        et la présence d'une maladie cardiaque."
      )

    )
  ),


  # ======================
  # MACHINE LEARNING
  # ======================

  tabPanel(
    "Prédire le risque",

    fluidPage(

      h2("Peut-on prédire la maladie ?"),

      p(
        "Construction et comparaison de modèles de classification."
      )

    )
  ),


  # ======================
  # SYNTHESE
  # ======================

  tabPanel(
    "Synthèse",

    fluidPage(

      h2("Que retenir ?"),

      p(
        "Résumé des résultats, limites et interprétation."
      )

    )
  )

)