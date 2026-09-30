# Projet Heart Disease — étape 1 : importation et diagnostic des NA
# Source : https://archive.ics.uci.edu/dataset/45/heart+disease
# Janosi, Steinbrunn, Pfisterer & Detrano (1989), DOI: 10.24432/C52P4X.
# Données sous licence CC BY 4.0 ; documentation dans dataset/heart-disease.names.
#
# Exécution dans une session R ouverte à la racine du projet :
# source("R/import.R", encoding = "UTF-8")
# Aucun package supplémentaire nécessaire.
# Ce script ne supprime ni ligne ni variable et n'impute aucune valeur.

# 1. Sélection explicite : une seule version par source -------------------
# Ne pas importer tous les fichiers .data : plusieurs sont des variantes
# des mêmes observations. Le fichier brut cleveland.data est signalé corrompu.
fichiers <- c(
  cleveland   = "processed.cleveland.data",
  hungarian   = "processed.hungarian.data",
  switzerland = "processed.switzerland.data",
  va          = "processed.va.data"  # VA Medical Center, Long Beach
)

# Ordre des 14 colonnes fourni par UCI : 13 variables et la cible num.
colonnes <- c(
  "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
  "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num"
)

# Les codes numériques sont conservés pour ce premier diagnostic.
# Attention : sex, cp, fbs, restecg, exang, slope et thal sont des catégories,
# même si leur stockage est numérique. Ne pas les traiter automatiquement
# comme des mesures continues dans les analyses ultérieures.
# num est la cible d'origine (0 à 4) : aucun regroupement binaire ici.

# 2. Importation de chaque fichier ---------------------------------------
importer_source <- function(provenance, dossier = "dataset") {
  chemin <- file.path(dossier, fichiers[[provenance]])
  if (!file.exists(chemin)) {
    stop("Fichier introuvable : ", chemin,
         "\nOuvrir R à la racine du projet. Dossier actuel : ", getwd())
  }

  donnees <- read.table(
    file = chemin,
    header = FALSE,              # Les fichiers n'ont pas d'en-tête.
    sep = ",", dec = ".",
    na.strings = "?",           # Marqueur manquant des fichiers processed.
    colClasses = "numeric",     # Une valeur inattendue provoque une erreur.
    strip.white = TRUE,
    comment.char = "", quote = "",
    fill = FALSE                # Ne pas compléter une ligne mal formée.
  )
  if (ncol(donnees) != length(colonnes)) {
    stop("Nombre de colonnes inattendu dans ", chemin)
  }
  names(donnees) <- colonnes
  donnees$provenance <- provenance
  donnees
}

donnees_par_source <- lapply(names(fichiers), importer_source)
names(donnees_par_source) <- names(fichiers)

# 3. Fusion verticale : empiler les lignes, pas joindre sur des mesures ---
heart <- do.call(rbind, donnees_par_source)
rownames(heart) <- NULL

# Ne pas remplacer les zéros par NA : certains sont des codes valides.
# Les valeurs suspectes non marquées par '?' restent à examiner séparément.
# La documentation des fichiers bruts mentionne -9 comme marqueur manquant ;
# on le signale s'il apparaît ici, sans décider silencieusement d'un recodage.
if (any(as.matrix(heart[colonnes]) == -9, na.rm = TRUE)) {
  warning("Présence de -9 : vérifier leur signification avant recodage.")
}

cat("\nDimensions du tableau fusionné :\n")
print(dim(heart))
cat("\nPremières lignes (head) :\n")
print(head(heart), row.names = FALSE)
cat("\nNombre de lignes par provenance :\n")
print(table(heart$provenance))
# head() montre Cleveland en premier car ses lignes sont empilées en premier.

# 4. Bilan global : nombre et pourcentage de NA par variable --------------
bilan_na <- function(donnees) {
  nb_na <- colSums(is.na(donnees[colonnes]))
  data.frame(
    variable = colonnes,
    n_observations = nrow(donnees),
    n_na = unname(nb_na),
    pct_na = 100 * unname(nb_na) / nrow(donnees),
    row.names = NULL
  )
}

na_global <- bilan_na(heart)

# 5. Bilan croisé variable x provenance ----------------------------------
# Pour chaque source, le dénominateur est SON nombre de lignes.
# Exemple : 10 NA / 200 observations = 5 %, et non 10 / 920.
na_par_provenance <- do.call(rbind, lapply(names(fichiers), function(src) {
  bilan <- bilan_na(donnees_par_source[[src]])
  data.frame(provenance = src, bilan, row.names = NULL)
}))
rownames(na_par_provenance) <- NULL

# Résumé complémentaire par source : distinguer cellules et lignes.
na_sources <- do.call(rbind, lapply(names(fichiers), function(src) {
  d <- donnees_par_source[[src]][colonnes]
  masque <- is.na(d)
  data.frame(
    provenance = src,
    n_observations = nrow(d),
    n_cellules = nrow(d) * ncol(d),
    n_na = sum(masque),
    pct_cellules_na = 100 * mean(masque),
    n_lignes_avec_na = sum(rowSums(masque) > 0),
    pct_lignes_avec_na = 100 * mean(rowSums(masque) > 0),
    n_lignes_completes = sum(rowSums(masque) == 0)
  )
}))
rownames(na_sources) <- NULL

# Arrondir seulement l'affichage : conserver la précision dans les objets.
afficher_bilan <- function(x) {
  colonnes_pct <- grepl("^pct_", names(x))
  x[colonnes_pct] <- lapply(x[colonnes_pct], round, digits = 2)
  print(x, row.names = FALSE)
}

cat("\nNA par variable, toutes provenances réunies :\n")
afficher_bilan(na_global)
cat("\nNA par variable ET provenance :\n")
afficher_bilan(na_par_provenance)
cat("\nRésumé des NA par provenance :\n")
afficher_bilan(na_sources)

# Objets réutilisables ensuite dans Shiny :
# heart : tableau fusionné ; donnees_par_source : liste des quatre tableaux.
# na_global, na_par_provenance, na_sources : diagnostics pour tables/graphes.
# provenance est une information de source, pas une mesure clinique.
# Les NA observés ne suffisent pas à identifier leur mécanisme (MCAR/MAR/MNAR).
# Prochaine étape : choisir les graphiques, puis discuter du traitement.

