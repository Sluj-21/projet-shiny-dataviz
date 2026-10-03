SHINY — HEART DISEASE, VERSION NETTOYÉE

Remplacer ensemble ui.R, server.R et R/import.R.
Garder dataset/ à la racine avec les quatre fichiers processed fournis.
Depuis une session R ouverte à la racine :
install.packages(c("shiny", "ggplot2", "DT"))
shiny::runApp(".")

NETTOYAGE
- Comparer les 14 variables originales avant tout recodage.
- Conserver la première occurrence de chaque profil dans sa provenance.
- Deux occurrences retirées : une en Hongrie, une à VA Long Beach.
- 920 lignes initiales, 918 après dédoublonnage.
- Zéros de cholesterol et pa_repos transformés en NA : 172 + 1 cellules.
- Les autres zéros ne sont pas transformés. Aucune imputation.
- Les fichiers sources restent inchangés.
- heart_brut, heart_avant_recodage, doublons_supprimes et journal_recodage
  permettent de retrouver les transformations dans R/import.R.
- Les analyses et les pourcentages utilisent les données nettoyées.
- Le diagnostic original est conservé ; le serveur crée la cible binaire.

BIBLIOGRAPHIE
Janosi A., Steinbrunn W., Pfisterer M., Detrano R. (1989).
Heart Disease. UCI Machine Learning Repository. DOI 10.24432/C52P4X.
https://doi.org/10.24432/C52P4X — CC BY 4.0.
National Library of Medicine. MedlinePlus. Cholesterol.
https://medlineplus.gov/cholesterol.html
National Heart, Lung, and Blood Institute. Low Blood Pressure.
https://www.nhlbi.nih.gov/health/low-blood-pressure
Pages consultées le 3 octobre 2026.
Le contexte physiologique motive le choix de recoder zéro ; ces références
ne documentent pas zéro comme code officiel de donnée manquante chez UCI.

VALIDATION
Effectifs et recodages recalculés indépendamment sur les fichiers sources.
Concordance des sorties UI/serveur et délimiteurs R vérifiés statiquement.
R absent de l'environnement de création : exécution et rendu Shiny non testés.
