# Serveur Shiny — dépend de R/import.R et des quatre fichiers processed.
# Packages à installer une fois : install.packages(c("shiny", "ggplot2", "DT"))
library(shiny)
library(ggplot2)

# Chargement une fois au démarrage, dans un environnement séparé.
# import.R peut imprimer un graphique : un périphérique temporaire sans fichier
# évite de créer Rplots.pdf. Les graphiques affichés dans Shiny sont rendus plus bas.
charger_import <- function() {
  if (!file.exists("R/import.R")) stop("R/import.R introuvable à la racine de l'application.")
  env <- new.env(parent = globalenv())
  grDevices::pdf(file = NULL)
  appareil <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(appareil), add = TRUE)
  sys.source("R/import.R", envir = env)
  env
}
import_uci <- charger_import()

# Copie pour l'application : préserver les objets produits par import.R.
donnees_uci <- import_uci$heart
if (!"diagnostic_initial" %in% names(donnees_uci)) {
  donnees_uci$diagnostic_initial <- donnees_uci$diagnostic
}
stopifnot(all(is.na(donnees_uci$diagnostic_initial) |
                donnees_uci$diagnostic_initial %in% 0:4))
donnees_uci$diagnostic <- ifelse(is.na(donnees_uci$diagnostic_initial),
                                NA_integer_, as.integer(donnees_uci$diagnostic_initial > 0))

# Proportions et intervalles de Wilson ; retourne aussi les catégories vides.
calcul_proportions <- function(d) {
  do.call(rbind, lapply(0:3, function(k) {
    y <- d$diagnostic[d$nb_vaisseaux == k]
    n <- length(y)
    if (!n) return(data.frame(nb_vaisseaux = k, n = 0, n_presence = 0,
                              proportion = NA_real_, ic_inf = NA_real_, ic_sup = NA_real_))
    p <- mean(y == 1)
    z <- qnorm(0.975)
    denom <- 1 + z^2 / n
    centre <- (p + z^2 / (2 * n)) / denom
    largeur <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
    data.frame(nb_vaisseaux = k, n = n, n_presence = sum(y == 1),
               proportion = p, ic_inf = centre - largeur, ic_sup = centre + largeur)
  }))
}

function(input, output, session) {
  output$indicateurs <- renderUI({
    b <- import_uci$bilan_nettoyage
    carte <- function(valeur, libelle) div(class = "metric", strong(valeur), span(libelle))
    div(class = "metric-row",
      carte(b$n_final, "observations après nettoyage"),
      carte(length(import_uci$fichiers), "centres de provenance"),
      carte(b$n_doublons_retires, "occurrences dupliquées retirées"),
      carte(b$n_zeros_recodes, "zéros recodés en NA")
    )
  })
  output$bilan_doublons <- renderText({
    b <- import_uci$bilan_nettoyage
    paste(b$n_doublons_retires, "occurrences retirées sur", b$n_brut,
          "lignes initiales ;", b$n_final,
          "observations conservées. Une ligne par paire est gardée. Les fichiers sources sont inchangés.")
  })
  output$journal_zeros <- DT::renderDT({
    DT::formatRound(DT::datatable(import_uci$journal_recodage, rownames = FALSE,
      options = list(scrollX = TRUE, dom = "t", pageLength = 8)), "pct_na_finaux", 2)
  })
  output$origine_na <- renderPlot({
    d <- import_uci$na_origine
    d$variable <- factor(d$variable, levels = rev(import_uci$colonnes))
    ggplot(d, aes(x = pct, y = variable, fill = origine)) +
      geom_col(width = .68) +
      scale_fill_manual(values = c("NA initiaux" = "#B8D4FA", "Zéros recodés" = "#165DDE")) +
      scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, " %")) +
      scale_y_discrete(labels = setNames(import_uci$dictionnaire$libelle,
                                        import_uci$dictionnaire$nom_fr)) +
      labs(x = "Pourcentage des observations après dédoublonnage", y = NULL, fill = NULL,
           caption = "Les deux contributions s'additionnent. Dénominateur commun : toutes les observations nettoyées.") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top", panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank())
  }, res = 110)
  # Les données sources restent communes en lecture ; les sorties sont par session.
  updateSelectInput(session, "source_apercu", choices = c(
    "Toutes" = "toutes", setNames(names(import_uci$fichiers), names(import_uci$fichiers))
  ))
  apercu <- reactive({
    req(input$source_apercu)
    if (input$source_apercu == "toutes") donnees_uci else
      donnees_uci[donnees_uci$provenance == input$source_apercu, ]
  })
  output$dimensions <- renderText({
    paste(nrow(apercu()), "observations —", ncol(apercu()),
          "colonnes affichables (diagnostic original inclus).")
  })
  output$apercu <- DT::renderDT({
    req(input$n_apercu)
    n <- max(1, min(100, as.integer(input$n_apercu)))
    DT::datatable(head(apercu(), n), rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 6))
  })
  output$effectifs <- renderTable({
    setNames(as.data.frame(table(apercu()$provenance)), c("Provenance", "Effectif"))
  })
  output$dictionnaire <- DT::renderDT({
    d <- import_uci$dictionnaire
    d$libelle[d$nom_fr == "diagnostic"] <- "Diagnostic binaire dans l'application ; code UCI conservé dans diagnostic_initial"
    DT::datatable(d, rownames = FALSE, options = list(pageLength = 14, dom = "t", scrollX = TRUE))
  })
  output$doublons <- DT::renderDT({
    DT::datatable(import_uci$doublons_details, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 6))
  })
  output$carte_na <- renderPlot({ import_uci$graphique_na_provenance() }, res = 110)
  output$na_global_ui <- renderTable(import_uci$na_global, digits = 2)
  output$na_sources_ui <- DT::renderDT({
    DT::formatRound(DT::datatable(import_uci$na_sources, rownames = FALSE,
      options = list(scrollX = TRUE, dom = "t")),
      c("pct_cellules_na", "pct_lignes_avec_na"), 2)
  })
  output$na_detail <- DT::renderDT({
    DT::formatRound(DT::datatable(import_uci$na_par_provenance, rownames = FALSE,
      options = list(pageLength = 14, scrollX = TRUE)), "pct_na", 2)
  })

  cleveland <- donnees_uci[donnees_uci$provenance == "cleveland", ]
  complet <- complete.cases(cleveland[c("nb_vaisseaux", "diagnostic")])
  cleveland_analyse <- cleveland[complet, ]
  resume <- calcul_proportions(cleveland_analyse)
  output$bilan_cleveland <- renderTable({
    data.frame(Total = nrow(cleveland),
      NA_vaisseaux = sum(is.na(cleveland$nb_vaisseaux)),
      NA_diagnostic = sum(is.na(cleveland$diagnostic)),
      Utilisables_pour_le_graphique = sum(complet))
  })
  output$croisement <- renderTable({
    as.data.frame.matrix(table(
      Nb_vaisseaux = factor(cleveland$nb_vaisseaux, levels = 0:3),
      Diagnostic = factor(cleveland$diagnostic, levels = 0:1,
                          labels = c("Absence", "Présence")), useNA = "ifany"))
  }, rownames = TRUE)
  output$proportions <- renderPlot({
    validate(need(nrow(cleveland_analyse) > 0, "Aucune observation exploitable."))
    ggplot(resume, aes(nb_vaisseaux, proportion)) +
      geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = .12,
                    colour = "#165DDE", na.rm = TRUE) +
      geom_point(size = 3.5, colour = "#165DDE", na.rm = TRUE) +
      scale_x_continuous(breaks = 0:3, labels = paste0(0:3, "\n(n = ", resume$n, ")")) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2),
                         labels = function(x) paste0(round(100*x), " %")) +
      labs(title = "Proportion de diagnostics positifs à Cleveland",
           x = "Nombre de vaisseaux visualisés", y = "Proportion de diagnostics positifs",
           caption = paste(sum(!complet), "observation(s) exclue(s) pour valeur manquante. IC de Wilson à 95 %.")) +
      theme_minimal(base_size = 12)
  }, res = 110)
  output$table_proportions <- renderTable({
    data.frame(Vaisseaux = resume$nb_vaisseaux, Effectif = resume$n,
      Diagnostics_positifs = resume$n_presence, Pourcentage = 100*resume$proportion,
      IC95_inf = 100*resume$ic_inf, IC95_sup = 100*resume$ic_sup)
  }, digits = 1)

  # Calcul explicite sur bouton pour séparer description et modélisation.
  modeles <- eventReactive(input$calcul_modeles, {
    tryCatch({
      variables <- c("diagnostic", "age", "sexe", "type_doul_thor", "pa_repos",
        "cholesterol", "glyc_jeun_elevee", "ecg_repos", "fc_max", "angine_effort",
        "depress_st", "pente_st", "test_thallium", "nb_vaisseaux")
      d <- cleveland[variables]
      d <- d[complete.cases(d), ]
      if (length(unique(d$diagnostic)) != 2) stop("Les deux diagnostics doivent être représentés.")
      cats <- c("sexe", "type_doul_thor", "glyc_jeun_elevee", "ecg_repos",
                "angine_effort", "pente_st", "test_thallium", "nb_vaisseaux")
      d[cats] <- lapply(d[cats], factor)
      if (any(vapply(d[cats], nlevels, integer(1)) < 2)) stop("Une catégorie n'a qu'une modalité observée.")
      d$nb_vaisseaux <- relevel(d$nb_vaisseaux, "0")
      avertissements <- character()
      resultat <- withCallingHandlers({
        reduit <- glm(diagnostic ~ age + sexe + type_doul_thor + pa_repos +
          cholesterol + glyc_jeun_elevee + ecg_repos + fc_max + angine_effort +
          depress_st + pente_st + test_thallium, family = binomial(), data = d)
        entier <- update(reduit, . ~ . + nb_vaisseaux)
        if (!reduit$converged || !entier$converged ||
            anyNA(coef(reduit)) || anyNA(coef(entier))) {
          stop("Ajustement instable ou coefficients non identifiables : examiner les catégories et les effectifs.")
        }
        capture.output({
          cat("Cas complets utilisés :", nrow(d), "sur", nrow(cleveland), "\n\n")
          cat("Diagnostics dans l'échantillon du modèle :\n")
          print(table(d$diagnostic))
          cat("\nTest global de l'ajout de nb_vaisseaux :\n")
          print(anova(reduit, entier, test = "LRT"))
          cat("\nAIC (plus faible = compromis ajustement/complexité plus favorable) :\n")
          print(AIC(reduit, entier))
        })
      }, warning = function(w) {
        avertissements <<- c(avertissements, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
      if (length(avertissements)) {
        resultat <- c("AVERTISSEMENTS : interprétation à vérifier avant conclusion.",
                      unique(avertissements), "", resultat)
      }
      paste(resultat, collapse = "\n")
    }, error = function(e) paste("Modélisation non disponible :", conditionMessage(e)))
  })
  output$resultat_modeles <- renderText({ req(input$calcul_modeles > 0); modeles() })
}
