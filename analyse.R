# La commandite : ce que valait le droit des associés commandités de Rubis
# Sources : cours Yahoo Finance (RUI.PA, ^FCHI, ML.PA, RMS.PA), statuts de Rubis du 8 juin 2023 (art. 56),
#           communiqués Rubis du 19/10/2026 et du 18/11/2026, résultats annuels 2025.
# Lancer depuis le dossier du projet : Rscript analyse.R

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(scales); library(jsonlite)
})

brut <- "donnees/brut/"
dir.create("sorties", showWarnings = FALSE)
res <- file("sorties/resultats.txt", "w", encoding = "UTF-8")
note <- function(...) { cat(sprintf(...), "\n", file = res); cat(sprintf(...), "\n") }

papier <- "white"; encre <- "#1B2A3A"; vert <- "#2E8B57"; bleu <- "#1F5A8C"; brume <- "#B8C7D6"
theme_set(theme_minimal(base_size = 12, base_family = "Calibri") +
  theme(plot.background  = element_rect(fill = papier, colour = NA),
        panel.background = element_rect(fill = papier, colour = NA),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.title    = element_text(face = "bold", colour = encre, size = 16, margin = margin(b = 4)),
        plot.subtitle = element_text(colour = "#4A5568", margin = margin(b = 12)),
        plot.caption  = element_text(colour = "#6B7280", size = 8, hjust = 0, margin = margin(t = 10)),
        axis.text     = element_text(colour = "#4A5568"),
        panel.grid.major.y = element_line(colour = "#E3E8EE"),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none", plot.margin = margin(16, 20, 12, 16)))
enregistrer <- function(g, fichier, l = 8, h = 5) ggsave(file.path("sorties", fichier), g, width = l, height = h, dpi = 300)

# ---------------------------------------------------------------------------
# 1. Cours quotidiens (ajustés des divisions d'actions, pas des dividendes) et dividendes
# ---------------------------------------------------------------------------
lire_yahoo <- function(f) {
  d <- fromJSON(paste0(brut, f), simplifyVector = FALSE)$chart$result[[1]]
  q <- d$indicators$quote[[1]]
  num <- function(x) sapply(x, function(v) if (is.null(v)) NA_real_ else v)
  cours <- data.frame(date = as.Date(as.POSIXct(num(d$timestamp), origin = "1970-01-01")),
                      ouverture = num(q$open), cloture = num(q$close)) |> filter(!is.na(cloture))
  dv <- d$events$dividends
  div <- if (is.null(dv)) data.frame(date = as.Date(character()), montant = numeric()) else
    data.frame(date = as.Date(as.POSIXct(sapply(dv, `[[`, "date"), origin = "1970-01-01")),
               montant = sapply(dv, `[[`, "amount"))
  list(cours = cours, div = div)
}
rubis <- lire_yahoo("RUI.PA.json")
rubis$cours$annee <- as.integer(format(rubis$cours$date, "%Y"))
rubis$div$annee   <- as.integer(format(rubis$div$date, "%Y"))

# Moyenne des cours d'ouverture des 20 dernières séances de chaque exercice (art. 56)
fin_annee <- rubis$cours |> group_by(annee) |> arrange(date) |>
  summarise(p20 = mean(tail(ouverture, 20), na.rm = TRUE), n = n()) |> filter(n > 200)
div_an <- rubis$div |> group_by(annee) |> summarise(d = sum(montant))
an <- left_join(fin_annee, div_an, by = "annee") |> mutate(d = coalesce(d, 0))

# ---------------------------------------------------------------------------
# 2. La formule de l'article 56, appliquée par action, exercice par exercice
#    Performance = moyenne de fin d'exercice - Cours de référence (plus haut des 3 exercices précédents)
#                  + dividendes versés depuis l'exercice du Cours de référence
# ---------------------------------------------------------------------------
perf_par_action <- function(p20, d, annees) {
  sapply(seq_along(p20), function(i) {
    if (i < 4) return(NA_real_)
    j <- (i - 3):(i - 1); ref <- j[which.max(p20[j])]
    p20[i] - p20[ref] + sum(d[(ref + 1):i])
  })
}
an$perf <- perf_par_action(an$p20, an$d, an$annee)

# Vérification sur les deux exercices connus (France Épargne, d'après les comptes de Rubis)
N <- 103.41e6
for (a in c(2024, 2025)) {
  x <- an[an$annee == a, ]
  note("Exercice %d : performance %.2f €/action, 3 %% x performance x %.1f M actions = %.1f M€ avant plafond",
       a, x$perf, N / 1e6, 0.03 * max(x$perf, 0) * N / 1e6)
}
note("Montants publiés : 11,3 M€ pour 2024 ; 30,9 M€ pour 2025 (plafond de 10 %% du résultat net de 308,8 M€)")
pbg_officielle <- c(`2024` = 11278793.27 / 0.03, `2025` = 1478956862.05)   # DEU 2025, section 6.3.2
for (a in names(pbg_officielle)) note("PBG %s : officielle %.0f M€, recalculée %.0f M€", a,
  pbg_officielle[[a]] / 1e6, an$perf[an$annee == as.integer(a)] * N / 1e6)
note("Le DEU 2025 indique aussi qu'aucun dividende n'a été dû aux commandités au titre de 2020 à 2023 ; recalcul : %s",
     paste(sprintf("%d %+.1f €", an$annee[an$annee %in% 2020:2023], an$perf[an$annee %in% 2020:2023]), collapse = ", "))
note("Exercices 2003-2025 où la performance est positive : %d sur %d",
     sum(an$perf > 0, na.rm = TRUE), sum(!is.na(an$perf)))

# ---------------------------------------------------------------------------
# 3. Valeur du droit des commandités par simulation (Monte-Carlo)
# ---------------------------------------------------------------------------
cours_actuel <- tail(rubis$cours$cloture, 1)
rendement_div <- 2.07 / cours_actuel
r_an <- diff(log(an$p20 + 0))            # variations annuelles des cours de fin d'exercice
sigma_hist <- sd(r_an[an$annee[-1] >= 2006])
note("Cours actuel %.2f € ; rendement du dividende %.1f %% ; volatilité annuelle historique (2006-2025) %.0f %%",
     cours_actuel, 100 * rendement_div, 100 * sigma_hist)

valeur_droit <- function(k = 0.08, sigma = sigma_hist, g = 0.02, q = rendement_div, horizon = 60,
                         n_sim = 20000, rn0 = 308.8e6, graine = 1, inclure_ref = FALSE) {
  set.seed(graine)
  hist_p <- tail(an$p20, 3); hist_d <- tail(an$d, 3)               # exercices 2023, 2024, 2025
  p <- matrix(NA_real_, n_sim, horizon + 3); dd <- p
  p[, 1:3] <- rep(hist_p, each = n_sim); dd[, 1:3] <- rep(hist_d, each = n_sim)
  derive <- log(1 + k) - q - sigma^2 / 2                              # rendement total espéré = k
  prec <- matrix(cours_actuel, n_sim)
  vact <- numeric(n_sim)
  for (t in 1:horizon) {
    i <- t + 3
    prec <- if (t == 1) prec else p[, i - 1]
    p[, i] <- if (t == 1) cours_actuel else prec * exp(derive + sigma * rnorm(n_sim))
    dd[, i] <- q * prec
    ref_pos <- max.col(p[, (i - 3):(i - 1)], ties.method = "first") + (i - 4)
    cum_d <- sapply(seq_len(n_sim), function(s) sum(dd[s, (ref_pos[s] + !inclure_ref):i]))
    perf <- p[, i] - p[cbind(seq_len(n_sim), ref_pos)] + cum_d
    plafond <- 0.10 * rn0 * (1 + g)^t
    paie <- pmin(0.03 * pmax(perf, 0) * N, plafond)
    vact <- vact + paie / (1 + k)^t
  }
  vact
}

v <- valeur_droit()
compens <- c(initiale = 12719328, revue = 11375150) * cours_actuel
note("Valeur du droit (k = 8 %%, g = 2 %%) : moyenne %.0f M€, médiane %.0f M€, intervalle 10-90 %% : %.0f à %.0f M€",
     mean(v) / 1e6, median(v) / 1e6, quantile(v, .1) / 1e6, quantile(v, .9) / 1e6)
note("Compensation en actions au cours actuel : %.0f M€ (19/10) puis %.0f M€ (18/11)",
     compens[1] / 1e6, compens[2] / 1e6)

v2 <- valeur_droit(inclure_ref = TRUE)
note("Variante où le dividende de l'exercice de référence est compté (2024 : 14,5 M€ au lieu de 8,9) : moyenne %.0f M€", mean(v2) / 1e6)
borne <- 0.10 * 308.8e6 * 1.02 / (0.08 - 0.02)
note("Borne haute, si le plafond de 10 %% du résultat était atteint chaque année (k = 8 %%, g = 2 %%) : %.0f M€", borne / 1e6)
sens <- expand.grid(k = c(0.06, 0.07, 0.08, 0.09, 0.10), g = c(0, 0.02, 0.04))
sens$valeur <- mapply(function(k, g) mean(valeur_droit(k = k, g = g, n_sim = 5000)) / 1e6, sens$k, sens$g)
note("Sensibilité (M€) :"); for (i in seq_len(nrow(sens))) note("  k = %.0f %%, g = %.0f %% : %.0f", 100 * sens$k[i], 100 * sens$g[i], sens$valeur[i])
write.csv(sens, "sorties/sensibilite.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 4. Graphiques
# ---------------------------------------------------------------------------
source_cap <- "Sources : cours Yahoo Finance, statuts de Rubis (2023), communiqués de Rubis (2026), calculs de l'auteur."

g1 <- an |> filter(!is.na(perf)) |> mutate(signe = perf > 0) |>
  ggplot(aes(annee, perf, fill = signe)) + geom_col(width = 0.7) +
  geom_hline(yintercept = 0, colour = encre, linewidth = 0.3) +
  scale_fill_manual(values = c(`TRUE` = vert, `FALSE` = brume)) +
  scale_y_continuous(labels = label_number(suffix = " €", decimal.mark = ",")) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  labs(title = "La performance boursière de Rubis a été positive quinze années sur vingt-quatre",
       subtitle = "Performance boursière par action au sens de l'article 56 des statuts de Rubis, en euros",
       x = NULL, y = NULL,
       caption = paste0(source_cap, "
Moyenne des ouvertures des vingt dernières séances, comparée au plus haut des trois exercices précédents, dividendes inclus."))
enregistrer(g1, "1_performance_commandites.png")

g2 <- ggplot(data.frame(v = v / 1e6), aes(v)) +
  geom_histogram(bins = 60, fill = bleu, colour = papier, linewidth = 0.1) +
  geom_vline(xintercept = mean(v) / 1e6, colour = encre, linetype = "dashed") +
  geom_vline(xintercept = compens / 1e6, colour = vert, linewidth = 0.8) +
  annotate("text", x = mean(v) / 1e6 + 8, y = Inf, vjust = 1.5, hjust = 0, colour = encre, family = "Calibri",
           label = sprintf("valeur moyenne du droit\n%.0f M€", mean(v) / 1e6)) +
  annotate("text", x = compens[2] / 1e6 - 8, y = Inf, vjust = 1.5, hjust = 1, colour = vert, family = "Calibri",
           label = sprintf("indemnité proposée\n%.0f à %.0f M€", compens[2] / 1e6, compens[1] / 1e6)) +
  scale_x_continuous(labels = label_number(suffix = " M€"), limits = c(0, 520)) +
  scale_y_continuous(labels = NULL) +
  labs(title = "L'indemnité proposée vaut plus de trois fois le droit qu'elle rachète",
       subtitle = "Valeur actuelle du dividende statutaire des commandités sur 20 000 trajectoires simulées du cours de Rubis",
       x = NULL, y = NULL,
       caption = paste0(source_cap, "
Rendement attendu 8 %, croissance du résultat 2 %, volatilité historique, horizon de soixante ans."))
enregistrer(g2, "2_valeur_du_droit.png")

indice <- function(f, nom) { x <- lire_yahoo(f)$cours; data.frame(date = x$date, nom = nom, cloture = x$cloture) }
comp <- bind_rows(indice("RUI.PA.json", "Rubis"), indice("ML.PA.json", "Michelin"),
                  indice("RMS.PA.json", "Hermès"), indice("FCHI.json", "CAC 40"))
debut <- as.Date(max(tapply(comp$date, comp$nom, min)))
comp <- comp |> filter(date >= debut) |> group_by(nom) |> arrange(date) |>
  mutate(base = 100 * cloture / first(cloture)) |> ungroup()
fin <- comp |> group_by(nom) |> slice_max(date, n = 1)
for (i in seq_len(nrow(fin))) note("Indice de prix base 100 au %s : %s = %.0f", format(debut), fin$nom[i], fin$base[i])
g3 <- ggplot(comp, aes(date, base, colour = nom)) + geom_line(linewidth = 0.5) +
  geom_text(data = fin, aes(label = nom), hjust = -0.1, family = "Calibri", size = 3.8) +
  scale_colour_manual(values = c(Rubis = bleu, Hermès = vert, Michelin = "#C08A2E", `CAC 40` = "#6B7280")) +
  scale_y_log10(labels = label_number(big.mark = " ")) +
  scale_x_date(breaks = as.Date(paste0(seq(2000, 2025, 5), "-01-01")), date_labels = "%Y", expand = expansion(mult = c(0.01, 0.12))) +
  labs(title = "Rubis, Michelin et Hermès comparés au CAC 40 depuis 2000",
       subtitle = sprintf("Indice des cours hors dividendes, base 100 au %s, échelle logarithmique", format(debut, "%d/%m/%Y")),
       x = NULL, y = NULL, caption = "Sources : cours Yahoo Finance, calculs de l'auteur. Les dividendes ne sont pas réinvestis, ce qui désavantage Rubis, dont le rendement est le plus élevé.")
enregistrer(g3, "3_commandites_cac40.png")

# ---------------------------------------------------------------------------
# 5. Une commandite type pour la terre
#    Actif : l'écart de flux entre un hectare de blé et de noyers et un hectare de blé seul (projet 07,
#    07-terre/analyse.R, blé année normale, avec aides). Les commanditaires financent les années négatives
#    (plantation, perte de récolte sous les arbres) ; l'exploitant commandité conduit les arbres pendant 45 ans.
#    Cascade : les commanditaires récupèrent d'abord leurs apports capitalisés à un taux préférentiel,
#    le surplus est ensuite partagé trois quarts / un quart, comme dans la commenda unilatérale médiévale.
# ---------------------------------------------------------------------------
ha <- list(duree = 45, produit = 1500, charges = 1600, part_intr = 0.31, plant = 1750, aide_plant = 0.70,
           entretien = 25, emprise = 0.05, perte_fin = 0.20, debut_perte = 15, bois_m3 = 40, plaq_m3 = 80,
           plaq_an = 22.5, plaq_net = 7.5)
ecart_ha <- function(prix_bois, produit = ha$produit) {
  t <- 0:ha$duree
  rdt <- ifelse(t <= ha$debut_perte, 1, 1 - ha$perte_fin * (t - ha$debut_perte) / (ha$duree - ha$debut_perte))
  perte_ble <- ifelse(t == 0, 0, produit * (1 - (1 - ha$emprise) * rdt) - ha$charges * ha$part_intr * ha$emprise)
  arbres <- ifelse(t == 0, -ha$plant * (1 - ha$aide_plant), -ha$entretien) +
    ifelse(t >= 20 & t < ha$duree, ha$plaq_an * ha$plaq_net, 0) +
    ifelse(t == ha$duree, ha$bois_m3 * prix_bois + ha$plaq_m3 * ha$plaq_net, 0)
  arbres - perte_ble
}
van <- function(x, r = 0.04) sum(x / (1 + r)^(seq_along(x) - 1))
tri <- function(x) tryCatch(uniroot(function(r) van(x, r), c(-0.5, 0.5))$root, error = function(e) NA_real_)
stopifnot(abs(van(ecart_ha(600)) - 2228) < 1)          # même résultat que le projet 07 (bois central, 600 €/m3)

cascade <- function(e, pref = 0.03, part_cmd = 0.25) {
  solde <- 0; cmt <- cmd <- numeric(length(e))
  for (i in seq_along(e)) {
    solde <- solde * (1 + pref)
    if (e[i] < 0) { cmt[i] <- e[i]; solde <- solde - e[i]; next }
    rembourse <- min(e[i], solde); solde <- solde - rembourse
    cmt[i] <- rembourse + (1 - part_cmd) * (e[i] - rembourse)
    cmd[i] <- part_cmd * (e[i] - rembourse)
  }
  data.frame(t = seq_along(e) - 1, ecart = e, commanditaires = cmt, commandites = cmd)
}
centre <- cascade(ecart_ha(600))
note("Commandite agroforestière, bois à 600 €/m3 : TRI de l'écart %.1f %%, TRI des commanditaires %.1f %%, apport maximal %.0f €/ha",
     100 * tri(centre$ecart), 100 * tri(centre$commanditaires), -min(cumsum(centre$commanditaires)))
note("Part des commandités : %.0f €/ha à la coupe, soit %.0f € actualisés à 4 %% ; pour 1 000 ha, apport des commanditaires %.1f M€",
     sum(centre$commandites), van(centre$commandites), -min(cumsum(centre$commanditaires)) * 1000 / 1e6)
for (p in c(300, 1000)) { x <- cascade(ecart_ha(p))
  note("Bois à %d €/m3 : TRI des commanditaires %.1f %%, part des commandités %.0f €/ha", p, 100 * tri(x$commanditaires), sum(x$commandites)) }
x <- cascade(ecart_ha(600, produit = 1200))
note("Blé en crise (1 200 €/ha), bois à 600 : TRI des commanditaires %.1f %%", 100 * tri(x$commanditaires))

set.seed(2)
prix_sim <- 600 * exp(rnorm(5000, 0, 0.47))          # 10-90 % ≈ 330-1 100 €/m3, autour de la fourchette du projet 07
tri_sim <- sapply(prix_sim, function(p) tri(cascade(ecart_ha(p))$commanditaires))
note("Prix du bois incertain (médiane 600 €/m3) : TRI médian des commanditaires %.1f %%, 10-90 %% : %.1f à %.1f %%, probabilité de rester sous 3 %% : %.0f %%",
     100 * median(tri_sim), 100 * quantile(tri_sim, .1), 100 * quantile(tri_sim, .9), 100 * mean(tri_sim < 0.03))

cum <- centre |> transmute(t, Commanditaires = cumsum(commanditaires), Commandités = cumsum(commandites)) |>
  tidyr::pivot_longer(-t, names_to = "partie", values_to = "cumul")
g4 <- ggplot(cum, aes(t, cumul, colour = partie)) +
  geom_hline(yintercept = 0, colour = encre, linewidth = 0.3) + geom_step(linewidth = 0.9) +
  geom_text(data = filter(cum, t == max(t)), aes(label = partie), hjust = -0.1, family = "Calibri", size = 3.8) +
  scale_colour_manual(values = c(Commanditaires = bleu, Commandités = vert)) +
  scale_y_continuous(labels = label_number(suffix = " €", big.mark = " ")) +
  scale_x_continuous(breaks = seq(0, 45, 5), expand = expansion(mult = c(0.01, 0.15))) +
  labs(title = "Les commanditaires portent seuls l'attente de quarante-cinq ans, puis partagent la coupe",
       subtitle = "Trésorerie cumulée par hectare d'une commandite agroforestière, en euros constants non actualisés (bois à 600 €/m3)",
       x = "Années après la plantation", y = NULL,
       caption = paste0("Sources : flux du projet 07 (blé et noyers contre blé seul, année normale, avec aides), calculs de l'auteur.",
                        "\nCascade : apports rendus d'abord avec 3 % par an, puis surplus partagé trois quarts aux commanditaires, un quart à l'exploitant commandité."))
enregistrer(g4, "4_commandite_tresorerie.png", l = 9.5, h = 5.4)

g5 <- ggplot(data.frame(tri = 100 * tri_sim), aes(tri)) +
  geom_histogram(bins = 50, fill = bleu, colour = papier, linewidth = 0.1) +
  geom_vline(xintercept = 3, colour = vert, linewidth = 0.8) +
  annotate("text", x = 3.1, y = Inf, vjust = 1.5, hjust = 0, colour = vert, family = "Calibri", label = "taux préférentiel\n3 %") +
  scale_x_continuous(labels = label_number(suffix = " %", decimal.mark = ",")) + scale_y_continuous(labels = NULL) +
  labs(title = "Le rendement des commanditaires se joue sur le prix futur du bois",
       subtitle = "Taux de rendement interne des commanditaires sur 5 000 tirages du prix du bois de noyer",
       x = NULL, y = NULL,
       caption = "Sources : flux du projet 07, calculs de l'auteur. Prix du bois tiré autour d'une médiane de 600 €/m3 (10 % des tirages sous 330 €, 10 % au-dessus de 1 100 €).")
enregistrer(g5, "5_commandite_rendement.png", l = 9.5, h = 5.4)

close(res)
