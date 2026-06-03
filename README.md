Exploration des données: 
 # on importe la bibliotheque de données 
install.packages("tidyverse")
library(tidyverse)
donnees_brutes <- read_csv("IRVE.csv")
str(donnees_brutes)
summary(donnees_brutes)
head(donnees_brutes)
names(donnees_brutes)
str(donnees_brutes)
names(donnees_brutes)
donnees_brutes <- donnees_brutes %>% select(nom_amenageur,nom_operateur,nom_station,implantation_station,adresse_station,nbre_pdc,puissance_nominale,prise_type_ef,prise_type_2,prise_type_combo_ccs,prise_type_chademo,gratuit,paiement_acte,paiement_cb,tarification,condition_acces,date_mise_en_service,consolidated_longitude,consolidated_latitude,consolidated_commune,consolidated_code_postal)
view(donnees_brutes)
#Statistique descriptive
na_summary <- data.frame(colonne=names(donnees_brutes),nb_na = colSums(is.na(donnees_brutes)),pct_na = round(colSums(is.na(donnees_brutes)) / nrow(donnees_brutes) *100, 1)) %>% filter(nb_na >0) %>% arrange(desc(pct_na))
print(na_summary)
cat("===Puissance nominale===\n")
summary(donnees_brutes_clean$puissance_nominale)
summary(donnees_brutes$puissance_nominale)
cat("Ecart-type:",round(sd(donnees_brutes$puissance_nominale),2),"\n")
quantile(donnees_brutes$puissance_nominale, probs = c(0.25, 0.5, 0.75))
donnees_brutes <- donnees_brutes %>% mutate(cat_puissance = case_when(puissance_nominale <22 ~ "Lente (<22kw)",puissance_nominale < 50 ~ "Normale(22-49 kw)",puissance_nominale < 150 ~ "Rapide(50-149 kw", TRUE ~ "Ultra-rapide (>= 150 kw)"))
table(donnees_brutes$cat_puissance)
cat("=== Nombre de points de charge par station ===\n")
summary(donnees_brutes$nbre_pdc)
freq_implantation <- donnees_brutes %>% count(implantation_station, sort = TRUE) %>% mutate(pct=round(n/sum(n)*100,1))
print(freq_implantation)
freq_prise <- donnees_brutes %>% count(prise_type_ef,prise_type_2,prise_type_chademo,prise_type_combo_ccs, sort = TRUE) %>% mutate(pct=round(n/sum(n)*100,1))
print(freq_prise)
top10_op <- donnees_brutes %>% count(nom_operateur, sort=TRUE) %>% top_n(10,n) %>% mutate(pct=round(n/sum(n)*100, 1))
print(top10_op)
donnees_brutes %>% summarise(nb_stations=n(),puiss_moyenne=round(mean(puissance_nominale),1),puiss_mediane=round(median(puissance_nominale),1),nb_pdc_moyen=round(mean(nbre_pdc),1)) %>% arrange(desc(nb_stations)) %>% print()
#nettoyage
donnees_brutes <- donnees_brutes %>% mutate(
  #numerique
  puissance_nominale =as.numeric(puissance_nominale),
  nbre_pdc =as.integer(nbre_pdc),
  consolidated_longitude =as.numeric(consolidated_longitude),
  consolidated_latitude =as.numeric(consolidated_latitude),
  
  #date
  date_mise_en_service = as.Date(date_mise_en_service, format = "%Y-%m-%d"),
  
  #booléens -> logique
  prise_type_ef = as.logical(prise_type_ef),
  prise_type_2           = as.logical(prise_type_2),
  prise_type_combo_ccs   = as.logical(prise_type_combo_ccs),
  prise_type_chademo     = as.logical(prise_type_chademo),
  gratuit                = as.logical(gratuit),
  paiement_acte          = as.logical(paiement_acte),
  paiement_cb            = as.logical(paiement_cb)
)
donnees_brutes <- donnees_brutes %>%
  mutate(implantation_station = iconv(implantation_station,
                                      from = "latin1", to = "UTF-8",
                                      sub = "byte"))

# Vérifier les modalités
table(donnees_brutes$implantation_station)

# Corriger les valeurs corrompues (si nécessaire)
donnees_brutes <- donnees_brutes %>%
  mutate(implantation_station = case_when(
    grepl("Parking priv", implantation_station) &
      grepl("usage public", implantation_station) ~ "Parking privé à usage public",
    grepl("Parking priv", implantation_station) &
      grepl("client", implantation_station)       ~ "Parking privé réservé à la clientèle",
    TRUE ~ implantation_station
  ))
#supprimer les trops petites ou trop grandes valeurs 
cat("Puissances avant nettoyage :\n")
summary(donnees_brutes$puissance_nominale)

# Valeurs absurdes (0, 0.0001, etc.)
cat("Nb lignes avec puissance <= 0 :", sum(donnees_brutes$puissance_nominale <= 0, na.rm = TRUE), "\n")

donnees_brutes_clean <- donnees_brutes %>%
  filter(puissance_nominale > 0 & puissance_nominale <= 400)

cat("Nb lignes après filtre puissance :", nrow(donnees_brutes_clean), "\n")

# Supprimer les doublons
# On garde les lignes uniques sur toutes les colonnes sélectionnées
nb_avant <- nrow(donnees_brutes_clean)
donnees_brutes_clean <- donnees_brutes_clean %>% distinct()
cat("Doublons supprimés :", nb_avant - nrow(donnees_brutes_clean), "\n")

#Traiter les NA des colonnes importantes

# nom_operateur : remplacer NA par "Inconnu"
donnees_brutes_clean <- donnees_brutes_clean %>%
  mutate(nom_operateur = ifelse(is.na(nom_operateur), "Inconnu", nom_operateur))

# consolidated_commune : remplacer NA par "Non renseignée"
donnees_brutes_clean <- donnees_brutes_clean %>%
  mutate(consolidated_commune = ifelse(is.na(consolidated_commune),
                                       "Non renseignée",
                                       consolidated_commune))

# tarification : colonne très incomplète (74% NA), on l'utilise prudemment
# On la garde telle quelle pour l'instant

# Extraire année et mois de la date 
donnees_brutes_clean <- donnees_brutes_clean %>%
  mutate(
    annee = year(date_mise_en_service),
    mois  = month(date_mise_en_service, label = TRUE)
  )

# Créer une colonne type_prise lisible 
donnees_brutes_clean <- donnees_brutes_clean %>%
  mutate(
    type_prise = case_when(
      prise_type_combo_ccs ~ "Combo CCS",
      prise_type_chademo   ~ "CHAdeMO",
      prise_type_2         ~ "Type 2",
      prise_type_ef        ~ "Type EF",
      TRUE                 ~ "Autre"
    )
  )

# Bilan du nettoyage
cat("=== BILAN NETTOYAGE ===\n")
cat("Lignes initiales   :", nrow(donnees_brutes), "\n")
cat("Lignes après nett. :", nrow(donnees_brutes_clean), "\n")
cat("Lignes supprimées  :", nrow(donnees_brutes) - nrow(donnees_brutes_clean), "\n")
cat("Colonnes           :", ncol(donnees_brutes_clean), "\n")

# VISUALISATION GRAPHIQUE 

#  Histogramme : répartition des puissances
p1 <- ggplot(donnees_brutes_clean, aes(x = puissance_nominale)) +
  geom_histogram(bins = 40, fill = "#2B4EA8", color = "white", alpha = 0.85) +
  scale_x_continuous(breaks = c(0, 22, 50, 100, 150, 250, 350, 400)) +
  labs(
    title    = "Distribution des puissances nominales (IRVE)",
    subtitle = paste0("n = ", format(nrow(donnees_brutes_clean), big.mark = " "), " points de charge"),
    x        = "Puissance nominale (kW)",
    y        = "Nombre de points de charge"
  ) +
  theme_minimal(base_size = 12)

print(p1)
ggsave("histogramme_puissance.png", plot = p1, width = 10, height = 6, dpi = 300)

# Histogramme : répartition des types de prise
p2 <- ggplot(donnees_brutes_clean, aes(x = fct_infreq(type_prise), fill = type_prise)) +
  geom_bar(alpha = 0.9, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Répartition des types de prise",
    x     = "Type de prise",
    y     = "Nombre de points de charge"
  ) +
  theme_minimal(base_size = 12)

print(p2)
ggsave("repartition_types_prise.png", plot = p2, width = 9, height = 6, dpi = 300)

# Évolution du nombre de stations par année
stations_par_annee <- donnees_brutes_clean %>%
  filter(!is.na(annee), annee >= 2010, annee <= 2025) %>%
  count(annee)

p3 <- ggplot(stations_par_annee, aes(x = annee, y = n)) +
  geom_area(fill = "#2B4EA8", alpha = 0.2) +
  geom_line(color = "#2B4EA8", size = 1.3) +
  geom_point(color = "#CC0000", size = 3) +
  scale_x_continuous(breaks = 2010:2025) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Évolution du nombre de PDC mis en service par année",
    subtitle = "Source : IRVE",
    x        = "Année",
    y        = "Nombre de PDC"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)
ggsave("evolution_par_annee.png", plot = p3, width = 12, height = 6, dpi = 300)

# Évolution par mois (pour une année donnée)
stations_2023 <- donnees_brutes_clean %>%
  filter(annee == 2023, !is.na(mois)) %>%
  count(mois)

p4 <- ggplot(stations_2023, aes(x = mois, y = n, group = 1)) +
  geom_col(fill = "#E8A020", alpha = 0.85) +
  labs(
    title = "Mises en service mensuelles en 2023",
    x     = "Mois",
    y     = "Nombre de PDC"
  ) +
  theme_minimal(base_size = 12)

print(p4)
ggsave("evolution_mensuelle_2023.png", plot = p4, width = 10, height = 5, dpi = 300)

# Top 10 opérateurs
p5 <- ggplot(top10_op, aes(x = reorder(nom_operateur, n), y = n)) +
  geom_col(fill = "#CC0000", alpha = 0.85) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.1, size = 3.5, color = "black") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 10 des opérateurs IRVE",
    x     = NULL,
    y     = "Nombre de points de charge"
  ) +
  theme_minimal(base_size = 12)

print(p5)
ggsave("top10_operateurs.png", plot = p5, width = 11, height = 6, dpi = 300)

# Camembert : parts de marché
parts <- donnees_brutes_clean %>%
  count(nom_operateur, sort = TRUE) %>%
  mutate(
    operateur = ifelse(row_number() <= 6, nom_operateur, "Autres"),
    operateur = fct_inorder(operateur)
  ) %>%
  group_by(operateur) %>%
  summarise(n = sum(n)) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

p6 <- ggplot(parts, aes(x = "", y = n, fill = operateur)) +
  geom_col(width = 1, color = "white", size = 0.5) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Parts de marché des opérateurs IRVE", fill = "Opérateur") +
  theme_void(base_size = 12)

print(p6)
ggsave("parts_marche_operateurs.png", plot = p6, width = 9, height = 8, dpi = 300)

# Boxplot : puissance par type d'implantation 
p7 <- ggplot(donnees_brutes_clean,
             aes(x = reorder(implantation_station, puissance_nominale, median),
                 y = puissance_nominale,
                 fill = implantation_station)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.8,
               outlier.color = "#CC0000", show.legend = FALSE) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Puissance nominale par type d'implantation",
    x     = NULL,
    y     = "Puissance nominale (kW)"
  ) +
  theme_minimal(base_size = 11)

print(p7)
ggsave("boxplot_implantation.png", plot = p7, width = 11, height = 6, dpi = 300)

# Nuage de points : puissance vs nombre de PDC 
p8 <- ggplot(donnees_brutes_clean %>% sample_n(5000),  # Échantillon pour lisibilité
             aes(x = nbre_pdc, y = puissance_nominale,
                 color = implantation_station)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title  = "Relation entre nombre de PDC et puissance nominale",
    x      = "Nombre de points de charge",
    y      = "Puissance nominale (kW)",
    color  = "Implantation"
  ) +
  theme_minimal(base_size = 12)

print(p8)
ggsave("scatter_pdc_puissance.png", plot = p8, width = 11, height = 6, dpi = 300)

#  Convertir un ggplot en plotly 
ggplotly(p3)   # Évolution par année, rendue interactive
ggplotly(p7)   # Boxplot interactif

#  Graphique plotly natif : barres opérateurs
plot_ly(top10_op,
        x    = ~reorder(nom_operateur, -n),
        y    = ~n,
        type = "bar",
        text = ~paste0(pct, "%"),
        marker = list(color = "#2B4EA8")) %>%
  layout(
    title  = "Top 10 des opérateurs IRVE",
    xaxis  = list(title = "", tickangle = -35),
    yaxis  = list(title = "Nombre de PDC")
  )

#  6.3 Graphique plotly : camembert interactif 
plot_ly(parts,
        labels   = ~operateur,
        values   = ~n,
        type     = "pie",
        textinfo = "label+percent") %>%
  layout(title = "Parts de marché des opérateurs IRVE")


# visualisation cartographique


# - Préparer les données cartographiques 
donnees_brutes_carte <- donnees_brutes_clean %>%
  filter(
    !is.na(consolidated_longitude),
    !is.na(consolidated_latitude),
    # Bornes France métropolitaine + DOM
    consolidated_latitude  >= 41 & consolidated_latitude  <= 52,
    consolidated_longitude >= -6 & consolidated_longitude <= 10
  )

cat("Points cartographiables :", nrow(donnees_brutes_carte), "\n")

#  Carte simple avec popups
leaflet(donnees_brutes_carte %>% sample_n(5000)) %>%  # Échantillon pour la fluidité
  addTiles() %>%
  addCircleMarkers(
    lng         = ~consolidated_longitude,
    lat         = ~consolidated_latitude,
    radius      = 4,
    color       = "#2B4EA8",
    fillOpacity = 0.6,
    popup       = ~paste0(
      "<b>", nom_station, "</b><br>",
      "Opérateur : ", nom_operateur, "<br>",
      "Puissance : <b>", puissance_nominale, " kW</b><br>",
      "Nb PDC : ", nbre_pdc, "<br>",
      "Type prise : ", type_prise
    )
  ) %>%
  addControl("<b>Stations IRVE - France</b>", position = "topright")



# --- 7.3 Heatmap de densité ---
leaflet(donnees_brutes_carte) %>%
  addTiles() %>%
  addHeatmap(
    lng       = ~consolidated_longitude,
    lat       = ~consolidated_latitude,
    intensity = ~puissance_nominale,
    blur      = 20,
    max       = 0.05,
    radius    = 15
  ) %>%
  addControl("<b>Heatmap - Densité des stations IRVE</b>", position = "topright")

# --- 7.4 Carte avec coloration selon la puissance ---
palette_puiss <- colorNumeric(
  palette = c("#00AA00", "#FFA500", "#CC0000"),  # Vert → Orange → Rouge
  domain  = donnees_brutes_carte$puissance_nominale
)

leaflet(donnees_brutes_carte %>% sample_n(10000)) %>%
  addTiles() %>%
  addCircleMarkers(
    lng         = ~consolidated_longitude,
    lat         = ~consolidated_latitude,
    radius      = 5,
    color       = ~palette_puiss(puissance_nominale),
    fillOpacity = 0.8,
    stroke      = FALSE,
    popup       = ~paste0(
      "<b>", nom_station, "</b><br>",
      "Puissance : <b>", puissance_nominale, " kW</b><br>",
      "Catégorie : ", cat_puissance, "<br>",
      "Implantation : ", implantation_station
    )
  ) %>%
  addLegend(
    pal      = palette_puiss,
    values   = ~puissance_nominale,
    title    = "Puissance (kW)",
    position = "bottomright"
  )

# --- 7.5 Carte avec clustering (marqueurs groupés) ---
leaflet(donnees_brutes_carte) %>%
  addTiles() %>%
  addMarkers(
    lng            = ~consolidated_longitude,
    lat            = ~consolidated_latitude,
    popup          = ~paste0("<b>", nom_station, "</b><br>",
                             "Puissance : ", puissance_nominale, " kW"),
    clusterOptions = markerClusterOptions()  # Se dézoom automatiquement
  ) %>%
  addControl("<b>Clustering des stations IRVE</b>", position = "topright")




