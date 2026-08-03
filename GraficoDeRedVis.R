require(multiweb)
require(igraph)
b <- readNetwork("BeagleChannel_links_standarized_actualizado.csv", edgeListFormat = 2)

importantes <- c(
  "Macrocystis pyrifera",
  "Grimothea gregaria",        # antes: Munida gregaria
  "Lithodes santolla",
  "Eleginops maclovinus",
  "Odontesthes nigricans",
  "Paranotothenia magellanica",
  "Mytilus edulis",            # antes: Mytilus edulis chilensis
  "Perumytilus purpuratus",
  "Nacella magellanica",
  "Phalacrocorax atriceps",
  "Larus dominicanus",
  "Lontra provocax",
  "Megaptera novaeangliae",
  "Codium sp",
  "Loxechinus albus",
  "Arbacia dufresnii",
  "Pseudechinus magellanicus",
  "Gastropoda"
)

nombres <- c(
  "Cachiyuyo",
  "Langostilla",
  "Centolla",
  "Róbalo",
  "Pejerrey",
  "Nototenido",
  "Mejillón",
  "Mejillín",
  "Lapa",
  "Cormorán imperial",
  "Gaviota cocinera",
  "Huillín",
  "Ballena jorobada",
  "Codium",
  "Erizo rojo",
  "Erizo negro",
  "Erizo magallánico",
  "Caracoles y babosas marinas"
)

# normaliza "." o "_" a espacio para que el match funcione
# sin importar el formato de V(b)$name (con puntos o con espacios)
norm <- function(x) trimws(gsub("[._]+", " ", x))

idx <- match(norm(importantes), norm(V(b)$name))
if (any(is.na(idx))) {
  warning("No se encontraron en el grafo: ",
          paste(importantes[is.na(idx)], collapse = ", "))
}

V(b)$label <- ""
V(b)$label[idx[!is.na(idx)]] <- nombres[!is.na(idx)]

#plot_troph_level_visNet(b, physics = "x",label_size=15)
plot_troph_level_visNet(b, physics = "full",label_size=25)

#
# Sub red de Centolla
#

library(igraph)

norm <- function(x) trimws(gsub("[._]+", " ", x))

foco <- "Lithodes santolla"
idx_foco <- which(norm(V(b)$name) == foco)

if (length(idx_foco) == 0) {
  stop("No se encontro '", foco, "' en V(b)$name. Revisa el formato de nombres del grafo.")
}

vecinos <- ego(b, order = 1, nodes = idx_foco, mode = "all")[[1]]
centolla_sub <- induced_subgraph(b, vecinos)

# --- mapeo cientifico -> comun para los 33 nodos de esta subred ---
cientificos <- c(
  "Lithodes santolla",
  "Amphipoda",
  "Ascidiacea",
  "Aulacomya atra",
  "Bivalvia",
  "Bryozoa",
  "Calliostoma nudum",
  "Copepoda",
  "Fissurella picta",
  "Gastropoda",
  "Halicarcinus planatus",
  "Hiatella arctica",
  "Hydrozoa",
  "Isopoda",
  "Macroalgae",
  "Macrocystis pyrifera",
  "Margarella violacea",
  "Membranipora isabelleana",
  "Grimothea gregaria",
  "Mytilus edulis",
  "Necromass",
  "Ophiuroidea",
  "Pagurus comptus",
  "Peltarion spinulosum",
  "Polychaeta",
  "Polyplacophora",
  "Porifera",
  "Pseudechinus magellanicus",
  "Serpulidae",
  "Tonicia sp",
  "Zooplankton",
  "Phalacrocorax atriceps",
  "Phalacrocorax magellanicus"
)
comunes <- c(
  "Centolla",
  "Anfípodos",
  "Ascidias",
  "Cholga",
  "Bivalvos",
  "Briozoos",
  "Caracol (Calliostoma)",
  "Copépodos",
  "Lapa",
  "Caracoles y babosas marinas",
  "Cangrejo plano",
  "Almeja perforadora",
  "Hidrozoos",
  "Isópodos",
  "Macroalgas",
  "Cachiyuyo",
  "Caracolito violeta",
  "Briozoo incrustante",
  "Langostilla",
  "Mejillón",
  "Materia orgánica muerta",
  "Ofiuras",
  "Cangrejo ermitaño",
  "Cangrejo araña",
  "Poliquetos",
  "Quitones",
  "Esponjas",
  "Erizo magallánico",
  "Gusanos tubícolas",
  "Quitón (Tonicia)",
  "Zooplancton",
  "Cormorán imperial",
  "Cormorán roquero"
)

idx <- match(norm(V(centolla_sub)$name), norm(cientificos))
if (any(is.na(idx))) {
  warning("Sin nombre comun asignado: ",
          paste(V(centolla_sub)$name[is.na(idx)], collapse = ", "))
}
V(centolla_sub)$label <- comunes[idx]

# destacar el nodo foco
V(centolla_sub)$color <- ifelse(
  norm(V(centolla_sub)$name) == foco,
  "orange",
  "lightblue"
)

plot_troph_level_visNet(centolla_sub, physics = "x")
plot_troph_level_visNet(centolla_sub, physics = "full")

