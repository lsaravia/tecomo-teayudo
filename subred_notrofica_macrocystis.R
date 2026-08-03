#!/usr/bin/env Rscript
# ============================================================
# Subred NO TROFICA: especies asociadas a Macrocystis pyrifera
# (provision de habitat/refugio en grampon y fronda), documentadas
# en literatura de la zona subantartica de Argentina y Chile.
#
# Fuentes (ver columna 'referencia' para el detalle por especie):
#  - Adami, M.L. & Gordillo, S. (1999). Structure and dynamics of
#    the biota associated with Macrocystis pyrifera (Phaeophyta)
#    from the Beagle Channel, Tierra del Fuego. Scientia Marina,
#    63(Supl. 1), 183-191.  [Canal Beagle, Argentina]
#  - Alonso, G.M. (2012). Amphipod crustaceans (Corophiidea and
#    Gammaridea) associated with holdfasts of Macrocystis pyrifera
#    from the Beagle Channel (Argentina)... Journal of Natural
#    History, 46(29-30), 1799-1894.  [Canal Beagle, Argentina]
#  - Cariceo, Y., Mutschke, E. & Rios, C. (2002). Ensambles de
#    Isopoda (Crustacea) en discos de fijacion del alga Macrocystis
#    pyrifera... en el Estrecho de Magallanes, Chile. Anales
#    Instituto Patagonia Ser. Cs. Nat., 30, 83-94. [Edo. Magallanes]
#  - Rios, C., Mutschke, E. & Cariceo, Y. (2003). Estructura
#    poblacional de Pseudechinus magellanicus... en grampones de
#    Macrocystis pyrifera... Estrecho de Magallanes, Chile. Anales
#    del Instituto de la Patagonia, 31, 75-86. [Edo. Magallanes]
#  - Vanella, F.A., Fernandez, D.A., Romero, M.C. & Calvo, J. (2007).
#    Changes in the fish fauna associated with a sub-Antarctic
#    Macrocystis pyrifera kelp forest in response to canopy removal.
#    Polar Biology, 30, 449-457. [Canal Beagle / Tierra del Fuego]
#  - Moreno, C.A. & Jara, H.F. (1984). Ecological studies on fish
#    fauna associated with Macrocystis pyrifera belts in the south
#    of Fueguian Islands, Chile. Marine Ecology Progress Series,
#    15, 99-107. [Islas Fueguinas, Chile]
#  - Diez, M.J., Florentin, O. & Lovrich, G.A. (2011). Distribucion
#    y estructura poblacional del cangrejo Halicarcinus planatus
#    (Brachyura, Hymenosomatidae) en el Canal Beagle, Tierra del
#    Fuego. Revista de Biologia Marina y Oceanografia, 46(2),
#    141-155.  [Canal Beagle, Argentina]
#  - Cardenas, C.A., Canete, J.I., Oyarzun, S. & Mansilla, A. (2007).
#    Podding of juvenile king crabs Lithodes santolla (Molina, 1782)
#    (Crustacea) in association with holdfasts of Macrocystis
#    pyrifera (Linnaeus) C. Agardh, 1980. Latin American Journal of
#    Aquatic Research, 35(1), 105-110.  [Chile]
#
# IMPORTANTE: todas las especies/grupos de la columna 'target' ya
# existen como nodos en tu red trofica (BeagleChannel_links_
# standarized_actualizado.csv), verificado contra los 166 nodos.
# ============================================================

library(igraph)
library(dplyr)

# ---- 1. Tabla de interacciones no troficas -------------------
no_troficas <- data.frame(
  source = "Macrocystis pyrifera",
  target = c(
    "Amphipoda", "Isopoda", "Pseudechinus magellanicus",
    "Bivalvia", "Gastropoda", "Polyplacophora", "Bryozoa",
    "Membranipora isabelleana", "Porifera", "Nemertea",
    "Polychaeta", "Ophiuroidea", "Cirripedia",
    "Patagonotothen cornucola", "Patagonotothen sima", "Harpagifer bispinis",
    "Halicarcinus planatus", "Lithodes santolla"
  ),
  tipo = "provision_habitat",
  referencia = c(
    "Alonso 2012, J Nat Hist 46(29-30):1799-1894 (Canal Beagle)",
    "Cariceo et al. 2002, An Inst Patagonia 30:83-94 (Edo. Magallanes)",
    "Rios et al. 2003, An Inst Patagonia 31:75-86 (Edo. Magallanes)",
    rep("Adami & Gordillo 1999, Sci Mar 63(S1):183-191 (Canal Beagle)", 10),
    rep("Vanella et al. 2007, Polar Biol 30:449-457 (Canal Beagle); Moreno & Jara 1984, Mar Ecol Prog Ser 15:99-107 (Islas Fueguinas)", 3),
    "Diez, Florentin & Lovrich 2011, Rev Biol Mar Oceanogr 46(2):141-155 (Canal Beagle)",
    "Cardenas et al. 2007, Lat Am J Aquat Res 35(1):105-110 (Chile)"
  ),
  stringsAsFactors = FALSE
)

cat("Interacciones no troficas a agregar:", nrow(no_troficas), "\n")
print(no_troficas %>% select(target, referencia))

# ---- 2. Chequeo: todos los nodos existen en el grafo 'b' -------
# (asume que 'b' ya esta cargado en el entorno, con V(b)$name
#  en formato "Genero especie", con o sin puntos)
norm <- function(x) trimws(gsub("[._]+", " ", x))

faltantes <- setdiff(norm(c(no_troficas$source, no_troficas$target)), norm(V(b)$name))
if (length(faltantes) > 0) {
  warning("Nodos no encontrados en V(b)$name: ", paste(faltantes, collapse = ", "))
}

# ---- 3. Agregar como una capa nueva (multiplex) -----------------
# Opcion A: grafo separado solo con la red no trofica
edges_nt <- no_troficas %>%
  mutate(from = norm(source), to = norm(target)) %>%
  select(from, to, tipo, referencia)

g_notrofico <- graph_from_data_frame(edges_nt, directed = TRUE)

# Opcion B: agregar estas aristas al grafo trofico existente 'b',
# marcadas con un atributo 'tipo' para poder diferenciarlas visualmente
b_multiplex <- b
idx_from <- match(edges_nt$from, norm(V(b_multiplex)$name))
idx_to   <- match(edges_nt$to,   norm(V(b_multiplex)$name))

nuevas_aristas <- as.vector(rbind(idx_from, idx_to))
b_multiplex <- add_edges(b_multiplex, nuevas_aristas,
                         attr = list(tipo = "no_trofica",
                                     referencia = edges_nt$referencia))

# Marcar el resto de las aristas preexistentes como troficas
# (solo si el atributo no existia previamente)
if (is.null(E(b)$tipo)) {
  E(b_multiplex)$tipo[is.na(E(b_multiplex)$tipo)] <- "trofica"
}

cat("\nListo. Objetos generados:\n")
cat(" - g_notrofico   : grafo solo con la subred no trofica (", vcount(g_notrofico), "nodos,", ecount(g_notrofico), "aristas)\n")
cat(" - b_multiplex   : grafo 'b' original + aristas no troficas (atributo E()$tipo)\n")

# ---- 4. Visualizacion sugerida -----------------------------------
# Solo la subred no trofica de Macrocystis:
plot_troph_level_visNet(g_notrofico, physics = "full")

# Red completa distinguiendo tipo de interaccion por color de arista:
plot_troph_level_visNet_multi(
  b_multiplex,
  layer_attr = "tipo",
  trophic_layer = "trofica",
  layer_dashed = "no_trofica",
  physics = "full"
)
