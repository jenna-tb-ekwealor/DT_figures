library(igraph)

set.seed(37)
pdf(file = "../output/purple.evolution.network.pdf", height = 4, width = 4.3)
purple.evolution <- erdos.renyi.game(120, 300, type = "gnm") %>%
  set_vertex_attr(name = "size", value = pmax(degree(.), 5)) %>%
  plot(vertex.label = NA, vertex.color = "#7962A5", main = "", margin = -0.125)
dev.off()

set.seed(38)
pdf(file = "../output/blue.geography.network.pdf", height = 4, width = 4.3)
blue.geography <- erdos.renyi.game(300, 900, type = "gnm") %>%
  set_vertex_attr(name = "size", value = pmax(degree(.), 5)) %>%
  plot(vertex.label = NA, vertex.color = "#94D0E3", main = "", margin = -0.125)
dev.off()

set.seed(39)
pdf(file = "../output/green.organism.network.pdf", height = 4, width = 4.3)
green.organism <- erdos.renyi.game(620, 4000, type = "gnm") %>%
  set_vertex_attr(name = "size", value = pmax(degree(.), 8)) %>%
  plot(vertex.label = NA, vertex.color = "#65A453", main = "", margin = -0.125)
dev.off()

set.seed(40)
pdf(file = "../output/yellow.cell.network.pdf", height = 4, width = 4.3)
yellow.cell <- erdos.renyi.game(400, 1000, type = "gnm") %>%
  set_vertex_attr(name = "size", value = pmax(degree(.), 5)) %>%
  plot(vertex.label = NA, vertex.color = "#F5C245", main = "", margin = -0.125)
dev.off()

set.seed(41)
pdf(file = "../output/red.molecule.network.pdf", height = 4, width = 4.3)
red.molecule <- erdos.renyi.game(620, 2102, type = "gnm") %>%
  set_vertex_attr(name = "size", value = pmax(degree(.), 5)) %>%
  plot(vertex.label = NA, vertex.color = "#DF6276", main = "", margin = -0.125)
dev.off()

set.seed(42)
pdf(file = "../output/collaboration.network.pdf", height = 4, width = 4.3)
collaboration <- erdos.renyi.game(900, 2800, type = "gnm") %>%
  set_vertex_attr(name = "size", value = pmax(degree(.), 1)) %>%
  plot(vertex.label = NA, vertex.color = c("#7962A5","#94D0E3","#65A453","#F5C245","#DF6276", "#E68758"), main = "", margin = -0.125)
dev.off()
