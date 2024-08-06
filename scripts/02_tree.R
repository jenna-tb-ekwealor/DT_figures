library(rgbif)
library(rotl)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ggtree")
# BiocManager::install("ggtreeExtra")
library(ggtree)
library(ggtreeExtra)
library(taxize)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# the next three lines find the script location and set that as the working directory. copy these lines into all scripts for this project. 
library(rstudioapi)
wd_script_location <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd_script_location)



#--- Download all plant names ---#
# Latest Static Version:	World Flora Online Taxonomic Backbone	108MB	v.2023.12	Dec. 22, 2023	(DwCA) Taxonomic classification.	10.5281/zenodo.10425161
# World Flora Online The Plant List https://www.worldfloraonline.org/downloadData
# this file is too large for github so it will have to be downloaded directly by each user, and placed into the data directory
allplants <- read.csv("../data/classification.csv", sep = "\t", na.strings=c("","NA")) 
allplants %>% dplyr::select(family, genus) %>% unique() -> allplants
# omit row if NA in family
allplants[!is.na(allplants$family),] -> allplants
colnames(allplants) <- c("Family", "Genus")
allplants$Kingdom <- rep("Plantae")

#--- Load DT taxa data ---#
# Marks_et.al_Appendix_S1 2021
marks <- read.csv("../data/Marks_et.al_Appendix_S1.csv", header = T)
# fix colnames
colnames(marks) <- as.character(unlist(marks[1,]))
marks = marks[-1, ]
marks$Kingdom <- rep("Plantae")

# count genera per family
marks_counts_genera <- marks %>% dplyr::group_by(Family, Kingdom) %>% select(Genus, Family) %>% distinct() %>% dplyr::summarize(count = n())  %>% arrange(desc(count)) %>% distinct() 
# # remove "idae" non-family names
# family_df <- dplyr::filter(family_df, !grepl("idae",family))

# count families per order
marks_counts_family <- marks %>% dplyr::group_by(Order, Kingdom) %>% select(Family, Order) %>% distinct()%>% dplyr::summarize(count = n())  %>% arrange(desc(count)) %>% distinct() %>% na.omit()

# count order per class
marks_counts_order <- marks %>% dplyr::group_by(Class, Kingdom) %>% select(Order, Class) %>% distinct()%>% dplyr::summarize(count = n())  %>% arrange(desc(count)) %>% distinct() %>% na.omit()

# combine marks with all plants
allplants_DT <- marks %>% dplyr::select(Family, Genus, Kingdom) %>% rbind(., allplants)

# # loop to prevent timeouts
# Families <- allplants_DT$Family %>% unique()
# 
# tax <- list()
# for (i in 1: length(Families)) {
#   tax[[i]] <- try(tax_name(sci = Families[i], get = c("order","class","phylum"), db = "ncbi"))
#   if (class(tax[[i]])=="try-error") {
#     Sys.sleep(10)
#     tax[[i]] <- try(tax_name(sci = Families[i], get = c("order","class","phylum"), db = "ncbi"))}
# }

# this took forever so make sure to save and reload
names(tax) <- Families
# convert to df
allplants_DT_tax <- do.call(rbind.data.frame, tax)
# add order, class, phylum to allplants_DT
allplants_DT_full <- left_join(allplants_DT, allplants_DT_tax, by = c("Family"="query")) %>% dplyr::select(-db)

# rename columns
dplyr::rename(allplants_DT_full, Order = order) -> allplants_DT_full
dplyr::rename(allplants_DT_full, Class = class) -> allplants_DT_full
dplyr::rename(allplants_DT_full, Phylum = phylum) -> allplants_DT_full

#
# # save this file
# write.csv(allplants_DT_full, "../data/allplants_DT_full.csv", row.names = F)

allplants_DT_full <- read.csv("../data/allplants_DT_full.csv", header = T)

#### non-plants ####
# load list of DT nonplants_DT
nonplants_DT <- read.csv("../data/nonplants.csv", header = T)
# get genus with taxize
nonplants_DT_tax <- tax_name(nonplants_DT$Genus, get = c("family","order","class","phylum"), db = 'ncbi')

nonplants_DT$Family <- nonplants_DT_tax$family
nonplants_DT$Order <- nonplants_DT_tax$order
nonplants_DT$Class <- nonplants_DT_tax$class
nonplants_DT$Phylum <- nonplants_DT_tax$phylum

# get rid of spaces in Kingdom and Order in df
nonplants_DT$Kingdom %>% str_remove_all(" ") -> nonplants_DT$Kingdom

# count genera per family
nonplants_DT_counts_genera <- nonplants_DT %>% dplyr::group_by(Family, Kingdom) %>% select(Genus, Family) %>% distinct() %>% dplyr::summarize(count = n())  %>% arrange(desc(count)) %>% distinct() 
# # remove "idae" non-family names
# family_df <- dplyr::filter(family_df, !grepl("idae",family))

# count families per order
nonplants_DT_counts_family <- nonplants_DT %>% dplyr::group_by(Order, Kingdom) %>% select(Family, Order) %>% distinct() %>% dplyr::summarize(count = n())  %>% arrange(desc(count)) %>% distinct() %>% na.omit()

# count order per class
nonplants_DT_counts_order <- nonplants_DT %>% dplyr::group_by(Class, Kingdom) %>% select(Order, Class) %>% distinct()%>% dplyr::summarize(count = n())  %>% arrange(desc(count)) %>% distinct() %>% na.omit()


# join counts
counts_family <- full_join(marks_counts_family, nonplants_DT_counts_family)



# get list of all of these at target taxon level, call all_nonplants
# let's try order level (order-level phylogeny, counting number of families)
# need all the orders that exist for each class
unique(nonplants_DT$Class)

# https://itis.gov
# retrieve all orders of each kingdom except plants
Archaea <- taxize::downstream("Archaea", downto = "Order", db = "ncbi")
Fungi <- taxize::downstream("Fungi", downto = "Order", db = "ncbi")

Bacteria_phyla <- taxize::downstream("eubacteria", downto = "Phylum", db = "ncbi") # timing out to order level so try looping through phyla 

Bac_tax <- list()
Bac_phy <- Bacteria_phyla[["eubacteria"]][["childtaxa_name"]]
for (i in 1: length(Bac_phy)) {
  Bac_tax[i] <- try(taxize::downstream(Bac_phy[i], downto = "Order", db = "ncbi"))
  if (class(Bac_tax[i])=="try-error") {
    Sys.sleep(10)
    Bac_tax[i] <- try(taxize::downstream(Bac_phy[i], downto = "Order", db = "ncbi"))}
}
names(Bac_tax) <- Bacteria_phyla[["eubacteria"]][["childtaxa_name"]]

# convert to df
Bacteria_df <- do.call(rbind.data.frame, Bac_tax) %>% dplyr::select(childtaxa_name)
Bacteria_df$Phylum <- rownames(Bacteria_df)
Bacteria_df$Phylum <- gsub('[.]', '', Bacteria_df$Phylum)
Bacteria_df$Phylum <- gsub('[0-9]', '', Bacteria_df$Phylum)
rownames(Bacteria_df) <- NULL
colnames(Bacteria_df) <- c("Order", "Phylum")
Bacteria_df$Kingdom <- rep("Bacteria")




Animalia_phyla <- taxize::downstream("Metazoa", downto = "Phylum", db = "ncbi") # timing out to order level so try looping through phyla 

An_tax <- list()
An_phy <- Animalia_phyla[["Metazoa"]][["childtaxa_name"]]
for (i in 1: length(An_phy)) {
  An_tax[i] <- try(taxize::downstream(An_phy[i], downto = "Order", db = "ncbi"))
  if (class(An_tax[i])=="try-error") {
    Sys.sleep(10)
    An_tax[i] <- try(taxize::downstream(An_phy[i], downto = "Order", db = "ncbi"))}
}
names(An_tax) <- Animalia_phyla[["Metazoa"]][["childtaxa_name"]]

# convert to df
Animalia_df <- do.call(rbind.data.frame, An_tax) %>% dplyr::select(childtaxa_name)
Animalia_df$Phylum <- rownames(Animalia_df)
Animalia_df$Phylum <- gsub('[.]', '', Animalia_df$Phylum)
Animalia_df$Phylum <- gsub('[0-9]', '', Animalia_df$Phylum)
rownames(Animalia_df) <- NULL
colnames(Animalia_df) <- c("Order", "Phylum")
Animalia_df$Kingdom <- rep("Animalia")
  


# clean up other outputs
Archaea_df <- do.call(rbind.data.frame, Archaea)
Archaea_df$Kingdom <- rep("Archaea")
Archaea_df %>% dplyr::select(childtaxa_name, Kingdom) -> Archaea_df
rownames(Archaea_df) <- NULL
colnames(Archaea_df) <- c("Order", "Kingdom")

Fungi_df <- do.call(rbind.data.frame, Fungi)
Fungi_df$Kingdom <- rep("Fungi")
Fungi_df %>% dplyr::select(childtaxa_name, Kingdom) -> Fungi_df
rownames(Fungi_df) <- NULL
colnames(Fungi_df) <- c("Order", "Kingdom")


# join nonplants
all_nonplants <- rbind((Bacteria_df %>% dplyr::select(-Phylum)), (Animalia_df %>% dplyr::select(-Phylum)), Fungi_df, Archaea_df)

# join all_nonplants with nonplants_DT
all_nonplants_DT <- full_join(all_nonplants, nonplants_DT)

# join all_nonplants_DT with allplants_DT
full_tree_df <- full_join(all_nonplants_DT, allplants_DT_full)

# write to files
write.csv(full_tree_df, "full_tree_df.csv", row.names = F)


# reload all that from here
full_tree_df <- read.csv("full_tree_df.csv", header = T)


resolved_names <- rotl::tnrs_match_names(unique((full_tree_df$Order %>% na.omit())))
# Warning message:
#   Haliangiales, Nannocystales, Polyangiales, Deferrisomatales, Syntrophales, Desulfatiglandales, Terriglobales, Thermodesulfovibrionales, Reyranellales, Futianiales, Maricaulales, Woeseiales, Thiohalobacterales, Thiohalomonadales, Steroidobacterales, Kangiellales, Moraxellales, Vulcanimicrobiales, Miltoncostaeales, Stomatohabitantales, Salsipaludibacterales, Anaerosomatales, Tepidiformales, Aggregatilineales, Trueperales, Dethiobacterales, Culicoidibacterales, Lutisporales, Monoglobales, Gemmatales, Pirellulales, Tichowtungiales, Phormidesmidales, Geitlerinematales, Oculatellales, Aegeococcales, Thermostichales, Error : Bad Request (HTTP 400), Penicillaria (in: tube anenomes), Scleralcyonacea, Malacalcyonacea, Acropomatiformes, Tubulaniformes, Archinemertea, Kentrorhagata, Sareales, Lichinodiales, Tracyllales, Aulographales, Phaeothecales, Neophaeothecales, Microcaldales, Nanobdellales, Halorutilales, Oncothecales are not matched 

in_tree <- rotl::is_in_tree(rotl::ott_id(resolved_names))
order_tree <- tol_induced_subtree(ott_id(resolved_names)[in_tree])

# how many Orders total?
length(unique(full_tree_df$Order))
# [1] 1050

# how many in tree?
length(resolved_names$search_string[in_tree])
# [1] 737

# join original df to rotl df since tip names are ott_id
full_final_df <- dplyr::left_join((resolved_names %>% drop_na(unique_name)), (full_tree_df %>% 
                                                                                dplyr::select(Order, Class, Phylum, Kingdom) %>% 
                                                                                unique() %>% 
                                                                                drop_na(Order) %>% 
                                                                                dplyr::filter(Order != "Error : Bad Request (HTTP 400)")), 
                                  c("unique_name" = "Order"),
                                  relationship = "many-to-many")
  
full_final_df$Order <- full_final_df$unique_name

# custom fill in blanks
full_final_df %>% dplyr::mutate(Order = case_when(search_string == "phormidesmidales"~ "Phormidesmidales",
                                                    search_string == "geitlerinematales"~ "Geitlerinematales",
                                                    search_string == "oculatellales"~ "Oculatellales",
                                                    search_string == "aegeococcales" ~ "Aegeococcales", 
                                                    search_string == "thermostichales"~ "Thermostichales",
                                                    search_string == "trueperales"~ "Trueperales",
                                                    search_string == "lulworthiomycetidae" ~ "Lulworthiomycetidae",                                                     search_string == "Hookeriaceae"~ "Plantae",
                                                    search_string == "oncothecales"~ "Oncothecales",
                                                        .default = Order)) -> full_final_df


full_final_df %>% dplyr::mutate(Kingdom = case_when(Order == "Gomontiellaceae"~ "Bacteria",
                                                  Order == "Coleofasciculaceae"~ "Bacteria",
                                                  Order == "Leptolyngbyaceae"~ "Bacteria",
                                                  Order == "Prochlorotrichaceae (inconsistent in FamilyI (in SubsectionIII))" ~ "Bacteria", 
                                                  Order == "Acaryochloridaceae"~ "Bacteria",
                                                  Order == "Desertifilaceae"~ "Bacteria",
                                                  Order == "Nodosilinea" ~ "Bacteria",                                                     Order == "Hookeriaceae"~ "Plantae",
                                                  Order == "Gloeoemargaritales"~ "Bacteria",
                                                  Order == "Parachela (genus in Deuterostomia)"~ "Bacteria",
                                                  Order == "Bdelloidea (class in Lophotrochozoa)"~ "Bacteria",
                                                  Order == "Nodosilinea" ~ "Bacteria",                                                     Order == "Hookeriaceae"~ "Plantae",
                                                  Order == "Cardiopteridaceae"~ "Bacteria",
                                                  Order == "Thermosulfidibacter" ~ "Bacteria",
                                                  Order == "Thermodesulfobiaceae" ~ "Bacteria",
                                                  Order == "Tribonematales" ~ "Protista",
                                                  Order == "Desulfomonile" ~ "Bacteria",
                                                  Order == "Desulfomonile" ~ "Bacteria",
                                                  Order == "Desulfobulbaceae" ~ "Bacteria",
                                                  Order == "Candidatus Peribacterales" ~ "Bacteria",
                                                  Order == "Vicinamibacteraceae" ~ "Bacteria",
                                                  Order == "Pyrenomonadales" ~ "Protista",
                                                  Order == "Thalassobacillus" ~ "Bacteria",
                                                  Order == "Bangiales" ~ "Protista",
                                                  Order == "Acetobacteraceae" ~ "Bacteria",
                                                  Order == "Geminicoccaceae" ~ "Bacteria",
                                                  Order == "Cryptomonadales" ~ "Protista",
                                                  Order == "Rhizobiales (order silva:D11342/#4)" ~ "Bacteria",
                                                  Order == "Nitrosococcus" ~ "Bacteria",
                                                  Order == "Granulosicoccaceae" ~ "Bacteria",
                                                  Order == "Methylohalomonas" ~ "Bacteria",
                                                  Order == "Thiohalospira" ~ "Bacteria",
                                                  Order == "Thiohalorhabdus" ~ "Bacteria",
                                                  Order == "Xanthomonadales" ~ "Bacteria",
                                                  Order == "Enterobacteriales" ~ "Bacteria",
                                                  Order == "Thermincola" ~ "Bacteria",
                                                  Order == "Carboxydocella" ~ "Bacteria",
                                                  Order == "Jatrophihabitans" ~ "Bacteria",
                                                  Order == "Motilibacteraceae" ~ "Bacteria",
                                                  Order == "Kitasatospora" ~ "Bacteria",
                                                  Order == "Myxobacterales" ~ "Bacteria",
                                                  Order == "Tumebacillus" ~ "Bacteria",
                                                  Order == "Desulfitispora" ~ "Bacteria",
                                                  Order == "Desulfuribacillus" ~ "Bacteria",
                                                  Order == "Erysipelotrichales (silva:L34616/#4)" ~ "Bacteria",
                                                  Order == "Caldicoprobacteraceae" ~ "Bacteria",
                                                  Order == "Lachnospiraceae" ~ "Bacteria",
                                                  Order == "Saccharofermentans" ~ "Bacteria",
                                                  Order == "Christensenellaceae" ~ "Bacteria",
                                                  Order == "Peptostreptococcaceae (family silva:L04166/#5)" ~ "Bacteria",
                                                  Order == "Thermosediminibacter" ~ "Bacteria",
                                                  Order == "Clostridiales" ~ "Bacteria",
                                                  Order == "Antipatharia (order worms:22549)" ~ "Animalia",
                                                  Order == "Squamata (order in Deuterostomia)" ~ "Animalia",
                                                  Order == "Rhynchocephalia" ~ "Animalia",
                                                  Order == "Proboscidea (genus in subkingdom SAR)" ~ "Animalia",
                                                  Order == "Cingulata (order in Deuterostomia)" ~ "Animalia",
                                                  Order == "Pilosa (order in Deuterostomia)" ~ "Animalia",
                                                  Order == "Pholidota (genus in kingdom Archaeplastida)" ~ "Animalia",
                                                  Order == "Neomphalidae" ~ "Animalia",
                                                  Order == "Anaspidea" ~ "Animalia",
                                                  Order == "Pleurobranchomorpha" ~ "Animalia",
                                                  Order == "Randiellidae" ~ "Animalia",
                                                  Order == "Parvidrilidae" ~ "Animalia",
                                                  Order == "Narapidae" ~ "Animalia",
                                                  Order == "Alluroididae" ~ "Animalia",
                                                  Order == "Mariplanellinae" ~ "Animalia",
                                                  Order == "Peronosporales" ~ "Fungi",
                                                  Order == "Urocystales" ~ "Fungi",
                                                  Order == "Xylonomycetales" ~ "Fungi",
                                                  Order == "Micraspides" ~ "Animalia",
                                                  Order == "Chattonellales" ~ "Protista",
                                                  Order == "Cephalothecaceae" ~ "Fungi",
                                                  Order == "Codiales" ~ "Plantae",
                                                  Order == "Cladosporites" ~ "Bacteria",
                                                  Order == "Comminutispora" ~ "Bacteria",
                                                  Order == "Conexivisphaera" ~ "Bacteria",
                                                  Order == "Thermophilales" ~ "Bacteria",
                                                  Order == "Methanosaetaceae" ~ "Bacteria",
                                                  Order == "Syntrophorhabdaceae" ~ "Bacteria",
                                                  
                                                  
                                                  .default = Kingdom)) -> full_final_df

#### SAVE FULL FINAL DF TO CSV ####
write.csv(full_final_df, "../output/final_tree_data.csv", row.names = F)



# fix tip labels for plotting data
otl_tips <- strip_ott_ids(order_tree$tip.label, remove_underscores = TRUE)
taxon_map <- structure(full_final_df$unique_name, names = full_final_df$unique_name) # can use this map to change the names further if need to
order_tree$tip.label <- taxon_map[ otl_tips ]


# plot tree
# set kingdom colors
animalia.color <- "#EE89AD"
fungi.color <- "#6D4C3A"
plantae.color <- "#44BC9D"
archaea.color <- "#E8E613"
bacteria.color <- "#F6881F"
protista.color <-"#0096FF"

# after: manually move label for 0 to shortest bar on y axis
# change values to linear numbers (1, 7, 55) instead of (0, 2, 4)
order_tree_plot <- ggtree(order_tree, layout = "fan", open.angle = 1, size = 0.15, ladderize = T) +
  ggtreeExtra::geom_fruit(data = counts_family, geom = geom_bar,
                          mapping=aes(y = Order, x = log10(count +1), fill = Kingdom),
                          pwidth=0.38,
                          orientation="y",
                          stat="identity",
                          axis.params=list(
                            axis="x", # add axis text of the layer.
                            text.size = 2,
                            text.angle=0, # the text size of axis.
                            hjust=0,# adjust the horizontal position of text of axis.
                            nbreak = 4
                          ),
                          grid.params=list()) +
  scale_x_continuous(expand=c(0,0)
  ) + 
scale_fill_manual(name = "Kingdom", 
                    values = c(Animalia = animalia.color, 
                               Fungi = fungi.color, 
                               Plantae = plantae.color, 
                               Archaea = archaea.color, 
                               Bacteria = bacteria.color,
                               Protista = protista.color)) +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_cladelabel(node = 704, color = plantae.color, label = "", offset=-0.5, align=TRUE) +
  geom_cladelabel(node = 1095, color = fungi.color, label = "", offset=-0.5, align=TRUE) +
  geom_cladelabel(node = 813, color = animalia.color, label = "", offset=-0.5, align=TRUE) +
  geom_cladelabel(node = 1146, color = bacteria.color, label = "", offset=-0.5, align=TRUE) +
  geom_cladelabel(node = 1242, color = archaea.color, label = "", offset=-0.5, align=TRUE)  


order_tree_plot 

pdf(file = "../output/tree.pdf", height = 4, width = 4.3)
order_tree_plot
dev.off()

# pdf(file = "../output/tree_nodenumbers.pdf", height = 4, width = 4.3)
# order_tree_plot +
#   geom_tiplab(size = 0.5) +
#   geom_text(aes(label=node), size = 1, color = "red")
# dev.off()


