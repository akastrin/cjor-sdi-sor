




#options(repos='http //cran.rstudio.com/')
#install.packages("midasr",dep=TRUE)



library(tidyverse)
library(tidygraph)
library(ggraph)
library(bibliometrix)
library(akc)

## Renviron
##options(browser="firefox")

file_wos <- "./data/savedrecs-2021-04-14.txt"
file_dim <- "./data/Dimensions-Publication-2021-04-14_09-13-36.xlsx"

M_wos <- convert2df(file = file_wos, dbsource = "isi", format = "plaintext")
M_dim <- convert2df(file = file_dim, dbsource = "dimensions", format = "excel")
M <- mergeDbSources(M_wos, M_dim, remove.duplicated = TRUE)



M %>%
  replace_na(list(PY = 2021)) -> M0


## M %>%
##   replace_na(list(PY = 2021)) %>%
##   filter(PY > 2015) %>%
##   select(PY) %>% nrow()


## M0 %>%
##   filter(PY <= 2017) -> M1

## M0 %>%
##   filter(PY > 2017) -> M2



results <- biblioAnalysis(M0, sep = ";")

S <- summary(object = results, k = 10, pause = FALSE)

## m0_id <- thematicMap(M0, field = "ID", n = 250, minfreq = 2, size = 0.5, repel = TRUE)
## m0_de <- thematicMap(M0, field = "DE", n = 250, minfreq = 2, size = 0.5, n.labels = 1, repel = TRUE, stemming = FALSE)
##m0_ti <- thematicMap(M0, field = "TI", n = 250, minfreq = 5, size = 0.5, repel = TRUE)
##m0_ab <- thematicMap(M0, field = "AB", n = 250, minfreq = 5, size = 0.5, repel = TRUE)

## ggsave("fig-1.png")


## m0_de$clusters %>%
##   as_tibble() %>%
##   arrange(desc(freq)) %>%
##   select(name)

## library(tidytext)
## library(stringdist)
## library(refinr)

## M0 %>%
##   as_tibble() %>%
##   select(DI, DE) %>%
##   unnest_tokens(word, DE, token = "regex", pattern = "; ") %>%
##   drop_na() %>% pull(word) %>% unique() -> kw_vec
##   ##select(word) %>% n_distinct(.$word) %>%
##   ##select(word) %>% pull(word) %>% unique() %>% length()
## group_by(word) %>% tally() %>% arrange(desc(n))


## ## Match by DE in AB
## M0 %>%
##   as_tibble() %>%
##   mutate(id = row_number()) %>%
##   select(id, TI, DE, AB) %>%
##   rename(title = TI, keyword = DE, abstract = AB) -> bib_tbl1

## ## Match by DE and ID in AB
## M0 %>%
##   as_tibble() %>%
##   mutate(id = row_number()) %>%
##   select(id, TI, ID, DE, AB) %>%
##   unite("KW", ID:DE, remove = FALSE, sep = "; ", na.rm = TRUE) %>%
##   select(id, TI, KW, AB) %>%
##   rename(title = TI, keyword = KW, abstract = AB) -> bib_tbl2

## Match by DE and ID in TI and AB
M0 %>%
  as_tibble() %>%
  mutate(id = row_number()) %>%
  select(id, TI, ID, DE, AB) %>%
  unite("KW", ID:DE, remove = TRUE, sep = "; ", na.rm = TRUE) %>%
  unite("TX", c(TI, AB), remove = TRUE, sep = ".", na.rm = TRUE) %>%
  select(id, KW, TX) %>%
  rename(keyword = KW, abstract = TX) -> bib_tbl3

## ## Match by DE and ID in TI and AB
## M0 %>%
##   as_tibble() %>%
##   mutate(id = row_number()) %>%
##   select(id, TI, ID, DE, AB, DI) %>%
##   unite("KW", ID:DE, remove = TRUE, sep = "; ", na.rm = TRUE) %>%
##   unite("TX", c(TI, AB), remove = TRUE, sep = ".", na.rm = TRUE) %>%
##   select(id, KW, TX, DI) %>%
##   rename(keyword = KW, abstract = TX, doi = DI) -> bib_tbl3_doi




## M0 %>% select(AU, DI)




bib_tbl3 %>%
  keyword_clean() %>%
  pull(keyword) %>%
  make_dict() -> my_dict

tidytext::stop_words %>%
    pull(word) %>%
    unique() -> my_stopword

bib_tbl3 %>%
    keyword_extract(id = "id", text = "abstract",
    dict = my_dict, stopword = my_stopword) -> extracted_keywords

extracted_keywords %>%
  keyword_merge() -> merged_keywords

merged_keywords %>%
  keyword_group(com_detect_fun = group_louvain) -> grouped_keywords_3

#grouped_keywords %>%
#  as_tibble()

#grouped_keywords %>%
#  keyword_table()

#grouped_keywords %>%
#  keyword_network(group_no = 1, max_nodes = 10)

## Number of documents in a given cluster

## grouped_keywords_3 %>%
##   activate(nodes) %>%
##   as_tibble() %>%
##   count(group)

## extracted_keywords %>% count(id)

## merged_keywords %>% pull(keyword) %>% is.na() %>% table()
## foo %>% pull(name) %>% is.na() %>% table()






minfreq <- 5
minfreq <- max(2,floor(minfreq * nrow(M) / 1000))



set.seed(20210425)
g <- grouped_keywords_3

as_adjacency_matrix(g, attr = "n", sparse = FALSE) -> NetMatrix

net <- g
group <- net %>% activate(nodes) %>% pull(group)
word <- net %>% activate(nodes) %>% pull(name)
color=V(net)$color
color[is.na(color)]="#D3D3D3"

###
W <- intersect(row.names(NetMatrix), word)
index <- which(row.names(NetMatrix) %in% W)
ii <- which(word %in% W)
word <- word[ii]
group <- group[ii]
color <- color[ii]
###

##C <- diag(NetMatrix)
C <- net %>% activate(nodes) %>% pull(freq)


##S <- normalizeSimilarity(NetMatrix, type = "association")
##diag <- Matrix::diag
net_mat <- NetMatrix
diag(net_mat) <- net %>% activate(nodes) %>% pull(freq)
D <- diag(net_mat)
S <- net_mat / ((outer(D, D, "*")))
#S=as.matrix(S)
#S[is.nan(S)]=0
#S=Matrix(S, sparse=TRUE)

sEij <- S[index, index]
##dim(sEij)
sC <- (C[index])


### centrality and density
label_cluster <- unique(group)
word_cluster <- word[group]
centrality <- c()
density <- c()
labels <- list()

df_lab <- data.frame(sC = sC, words = word, groups = group, #color=color,
                     cluster_label = "NA", stringsAsFactors = FALSE)



#color=c()
for (i in label_cluster) {
  ind <- which(group == i)
  w <- df_lab$words[ind]
  wi <- which.max(df_lab$sC[ind])
  df_lab$cluster_label[ind] <- paste(w[wi[1 : min(c(length(wi), 3))]], collapse = ";", sep = "")
  centrality <- c(centrality, sum(sEij[ind, -ind]))
  density <- c(density, sum(sEij[ind, ind]) / length(ind) * 100)
  df_lab_g <- df_lab[ind, ]
  df_lab_g <- df_lab_g[order(df_lab_g$sC, decreasing = TRUE), ]
  #if (dim(df_lab_g)[1]>2){k=3}else{k=1}
  k <- 1
  labels[[length(labels) + 1]] <- paste(df_lab_g$words[1 : k], collapse = ";")
  ##color=c(color,df_lab$color[ind[1]])
}

centrality <- centrality


col_set <- RColorBrewer::brewer.pal(8, "Accent")
df <- data.frame(centrality = centrality, density = density, rcentrality = rank(centrality), rdensity = rank(density),
                 label = label_cluster, color = col_set)

meandens <- mean(df$rdensity)
meancentr <- mean(df$rcentrality)
rangex <- max(c(meancentr - min(df$rcentrality), max(df$rcentrality) - meancentr))
rangey <- max(c(meandens - min(df$rdensity), max(df$rdensity) - meandens))

df$name <- unlist(labels)
df <- df[order(df$label), ]
df_lab <- df_lab[df_lab$sC >= minfreq, ]
df <- df[(df$name %in% intersect(df$name, df_lab$cluster_label)), ]

row.names(df) <- df$label

A <- group_by(df_lab, .data$groups) %>% summarise(freq = sum(.data$sC)) %>% as.data.frame()

df$freq <- A[, 2]

W <- df_lab %>% group_by(.data$groups) %>% #dplyr::filter(.data$sC>1) %>%
  arrange(-.data$sC, .by_group = TRUE) %>%
  dplyr::top_n(10, .data$sC) %>%
  summarise(wordlist = paste(.data$words, .data$sC, collapse = "\n")) %>% as.data.frame()

df$words <- W[, 2]

### number of labels for each cluster
labels <- gsub("\\d", "", df$words)

### cut ties over 10 words
df$words <- unlist(lapply(df$words, function(l) {
  l <- unlist(strsplit(l, "\\\n"))
  l <- l[1 : (min(length(l), 10))]
  l <- paste0(l,collapse = "\n")
}))


n.labels <- 1
L <- unlist(lapply(labels, function(l) {
  l <- strsplit(l," \\\n")
  l <- paste(l[[1]][1 : min(n.labels, lengths(l))], collapse = "\n")
}))

##df$name_full <- L
###
### Replace original labels with custom labels
L <- c("decision support", "optimization", "algorithms & graphs",
       "models & simulations", "systems & logistics", "heuristics & utilities",
       "game theory", "statistical methods")
df$name_full <- L

xlimits <- c(meancentr - rangex - 0.5, meancentr + rangex + 0.5)
ylimits <- c(meandens - rangey - 0.5, meandens + rangey + 0.5)

annotations <- data.frame(
  xpos <- sort(c(xlimits, xlimits)),
  ypos <- c(ylimits, ylimits),
  words <- c("Emerging or\nDeclining Themes","Niche Themes","Basic Themes", "Motor Themes"),
  hjustvar <- c(0, 0, 1, 1),
  vjustvar <- c(0, 1.0, 0, 1))

x <- c(max(df$rcentrality) - 0.02 - diff(range(df$rcentrality)) * 0.125, max(df$rcentrality) - 0.02) + 0.5
y <- c(min(df$rdensity), min(df$rdensity) + diff(range(df$rdensity)) * 0.125)



size <- 2


plt <- ggplot(df, aes(x = .data$rcentrality, y = .data$rdensity, text = c(.data$words))) +
  ##geom_point(group = "NA", aes(size = log(as.numeric(.data$freq))), shape = 20, col = adjustcolor(df$color, alpha.f = 0.5))     # Use hollow circles
  geom_point(group = "NA", aes(size = log(as.numeric(.data$freq))), shape = 20, col = df$color)

plt <- plt +
  ggrepel::geom_label_repel(aes(group = "NA", label = unlist(tolower(.data$name_full)), size = 0.25, angle = 0))

plt <- plt +
  geom_hline(yintercept = meandens, linetype = 2, color = "black") +
  geom_vline(xintercept = meancentr,linetype = 2, color = "black") +
  scale_radius(range = c(1 * (1 + size), 7 * (1 + size))) +
  labs(x = "Centrality", y = "Density") +
  xlim(xlimits) +
  ylim(ylimits) +
  annotate("text", x = annotations$xpos, y = annotations$ypos, hjust = annotations$hjustvar,
           vjust = annotations$vjustvar, label = annotations$words, color = adjustcolor("gray20", alpha.f = 0.5), size = 3) +
  ##cowplot::theme_cowplot() +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 10),
        legend.position = "none",
        aspect.ratio = 1) +
  ggExtra::removeGrid()

plt
ggsave("test-1.pdf", width = 120, height = 120, units = "mm")
ggsave("test-1.png", width = 120, height = 120, units = "mm")
system("pdfcrop test-1.pdf")


g2df_3 <- df


g2df_3 %>%
  select(label, words) %>%
  separate_rows(words, sep = "\n") %>%
  separate(col = words, into = c("term", "freq"), sep = "\\s+(?=\\d+$)", convert = TRUE) %>%
  group_by(label) %>%
  top_n(20) %>%
  print(n = Inf)




## 1 decision support MOTOR
## 2 optimization MOTOR
## 3 algorithms & graphs EMERGING or DECLINING
## 4 models & simulations EMERGING or DECLINING
## 5 systems & logistics MOTOR
## 6 heuristics & utilities BASIC
## 7 set, dynamics, game, clustering, efficient solution EMERGING or DECLINING
## 8 statistical methods NICHE


## ## Summary
## merged_keywords %>%
##   left_join(x = ., y = grouped_keywords_3 %>% activate(nodes) %>% as_tibble(), by = c("keyword" = "name")) %>%
##   group_by(group) %>%
##   summarise(across(id, list(n = n_distinct))) -> group2id


## df %>%
##   as_tibble() %>%
##   left_join(group2id, by = c("label" = "group")) %>%
##   select(name_full, freq, id_n, centrality, density) %>% pull(density) %>% mean() %>% round(1)

## ## KW -> doi
## merged_keywords %>%
##   left_join(bib_tbl3_doi, by = c("id" = "id")) %>%
##   left_join(x = ., y = M0 %>% as_tibble() %>% select(DI, TI), by = c("doi" = "DI")) %>%
##   select(keyword.x, TI, doi) %>%
##   rename(keyword = keyword.x, title = TI) %>%
##   write_excel_csv("keyword2title.csv")





## ## String distance matrix
## sdm = stringdistmatrix(kw_vec, kw_vec, useNames = TRUE, method = "lcs")
## sdm[1:5, 1:5]

## # Hierarchical clustering
## sdm_dist = as.dist(sdm) # convert to a dist object (you essentially already have distances calculated)
## hclust(sdm_dist, method = "ward.D") %>% plot()

## library("cluster")
## bum <- pam(sdm_dist, 5)

## clusplot(pam(sdm_dist, 5), color=TRUE, shade=F, labels=2, lines=0)


## library(fpc)
## pamk.best <- pamk(sdm_dist)
## cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
## plot(pam(sdm_dist, pamk.best$nc))




## library("WeightedCluster")
## hc <- hclust(sdm_dist)
## hcRange <- as.clustrange(hc, diss = sdm_dist, ncluster=10)
## summary(hcRange)
## plot(hcRange, stat = c("ASWw", "HG", "PBC"), lwd = 2)



## ## You can try adist function (Approximate String Distances).

## which(sdm == max(sdm), arr.ind = TRUE)

## bla <- n_gram_merge(vect = kw_vec)
## key_collision_merge(kw_vec)

## x_refin <- kw_vec %>%
##   key_collision_merge() %>%
##   n_gram_merge()

## inspect_results <- tibble(original_values = kw_vec, edited_values = x_refin) %>%
##   mutate(equal = original_values == edited_values)



## ## Using tidytext
## M0 %>%
##   as_tibble() %>%
##   select(DI, DE) %>%
##   unnest_tokens(word, DE, token = "regex", pattern = "; ") %>%
##   drop_na() %>%
##   ##count(word, sort = TRUE) -> m0_td
##   count(DI, word) %>%
##   cast_dtm(DI, word, n) -> m0_tf_idf

## kfit <- kmeans(m0_tf_idf, centers = 5)


## library("quanteda")




## sdm <- stringdistmatrix(kw_vec, kw_vec, useNames = TRUE, method = "lv")
## sdm_dist <- as.dist(sdm)

## pam_fit <- pam(sdm_dist, 10)

## enframe(pam_fit$clustering, name = "term", value = "cluster") %>%
##   count(term, cluster) %>%
##   arrange(cluster) %>%
##   write_excel_csv("terms-to-clusters-10-2021-04-22.csv")




## tbl <- read_csv("./terms-to-clusters-10-2021-04-22.csv")
## tbl %>% pull(term) -> kw_vec

## ## CO-WORD ANALYSIS
## W <- stringdistmatrix(kw_vec, kw_vec, useNames = TRUE, method = "jw")
## W[W > 0.25] <- NA
## logit <- function(p) log(p) / log(1 - p)
## W <- logit(W)
## W[is.na(W)] <- 0
## diag(W) <- 0

## M <- data.frame(t(combn(kw_vec, 2)))
## dim(M)
## M$D <- apply(M, 1, function(x) stringdist(x[1], x[2], method = "jw"))
## n <- length(kw_vec)
## W <- matrix(NA, n, n)
## rownames(W) <- colnames(W) <- kw_vec
## W[lower.tri(W)] <- M$D
## W[upper.tri(W)] <- t(W)[upper.tri(W)]
## W[W < 0.6] <- NA
## logit <- function(p) log(p) / log(1 - p)
## W <- logit(W)
## W[is.na(W)] <- 0

## WW <- graph.adjacency(W, diag = TRUE, weighted = TRUE, mode = "undirected")


## bsk.network <- WW

## library(igraph)

## # Create igraph object
## bsk.network <- graph.adjacency(NetMatrix, diag = TRUE, mode = "undirected", weighted = TRUE)

## # vertex labels
## #V(bsk.network)$name <- colnames(NetMatrix)

## ## Compute node degrees (#links) and use that to set node size:
## deg <- degree(bsk.network, mode = "all")
## V(bsk.network)$deg <- deg
## size <- 20
## V(bsk.network)$size <- (deg / max(deg)[1]) * size

## ## label size
## labelsize <- 1.2
## lsize <- log(1 + (deg / max(deg)[1])) * labelsize
## lsize[lsize < 0.5] <- 0.5  ### min labelsize is fixed to 0.5
## V(bsk.network)$label.cex <- lsize

## ## Select number of vertices to plot
## Deg <- deg - diag(W)
## min_deg <- 2
## Vind <- Deg < min_deg
## bsk.network <- delete.vertices(bsk.network, which(Vind))


## # Community Detection
## ##cl <- clusteringNetwork(bsk.network, cluster = "louvain")
## net_groups <- cluster_louvain(bsk.network, weights = E(bsk.network)$weight)


























## m1_id <- thematicMap(M1, field = "ID", n = 100, minfreq = 2, size = 0.5, repel = TRUE)
## m1_de <- thematicMap(M1, field = "DE", n = 150, minfreq = 3, size = 0.5, repel = TRUE)
## ##m1_ti <- thematicMap(M1, field = "TI", n = 250, minfreq = 2, size = 0.5, repel = TRUE)
## ##m1_ab <- thematicMap(M1, field = "AB", n = 250, minfreq = 2, size = 0.5, repel = TRUE)

## m2_id <- thematicMap(M2, field = "ID", n = 100, minfreq = 2, size = 0.5, repel = TRUE)
## m2_de <- thematicMap(M2, field = "DE", n = 200, minfreq = 3, size = 0.5, repel = TRUE)
## ##m2_ti <- thematicMap(M2, field = "TI", n = 250, minfreq = 5, size = 0.5, repel = TRUE)
## ##m2_ab <- thematicMap(M2, field = "AB", n = 250, minfreq = 5, size = 0.5, repel = TRUE)


## ## Combine keywords
## KWlist <- keywordAssoc(M0, sep = ";",n = 10, excludeKW = NA)






## years <- c(2016)

## nexus <- thematicEvolution(M0, field="DE", years=years, n=100, minFreq=2)

## plotThematicEvolution(nexus$Nodes,nexus$Edges)



## CS0 <- conceptualStructure(M0, field="DE", method="MCA", minDegree=2, clust="auto", stemming=FALSE, labelsize=10, documents=2,
##                           graph = FALSE)
## CS0$graph_terms
## ggsave("tm-2.png")

## CS1 <- conceptualStructure(M1, field="DE", method="MCA", minDegree=2, clust="auto", stemming=FALSE, labelsize=10, documents=2,
##                           graph = FALSE)

## CS1$graph_terms

## CS2 <- conceptualStructure(M2, field="DE", method="MCA", minDegree=1, clust="auto", stemming=FALSE, labelsize=10, documents=2,
##                           graph = FALSE)

## CS2$graph_terms




## # Most productive authors
## authors <- names(results$Authors)[1:10]
## ##authors <- str_to_title(authors)
## indices <- Hindex(M0, field = "author", elements = authors, sep = ";", years = 10)
## myTC <- map(indices$CitationList, "TotalCitation") %>% sapply(sum, na.rm = TRUE)

## indices$H %>%
##   mutate(TC2 = myTC) %>%
##   select(Author, NP, TC2, h_index)

## Hindex(M0, field = "author", elements = names(results$Authors)[1:10])$H %>%
##                                                                       select(Author, NP, TC, h_index, g_index, m_index) %>%
##                                                                       pull(m_index) %>% round(2)




## M0 %>% select(C1)

## tibble(results$FirstAuthors, results$CO) %>% print(n = Inf)


## S$MostCitedPapers %>%
##   as_tibble() %>%
##   select(-DOI)




## Co-authorship
net_mat <- biblioNetwork(M0, n = NULL, analysis = "collaboration", network = "authors", sep = ";")
## net_plt <- networkPlot(net_mat, degree = 0, weighted = TRUE, type = "auto", verbose = FALSE)
net_plt <- networkPlot(net_mat, degree = NULL, weighted = TRUE, type = "auto", verbose = FALSE)

## networkStat(net_mat)

## g <- networkStat(net_mat)$graph
## betweenness(g,  directed = FALSE, normalized = FALSE) %>% mean(na.rm = TRUE) %>% round(4)#%*% max()

set.seed(42)
net_plt %>%
  .$graph %>%
  tidygraph::as_tbl_graph(directed = FALSE) %>%
  mutate(surname = str_replace(name, "\\s+[^ ]+$", "")) %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_link0(edge_width = 0.6, edge_colour = "gray66") +
  geom_node_point(aes(size = deg), shape = 21, fill = "lightsteelblue2", color = "gray25") +
  geom_node_label(aes(filter = (deg > 5), label = surname), repel = TRUE, size = 2, max.overlaps = Inf,
                  label.padding = unit(0.2, "lines")) +
  theme_graph() +
  theme(legend.position = "none")

ggsave("co-authors-net.pdf", width = 120, height = 120, units = "mm")
system("pdfcrop co-authors-net.pdf")
system("convert -density 300 co-authors-net-crop.pdf co-authors-net-crop.png")



## net_plt %>%
##   .$graph %>%
##   tidygraph::as_tbl_graph(directed = FALSE) %>%
##   activate(nodes)





## Top authors' production over time

res <- authorProdOverTime(M0, k = 10, graph = FALSE)

df2 <- res$dfAU %>%
  as_tibble() %>%
  mutate(name = str_to_title(Author) %>% as_factor())


df2 %>%
  group_by(name) %>%
  summarise(freq_sum = sum(freq)) %>%
  mutate(name = fct_reorder(name, freq_sum, desc)) %>% pull(name) -> name_fct

ggplot(df2, aes(x = .data$name, y = .data$year)) +
  geom_line(data = df2, aes(x = .data$name, y = .data$year, group = .data$name),
            size = 0.6, color = "gray66") +
  geom_point(aes(size = .data$freq), shape = 21, color = "black", fill = "lightsteelblue2") +
  scale_size(range = c(2, 4)) +
  scale_y_continuous(breaks = seq(min(df2$year), max(df2$year), by = 2)) +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 0)) +
    labs(x = "Author", y = "Year") +
  scale_x_discrete(limits = rev(levels(name_fct))) +
    coord_flip()

##ggsave("authors-over-time.png", width = 120, height = 120, units = "mm")
ggsave("authors-over-time.pdf", width = 120, height = 80, units = "mm")
system("pdfcrop authors-over-time.pdf")
system("convert -density 300 authors-over-time-crop.pdf authors-over-time-crop.png")







## print(res$dfAU)
## plot(res$graph)
## ggsave("authors-in-time.png")






## data <- res$dfAU %>%
##     mutate(Author = str_to_title(Author))

## plt <- ggplot(data, aes(x = Author, y =  year)) +
##     geom_line(aes(x = Author, y = year, group = Author),size = 0.5, color = "gray", alpha=0.3) +
##     geom_point(aes(size = freq), color = "firebrick") +
##     scale_y_continuous(breaks = seq(min(data$year), max(data$year), by = 2)) +
##   ##scale_x_discrete(limits = rev(levels(data$Author))) +
##   scale_x_discrete(limits = levels(data$Author)) +
##     labs(x = "Author", y = "Year") +
##     coord_flip() +
##     theme_bw() +
##   theme(axis.line = element_line(colour = "black"),
##         axis.title = element_text(size = 14),
##         axis.text = element_text(size = 14),
##           panel.grid.major = element_blank(),
##           panel.grid.minor = element_blank(),
##           panel.border = element_blank(),
##           panel.background = element_rect(fill = "transparent"),
##           plot.background = element_rect(fill = "transparent", color = NA),
##           legend.position = "none",
##           legend.background = element_rect(fill = "transparent"),
##           legend.box.background = element_rect(fill = "transparent"))

## ggsave("authors_over_time.pdf", plt, width = 8, height = 5, bg = "transparent")




## ## Institutions

## metaTagExtraction(M0, Field = "AU1_CO", sep = ";")
