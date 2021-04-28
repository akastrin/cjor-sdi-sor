library(tidyverse)
library(tidygraph)
library(ggraph)
library(bibliometrix)
library(akc)

source("utils.R")


## Download data from Zenodo
id <- "4723292"
zenodo_dl(id)

## Read data files
file_wos <- "./data/savedrecs-2021-04-14.txt"
file_dim <- "./data/Dimensions-Publication-2021-04-14_09-13-36.xlsx"

## Convert to data.frame
M_wos <- convert2df(file = file_wos, dbsource = "isi", format = "plaintext")
M_dim <- convert2df(file = file_dim, dbsource = "dimensions", format = "excel")
M <- mergeDbSources(M_wos, M_dim, remove.duplicated = TRUE)

## Manually replace NA values in the PY field
M %>%
  replace_na(list(PY = 2021)) -> M0

## Run analysis
results <- biblioAnalysis(M0, sep = ";")

## Summary
S <- summary(object = results, k = 10, pause = FALSE)

## Top authors' production over time (Figure 1)
res <- authorProdOverTime(M0, k = 10, graph = FALSE)

df2 <- res$dfAU %>%
  as_tibble() %>%
  mutate(name = str_to_title(Author) %>% as_factor())

df2 %>%
  group_by(name) %>%
  summarise(freq_sum = sum(freq)) %>%
  mutate(name = fct_reorder(name, freq_sum, desc)) %>% pull(name) -> name_fct

plt <- ggplot(df2, aes(x = .data$name, y = .data$year)) +
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

ggsave("fig-1.pdf", width = 120, height = 80, units = "mm")
ggsave("fig-1.png", width = 120, height = 80, units = "mm")
# system("pdfcrop fig-1.pdf")
# system("convert -density 300 fig-1-crop.pdf fig-1-crop.png")

## Co-authorship network (Figure 2)
net_mat <- biblioNetwork(M0, n = NULL, analysis = "collaboration", network = "authors", sep = ";")
net_plt <- networkPlot(net_mat, degree = NULL, weighted = TRUE, type = "auto", verbose = FALSE)

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

ggsave("fig-2.pdf", width = 120, height = 120, units = "mm")
ggsave("fig-2.png", width = 120, height = 120, units = "mm")
# system("pdfcrop fig-2.pdf")
# system("convert -density 300 fig-2-crop.pdf fig-2-crop.png")

## Keyword co-occurrence network (Figure 3)
M0 %>%
  as_tibble() %>%
  mutate(id = row_number()) %>%
  select(id, TI, ID, DE, AB) %>%
  unite("KW", ID:DE, remove = TRUE, sep = "; ", na.rm = TRUE) %>%
  unite("TX", c(TI, AB), remove = TRUE, sep = ".", na.rm = TRUE) %>%
  select(id, KW, TX) %>%
  rename(keyword = KW, abstract = TX) -> bib_tbl3

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

## Prepare object for strategic diagram
minfreq <- 5
minfreq <- max(2,floor(minfreq * nrow(M) / 1000))

set.seed(20210425)
g <- grouped_keywords_3

as_adjacency_matrix(g, attr = "n", sparse = FALSE) -> NetMatrix

net <- g
group <- net %>% activate(nodes) %>% pull(group)
word <- net %>% activate(nodes) %>% pull(name)
# color=V(net)$color
# color[is.na(color)]="#D3D3D3"

W <- intersect(row.names(NetMatrix), word)
index <- which(row.names(NetMatrix) %in% W)
ii <- which(word %in% W)
word <- word[ii]
group <- group[ii]
color <- color[ii]

C <- net %>% activate(nodes) %>% pull(freq)

net_mat <- NetMatrix
diag(net_mat) <- net %>% activate(nodes) %>% pull(freq)
D <- diag(net_mat)
S <- net_mat / ((outer(D, D, "*")))

sEij <- S[index, index]
sC <- (C[index])

## Centrality and density
label_cluster <- unique(group)
word_cluster <- word[group]
centrality <- c()
density <- c()
labels <- list()

df_lab <- data.frame(sC = sC, words = word, groups = group,
                     cluster_label = "NA", stringsAsFactors = FALSE)

for (i in label_cluster) {
  ind <- which(group == i)
  w <- df_lab$words[ind]
  wi <- which.max(df_lab$sC[ind])
  df_lab$cluster_label[ind] <- paste(w[wi[1 : min(c(length(wi), 3))]], collapse = ";", sep = "")
  centrality <- c(centrality, sum(sEij[ind, -ind]))
  density <- c(density, sum(sEij[ind, ind]) / length(ind) * 100)
  df_lab_g <- df_lab[ind, ]
  df_lab_g <- df_lab_g[order(df_lab_g$sC, decreasing = TRUE), ]
  k <- 1
  labels[[length(labels) + 1]] <- paste(df_lab_g$words[1 : k], collapse = ";")
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

W <- df_lab %>% group_by(.data$groups) %>%
  arrange(-.data$sC, .by_group = TRUE) %>%
  dplyr::top_n(10, .data$sC) %>%
  summarise(wordlist = paste(.data$words, .data$sC, collapse = "\n")) %>% as.data.frame()

df$words <- W[, 2]

## Labels for each cluster
labels <- gsub("\\d", "", df$words)

## Cut ties over 10 words
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

# df$name_full <- L

## Replace original labels with custom labels
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
ggsave("fig-3.pdf", width = 120, height = 120, units = "mm")
ggsave("fig-3.png", width = 120, height = 120, units = "mm")
# system("pdfcrop fig-3.pdf")
# system("convert -density 300 fig-3-crop.pdf fig-3-crop.png")

## Labels exploration
## g2df_3 <- df
## g2df_3 %>%
##   select(label, words) %>%
##   separate_rows(words, sep = "\n") %>%
##   separate(col = words, into = c("term", "freq"), sep = "\\s+(?=\\d+$)", convert = TRUE) %>%
##   group_by(label) %>%
##   top_n(20) %>%
##   print(n = Inf)
