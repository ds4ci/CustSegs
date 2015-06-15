######
# Check stability of 3-cluster solution
##
library(tidyr)
library(dplyr)
library(ggplot2)

fc_seed <- 123
num_clusters <- 3
num_trys <- 500
fc_cont@verbose <- 0
cli_trys <- NULL

# build df with cluster info for each seed
for (itry in 1:num_trys) {
  fc_seed <- fc_seed + 1
  set.seed(fc_seed)
  cli <- kcca(vol.mat, k = num_clusters, save.data = TRUE,
              control = fc_cont, family = kccaFamily(fc_family))
  cli_info <- cli@clusinfo %>%
    mutate(clust_num = row_number(),
           clust_rank = min_rank(desc(size))) %>%
    arrange(clust_rank) %>%
    select(c(6, 5, 1:4))

  cli_try <- cbind(data.frame(k = num_clusters, seed = fc_seed),
                   cli_info)
  cli_trys <- rbind(cli_trys, cli_try)
}

# set up plot of size of rank 2 x rank 1
cli_sizes <- cli_trys %>%
  dplyr::select(k, seed, clust_num, clust_rank, size) %>%
  filter(clust_rank <= 2) %>%
  mutate(clust_label = paste0("Size_", clust_rank)) %>%
  dplyr::select(-clust_rank) %>%
  spread(key = clust_label, value = size) %>%
  group_by(k, seed) %>%
  summarize(c1 = first(clust_num),
            c2 = last(clust_num),
            Size_1 = min(Size_1, na.rm = TRUE),
            Size_2 = min(Size_2, na.rm = TRUE))


# get location of peak
s2d <- with(cli_sizes, kde2d(Size_1, Size_2, n = 100))
s2d_peak <- which(s2d$z == max(s2d$z))
Size_1_peak_at <- round(s2d$x[s2d_peak %% 100], 1)
xend <- Size_1_peak_at + 100
Size_2_peak_at <- round(s2d$y[s2d_peak %/% 100], 1)
yend <- Size_2_peak_at + 100

ggplot(cli_sizes, aes(Size_1, Size_2)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_density2d() +
  annotate("segment", x = Size_1_peak_at, y = Size_2_peak_at,
           xend = xend, yend = yend, color = "red", size = 1) +
  annotate("text", xend, yend, label = paste0("(", Size_1_peak_at, ", ", Size_2_peak_at, ")"), vjust = 0) +
  ggtitle(paste0("Size of Cluster 2 by Size of Cluster 1 for k=", num_clusters, ", # tries=", num_trys))

## reading off of plot, pick seeds near one of three maxima with cluster rank = order
#  Top: @(1075, 255): 169
#  2nd: @(940, 385): 215
#  3rd: @(1250, 90): 129

# find closest row in cli_trys (with rank == num)

cli_best <- cli_sizes %>%
  filter(c1 == 1 & c2 == 2) %>%    ## just look at solutions with clusters in decending sizes
  mutate(distance = sqrt((Size_1 - Size_1_peak_at)^2 + (Size_2 - Size_2_peak_at)^2)) %>%
  dplyr::select(-starts_with("c")) %>%
  arrange(distance)

cli_best




