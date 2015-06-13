######
# Check stability of 3-cluster solution
##
library(tidyr)
library(dplyr)
library(ggplot2)

fc_seed <- 123
num_clusters <- 3
num_trys <- 100
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
  select(k, seed, clust_rank, size) %>%
  filter(clust_rank <= 2) %>%
  mutate(clust_label = paste0("Size_", clust_rank)) %>%
  select(-clust_rank) %>%
  spread(key = clust_label, value = size)

ggplot(cli_sizes, aes(Size_1, Size_2)) +
  geom_point(alpha = 0.5, size = 4) +
  stat_density2d() +







