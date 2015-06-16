fc_rclust <- function(x, k, nrep=100, verbose=FALSE, FUN = kcca, seed=1234, plotme=TRUE){
  fc_seed = seed
  fc_tries <- NULL
  for (itry in 1:nrep) {
    fc_seed <- fc_seed + 1
    set.seed(fc_seed)
    cli <- flexclust::kcca(x, k, save.data = TRUE,
                           control = fc_cont, family = kccaFamily(fc_family))
    cli_info <- cli@clusinfo %>%
      mutate(clust_num = row_number(),
             clust_rank = min_rank(desc(size))) %>%
      arrange(clust_rank) %>%
      dplyr::select(c(6, 5, 1:4))
    cli_try <- cbind(data.frame(k = num_clusters, seed = fc_seed),
                     cli_info)
    cli_trys <- rbind(cli_trys, cli_try)
  }
  cli_trys <- as.tbl(cli_trys)

  cli_sizes <- cli_trys %>%
    dplyr::select(k, seed, clust_num, clust_rank, size) %>%
    filter(clust_rank <= 2) %>%
    mutate(clust_label = paste0("Size_", clust_rank),
           in_order = clust_num == clust_rank) %>%
    dplyr::select(-clust_rank, -clust_num) %>%
    spread(key = clust_label, value = size) %>%
    group_by(k, seed) %>%
    summarize(in_order = all(in_order),
              Size_1 = min(Size_1, na.rm = TRUE),
              Size_2 = min(Size_2, na.rm = TRUE))

  # get location of peak numerically with MASS:kde2d
  s2d <- with(cli_sizes, MASS::kde2d(Size_1, Size_2, n = 100))
  s2d_peak <- which(s2d$z == max(s2d$z))
  Size_1_peak_at <- round(s2d$x[s2d_peak %% 100], 1)
  Size_2_peak_at <- round(s2d$y[s2d_peak %/% 100], 1)

  if(plotme) {
    xend <- Size_1_peak_at + 100
    yend <- Size_2_peak_at + 100
    p <- ggplot2::ggplot(cli_sizes, aes(Size_1, Size_2)) +
      geom_point(alpha = 0.5, size = 2) +
      stat_density2d() +
      annotate("segment", x = Size_1_peak_at, y = Size_2_peak_at,
               xend = xend, yend = yend, color = "red", size = 1) +
      annotate("text", xend, yend,
               label = paste0("(", Size_1_peak_at, ", ", Size_2_peak_at, ")"), vjust = 0) +
      ggtitle(paste0("Size of Cluster 2 by Size of Cluster 1 for k=", k, ", # tries=", nrep))
    print(p)
  }

  cli_best <- cli_sizes %>%
    filter(in_order) %>%    ## just look at solutions with clusters in decending sizes
    mutate(distance = sqrt((Size_1 - Size_1_peak_at)^2 + (Size_2 - Size_2_peak_at)^2)) %>%
    arrange(distance)

  return(list(best = cli_best,
              sizes = cli_sizes,
              peak_at = c(Size_1_peak_at, Size_2_peak_at),
              tries = cli_trys))
}
