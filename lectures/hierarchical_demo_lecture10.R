
#
# food6 <- read_csv("https://massey.ac.nz/~anhsmith/data/food.csv") |>
#   column_to_rownames(var="Country") |>
#   filter(
#     rownames(food_norm) %in%
#       c("Yugoslavia","Romania","Bulgaria",
#         "Albania","Italy","Greece")
#   ) |>
#   select(RedMeat, WhiteMeat)


ymax_dend <- -1.5

# Massey colours
 # colm <- c("#004B8D", "#E4A024", "#9A3324", "#5E6738", "#D45D00", "#83786F",
 #           "#A8AD00", "#C5B783", "#4F758B", "#98A4AE", "#0090E9")
#

food6_distmat <- food6 |> dist(method = dist_type) |> round(2) |> as.matrix() |> as.data.frame()

distmat0 <- food6_distmat |>
  replace_upper_triangle(".") |>
  slice(-1) |> select(-last_col()) |>
  column_to_rownames(var="rowname")


scat0 <- food6 |>
  ggplot() +
  aes(x=RedMeat,y=WhiteMeat) +
  geom_point(size=2) +
  geom_text(aes(label=rownames(food6)),
            nudge_x = c( .8,  0, -.7, -.7,  0.3,  0.4 ),
            nudge_y = c(-.3, .3,   0,   0,  0.3,  0.3 )
            ) +
  coord_equal()


## Scatterplot with all pairs connected

combos <- combn(rownames(food6),2)
segs <- data.frame(
  x =    food6[combos[1,],1],
  xend = food6[combos[2,],1],
  y =    food6[combos[1,],2],
  yend = food6[combos[2,],2]
)

scat_all <- scat0 +
    geom_segment(
      data=segs,
      aes(x=x,xend=xend,
          y=y,yend=yend),
      inherit.aes = FALSE,
      col=colm[6]
      )



dend0 <- food6_distmat |> as.dist() |> hclust(method = "average") |>
  fviz_dend(horiz=T, label_cols = "black",
            k_colors = 2,
            palette = c("white"),
            col_labels_by_k=F) +
  ylim(food6 |> dist(method = dist_type) |> max(), ymax_dend) +
  ylab("Distance") +
  ggtitle("")


scat_bi <- scat0 +
  geom_segment(aes(x = food6["Bulgaria","RedMeat"], xend=food6["Italy","RedMeat"],
                   y = food6["Bulgaria","WhiteMeat"], yend=food6["Italy","WhiteMeat"]),
               lwd=1.2,
               col=colm[2])


dend_bi <- dend0 +
  geom_path(aes(y=c(0,food6_distmat["Bulgaria","Italy"],food6_distmat["Bulgaria","Italy"],0), #
                x=c(3,3,4,4)),
            col=colm[2],
            lwd=2)


scat_bi_ga <- scat_bi +
  geom_segment(aes(x = food6["Greece","RedMeat"], xend=food6["Albania","RedMeat"],
                   y = food6["Greece","WhiteMeat"], yend=food6["Albania","WhiteMeat"]),
               lwd=1.2,
               col=colm[3])

dend_bi_ga <- dend_bi +
  geom_path(aes(y=c(0,food6_distmat["Greece","Albania"],food6_distmat["Greece","Albania"],0), #
                x=c(1,1,2,2)),
            col=colm[3],
            lwd=2)


scat_bi_ga_yr <- scat_bi_ga +
  geom_segment(aes(x = food6["Yugoslavia","RedMeat"], xend=food6["Romania","RedMeat"],
                   y = food6["Yugoslavia","WhiteMeat"], yend=food6["Romania","WhiteMeat"]),
               lwd=1.2,
               col=colm[4])

dend_bi_ga_yr <- dend_bi_ga +
  geom_path(aes(y=c(0,food6_distmat["Yugoslavia","Romania"],
                    food6_distmat["Yugoslavia","Romania"],0), #
                x=c(5,5,6,6)),
            col=colm[4],
            lwd=2)


f_bi_yr = data.frame(
  RedMeat = c(
    mean(food6[c("Yugoslavia","Romania"),"RedMeat"]),
    mean(food6[c("Bulgaria","Italy"),"RedMeat"])
    ),
  WhiteMeat = c(
    mean(food6[c("Yugoslavia","Romania"),"WhiteMeat"]),
    mean(food6[c("Bulgaria","Italy"),"WhiteMeat"])
    )
  )
scat_biyr_ga <- scat_bi_ga_yr +
  geom_path(data=f_bi_yr,
            aes(x = RedMeat,
                y = WhiteMeat),
            lwd=1.2,
            col=colm[5])

dist_bi_yr <- mean(
  c(food6_distmat["Yugoslavia","Italy"],
    food6_distmat["Yugoslavia","Bulgaria"],
    food6_distmat["Romania","Italy"],
    food6_distmat["Romania","Bulgaria"])
)

dend_biyr_ga <- dend_bi_ga_yr +
  geom_path(aes(y=c(food6_distmat["Bulgaria","Italy"],
                    dist_bi_yr,dist_bi_yr,
                    food6_distmat["Yugoslavia","Romania"]), #
                x=c(3.5,3.5,5.5,5.5)),
            col=colm[5],
            lwd=2)


scat_biyrga <- scat_biyr_ga +
  geom_segment(aes(x   =  mean(food6[c("Yugoslavia","Romania","Bulgaria","Italy"),"RedMeat"]),
                   xend = mean(food6[c("Greece","Albania"),"RedMeat"]),
                   y =    mean(food6[c("Yugoslavia","Romania","Bulgaria","Italy"),"WhiteMeat"]),
                   yend = mean(food6[c("Greece","Albania"),"WhiteMeat"])
  ),
  lwd=1.2,
  col=colm[9])

dist_biyr_ga <- mean(
  c(food6_distmat["Bulgaria","Greece"],
    food6_distmat["Bulgaria","Albania"],
    food6_distmat["Italy","Greece"],
    food6_distmat["Italy","Albania"],
    food6_distmat["Yugoslavia","Greece"],
    food6_distmat["Yugoslavia","Albania"],
    food6_distmat["Romania","Greece"],
    food6_distmat["Romania","Albania"])
)

dend_biyrga <- dend_biyr_ga +
  geom_path(aes(y=c(food6_distmat["Greece","Albania"],
                    dist_biyr_ga, dist_biyr_ga,
                    dist_bi_yr), #
                x=c(1.5,1.5,4.5,4.5)),
            col=colm[9],
            lwd=2)



## adapt distance matrix to _bi

dim_distmat0 <- dimnames(distmat0)

distmat_H <- distmat0
distmat_H["Bulgaria","Italy"] <- cell_spec(distmat_H["Bulgaria","Italy"], background = colm[2])

dist_bi_others <- c(
  Yugoslavia = mean(c(food6_distmat["Yugoslavia","Italy"],food6_distmat["Yugoslavia","Bulgaria"])),
  Romania = mean(c(food6_distmat["Romania","Italy"],food6_distmat["Romania","Bulgaria"])),
  Greece = mean(c(food6_distmat["Greece","Italy"],food6_distmat["Greece","Bulgaria"])),
  Albania = mean(c(food6_distmat["Albania","Italy"],food6_distmat["Albania","Bulgaria"]))
)
distmat_bi <- rbind(
  distmat0[dim_distmat0[[1]][1:3],dim_distmat0[[2]][1:4]],
  "Ita+Bul" = dist_bi_others
)


## adapt distance matrix to _ga

distmat_bi_H <- distmat_bi
distmat_bi_H["Albania","Greece"] <- cell_spec(distmat_bi_H["Albania","Greece"], background = colm[3])

distmat_bi_ga <- tibble(
    rowname=c("Romania","Gre+Alb","Ita+Bul"),
    Yugoslavia = c(Romania =   distmat_bi["Romania","Yugoslavia"],
                   `Gre+Alb` = distmat_bi[c("Greece","Albania"), "Yugoslavia"] |> as.numeric() |> mean() |> round(2),
                   `Ita+Bul` = distmat_bi["Ita+Bul", "Yugoslavia"] |> as.numeric() ),
    Romania = c(Romania   = ".",
               `Gre+Alb` = distmat_bi[c("Greece","Albania"), "Romania"] |> as.numeric() |> mean() |> round(2),
               `Ita+Bul` = distmat_bi["Ita+Bul", "Romania"] |> as.numeric() ),
    `Gre+Alb` = c(Romania   = ".",
                  `Gre+Alb` = ".",
                  `Ita+Bul` = distmat_bi["Ita+Bul",c("Greece","Albania")] |> as.numeric() |> mean() |> round(2))
    ) |> column_to_rownames()


## adapt distance matrix to _yr

# Highlight
distmat_bi_ga_H <- distmat_bi_ga
distmat_bi_ga_H["Romania","Yugoslavia"] <- cell_spec(distmat_bi_ga_H["Romania","Yugoslavia"], background = colm[4])

# Make new distmat
distmat_bi_ga_yr <- tibble(
  rowname=c("Gre+Alb","Ita+Bul"),
  `Yug+Rom` = c(`Gre+Alb` = distmat_bi_ga["Gre+Alb", c("Yugoslavia","Romania")] |> as.numeric() |> mean() |> round(2),
                `Ita+Bul` = distmat_bi_ga["Ita+Bul", c("Yugoslavia","Romania")] |> as.numeric() |> mean() |> round(2) |> as.character()),
  `Gre+Alb` = c(`Gre+Alb` = ".",
                `Ita+Bul` = distmat_bi_ga["Ita+Bul", "Gre+Alb"])
  ) |> column_to_rownames()


#
distmat_bi_ga_yr_H <- distmat_bi_ga_yr
distmat_bi_ga_yr_H["Ita+Bul","Yug+Rom"] <- cell_spec(distmat_bi_ga_yr_H["Ita+Bul","Yug+Rom"],
                                                     background = colm[5])


## adapt distance matrix to _biyr_ga

# Highlight

# Make new distmat
distmat_biyr_ga <- tibble(
  rowname = "Gre+Alb",
  `Yug+Rom+Ita+Bul` = c(distmat_bi_ga_yr[1,1],distmat_bi_ga_yr[2,2]) |> as.numeric() |> mean() |> as.character()
) |> column_to_rownames()

#
distmat_biyr_ga_H <- distmat_biyr_ga
distmat_biyr_ga_H["Gre+Alb","Yug+Rom+Ita+Bul"] <- cell_spec(distmat_biyr_ga["Gre+Alb","Yug+Rom+Ita+Bul"], background = colm[9])





# library("FactoMineR")
# pca_food6 <- food6 |> PCA(graph = FALSE)
#
# fviz_pca_ind(pca_food6, repel = TRUE) +
#   ggtitle("")


# library(modelsummary)
#
# dist_fun <- function(x) {
#   out <- dist(x, diag = T, upper = T) |>
#     as.matrix() |> as.data.frame()
#   datasummary_correlation_format(
#     out,
#     fmt = 2,
#     upper_triangle = ".",
#     diagonal = ".")
# }
#
# food6 |> datasummary_correlation(method=dist_fun) |>
#   kable_styling(font_size = 16)
