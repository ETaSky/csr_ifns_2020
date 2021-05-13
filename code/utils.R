theme_jw <- function(base_size=10) {
    require(grid)
    require(ggthemes)
    (theme_foundation(base_size=base_size)
        + theme(text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(face = "bold", size = rel(1), color = "black"),
                axis.title.y = element_text(angle=90, vjust =2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(size = rel(0.8), color = "black"),
                axis.line = element_line(color = "black", size = 0.2),
                axis.ticks = element_line(color = "black", size = 0.2),
                panel.grid.major = element_line(color = "grey", size = 0.1),
                panel.grid.minor = element_line(color = "grey", size = 0.1),
                legend.key = element_rect(colour = NA),
                legend.position = "right",
                legend.direction = "vertical",
                legend.key.size= unit(0.4, "cm"),
                legend.title = element_text(face="bold.italic", size = rel(1.0)),
                legend.text=element_text(size = rel(1.0)),
                plot.margin=unit(c(1,0.5,0.5,0.5),"lines"),
                plot.title = element_text(hjust = 0, vjust = 1, size = rel(1.2), face = "bold"), 
                plot.title.position = "panel",
                strip.background=element_rect(colour="black",fill="black"),
                strip.text = element_text(face="bold", color = "white", size = rel(1))
        ))
}

merge_iter <- function(x, by){
    # This function is used to iteratively merge dataframe of the same structure from a list
    #
    # Arguments:
    #   x, the list storing the dataframes
    #   by, either a single value indicating the column that will be used as key column by all dataframes; or a vector of the same length of x, which stores the number of key column.
    #
    # Return:
    #   A merged dataframe.
    ll <- length(x)
    if (length(by) > 1 & length(by) != ll){
        return("Error in by")
    } else if (length(by)==1){
        if (length(x)<=2){
            t <- merge(x[[1]], x[[2]], by = by)
            return(t)
        } else {
            t <- merge(x[[1]], merge_iter(x[-1], by = by), by.x = by, by.y = 1)
            return(t)
        }
    } else {
        # TBA
    }

}

set_panel_size <- function (p = last_plot(), g = ggplot2::ggplotGrob(p), file = NULL, margin = unit(1, "mm"), width = unit(4, "cm"), height = unit(4, "cm"), panel.size = T, msg = F, bitmap = "png", useDingbats = F, ...) {
    # This function is used to fine-tube the size of the panel
    #
    # Args:
    #   p: the ggplot object
    #   g: the ggplotGrob table for p, usually this parameter should not be changed
    #   file: file path
    #   margin: page margin, a unit object
    #   width: the exact unit object
    #   height: the exact unit object
    #   panel.size: whether the width and height is panel size or the overall graph
    #   msg: whether to print the grob table
    #   bitmap: whether to also export an exact version of bitmap: "png", "tiff", NULL
    #   ...: other ggsave parameters
    panels <- grep("panel", g$layout$name)
    panel_index_w <- unique(g$layout$l[panels])
    panel_index_h <- unique(g$layout$t[panels])
    nw <- length(panel_index_w)
    nh <- length(panel_index_h)
    if (panel.size){
        if (length(width)==1 & length(height)==1){
            g$widths[panel_index_w] <- rep(width, nw)
            g$heights[panel_index_h] <- rep(height, nh)
            if (!is.null(file)) {
                ggplot2::ggsave(file, g, width = grid::convertWidth(sum(g$widths) + 2 * margin, unitTo = "in", valueOnly = TRUE), height = grid::convertHeight(sum(g$heights) + 2 * margin, unitTo = "in", valueOnly = TRUE), useDingbats = useDingbats, ...)
                if (!is.null(bitmap)) {
                    ggplot2::ggsave(paste0(tools::file_path_sans_ext(file), ".", bitmap), g, width = grid::convertWidth(sum(g$widths) + 2 * margin, unitTo = "in", valueOnly = TRUE), height = grid::convertHeight(sum(g$heights) + 2 * margin, unitTo = "in", valueOnly = TRUE),  dpi = 600, ...)
                }
            }
        }
        else{
            g$widths[panel_index_w] <- width
            g$heights[panel_index_h] <- height
            if (!is.null(file)) {
                ggplot2::ggsave(file, g, width = grid::convertWidth(sum(g$widths) + 2 * margin, unitTo = "in", valueOnly = TRUE), height = grid::convertHeight(sum(g$heights) + 2 * margin, unitTo = "in", valueOnly = TRUE), useDingbats = useDingbats, ...)
                if (!is.null(bitmap)) {
                    ggplot2::ggsave(paste0(tools::file_path_sans_ext(file), ".", bitmap), g, width = grid::convertWidth(sum(g$widths) + 2 * margin, unitTo = "in", valueOnly = TRUE), height = grid::convertHeight(sum(g$heights) + 2 * margin, unitTo = "in", valueOnly = TRUE),  dpi = 600, ...)
                }
            }
        }
    }
    else{
        sum_panel_width = convertWidth(width - sum(g$widths[-panel_index_w]) - 2 * margin, unitTo = "in", valueOnly = TRUE)
        sum_panel_height = convertWidth(height - sum(g$heights[-panel_index_h]) - 2 * margin, unitTo = "in", valueOnly = TRUE)
        panel_width_r <- as.numeric(g$widths[panel_index_w])
        panel_height_r <- as.numeric(g$heights[panel_index_h])
        g$widths[panel_index_w] <- unit(panel_width_r/sum(panel_width_r)*sum_panel_width, units = "in")
        g$heights[panel_index_h] <- unit(panel_height_r/sum(panel_height_r)*sum_panel_height, units = "in")
        if (!is.null(file)) {
            ggplot2::ggsave(file, g, width = width, height = height, useDingbats = useDingbats, ...)
            if (!is.null(bitmap)) {
                ggplot2::ggsave(paste0(tools::file_path_sans_ext(file), ".", bitmap), g, width = grid::convertWidth(sum(g$widths) + 2 * margin, unitTo = "in", valueOnly = TRUE), height = grid::convertHeight(sum(g$heights) + 2 * margin, unitTo = "in", valueOnly = TRUE),  dpi = 600, ...)
            }
            print(g$widths[panel_index_w])
            print(g$heights[panel_index_h])
        }
    }
}

gg_color_hue <- function(n) {
    # https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

subset_dm_by_names <- function(DM, Sname){
    # Extract the distance matrix using the sample names of a subset of the samples
    #
    # Args:
    #   DM: the distance matrix as a dist class
    #   Sname: a vector of the sample names
    #
    # Returns:
    #   The extracted distance matrix as a class dist object
    DM_mat <- DM %>% as.matrix
    DM_sname <- DM_mat %>% row.names()
    tmp <- match(Sname, DM_sname)
    Exist <- tmp[!is.na(tmp)]
    NoExist <- Sname[which(is.na(tmp))]
    print("Following samples do not exist in the distance matrix:")
    print(NoExist)
    DM <- DM_mat[Exist, Exist] %>% as.dist()
    return(DM)
}

compare_group_dist <- function(dist_mat, metadata, key, by, q = 4){
    # This function organize a distance object into a dataframe, in which the between group and within group distance are recoganized
    # Arguments:
    #   dist_mat, the distance matrix, dist object
    #   metadata, the associated metadata
    #   key, the column in metadata that matches the names of the dist_mat
    #   by, the grouping variable
    require(broom)
    require(dplyr)
    dist_pairs <- tidy(dist_mat)
    dist_pairs$Group_comp = ""
    dist_pairs <- as.matrix(dist_pairs)
    if (Sys.info()[['sysname']]=="Windows"){
        Group_comp = unlist(lapply(seq(nrow(dist_pairs)), function(x){paste(c(get_group_name(dist_pairs[x, 1], metadata, key, by), get_group_name(dist_pairs[x, 2], metadata, key, by)) %>% sort, collapse = "_")}))
    } else {
        Group_comp = unlist(parallel::mclapply(seq(nrow(dist_pairs)), function(x){paste(c(get_group_name(dist_pairs[x, 1], metadata, key, by), get_group_name(dist_pairs[x, 2], metadata, key, by)) %>% sort, collapse = "_")}, mc.cores = q))
    }
    dist_pairs <- data.frame(dist_pairs, stringsAsFactors = F)
    dist_pairs$distance <- as.numeric(dist_pairs$distance)
    dist_pairs$Group_comp <- Group_comp
    return(dist_pairs)
}
get_group_name <- function(item, metadata, key, by){
    # This function returns the group value of the item
    # Argument:
    #   item, the value to be identified
    #   metadata,
    #   key, item belongs to key
    #   by, grouping variable
    return(metadata[, by][match(item, metadata[, key])])
}

reorder_row_values <- function(vec1, vec2, levels = NULL){
    if (is.null(levels)){
        levels = sort(unique(c(vec1, vec2)))
    }
    old = cbind(vec1, vec2)
    new = old
    for (i in seq(dim(old)[1])){
        new[i, ] = levels[levels %in% old[i, ]]
    }
    return(new)

}
