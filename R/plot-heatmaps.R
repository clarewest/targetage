#' Plot Heatmap
#'
#' Plots a heatmap of the associations for an individual gene
#'
#' @param associations dataframe. The target-disease associations in wide
#'    format, in which each row is a target
#' @param gene_code string. Gene code for the gene of interest.
#' @param morbidities_ignore vector. Morbidities to ignore when plotting
#' @param save logical. If true, the plot is saved, with the default name
#' @return Returns a heatmap showing the association of a gene with each morbidity
#' @examples
#' plot_heatmap("APOE")
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate select filter left_join
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot
#' @export
plot_heatmap <-
  function(associations,
           gene_code,
           save = FALSE,
           morbidities_ignore = NULL) {
    ## Check that the gene is in the dataframe and get the target id
    targetid = subset(associations, target.gene_info.symbol == gene_code)$target.id
    if (length(targetid) < 1) {
      stop("Gene not found")
    }
    ## get the target
    gene <- associations %>% dplyr::filter(target.id == targetid)
    ## reshape to long format
    gene_plot_df <- gene %>%
      dplyr::select(target.id, contains("association_score"))  %>%
      pivot_longer(contains("association_score")) %>%
      separate(
        col = name,
        c("morbidity", "score", "blah", "datatype"),
        sep = "\\.",
        fill = "right"
      ) %>%
      mutate(datatype = ifelse(is.na(datatype), "overall", datatype))
    gene_plot_df <- gene_plot_df %>%
      mutate(morbidity = factor(morbidity, levels = c(
        sort(unique(gene_plot_df$morbidity[gene_plot_df$morbidity != "longevity"])), "longevity")) ## alphabetical order for morbidities, but with longevity on the end
      )
    ## make a heatmap
    textcol <- "grey40"
    gene_heatmap <-
      gene_plot_df %>%
      ## filter out morbidities specified for ignoring
      dplyr::filter(!morbidity %in% morbidities_ignore) %>%
      mutate(
        datatype = factor(datatype, levels = unique(gene_plot_df$datatype)),
        morbidity = factor(morbidity)
      ) %>%
      filter(datatype != "affected_pathway") %>% ## ignore systems biology associations
      ggplot(., aes(x = morbidity, y = datatype, fill = value)) +
      geom_tile(colour = "black", size = 0.3) +
      coord_equal() +
      theme_bw(base_size = 10) +
      scale_y_discrete(expand = c(0, 0)) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_fill_gradient(
        "score",
        low = "white",
        high = "steelblue",
        na.value = 'gray90',
        limits = c(0, 1),
        guide = guide_colorbar(frame.colour = "black", frame.linewidth = 0.8)
      ) +
      theme(
        axis.text.x = element_text(
          colour = textcol,
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(vjust = 0.2, colour = textcol),
        axis.ticks = element_line(size = 0.4),
        axis.title = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
        plot.title = element_text(
          colour = textcol,
          hjust = 0,
          size = 14,
          face = "bold"
        ),
        panel.border = element_rect(colour = "black", size = 0.8)
      )
    if (save == TRUE) {
      filename = paste0(gene_code, "_heatmap.png")
      ggsave(
        gene_heatmap,
        file = filename,
        width = 4.5,
        height = 3.5,
        dpi = 600
      )
    }
    else{
      return(gene_heatmap)
    }
  }

#' Plot Multi-Heatmap
#'
#' Plots a heatmap of the associations for a subset of genes
#'
#' @param associations dataframe. The target-disease associations in wide
#'    format, in which each row is a target-disease association
#' @param morbidities_ignore vector. Morbidities to ignore when plotting
#' @param save logical. If true, the plot is saved as a png
#' @return Returns a heatmap showing the overall association score of each
#'  target with each morbidity
#' @examples
#' plot_multiheatmap(some_associations)
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate select filter left_join
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot
#' @export
plot_multiheatmap <- function(associations, save = FALSE, sort_nmorb = FALSE, gene_order = NULL) {
  textcol <- "grey40"
  df <- associations %>%
    dplyr::select(starts_with("target"), contains("association_score.overall")) %>%
    pivot_longer(cols = contains("association_score.overall")) %>%
    mutate_if(is.character, stringr::str_replace, pattern = ".association_score.overall",replacement = "")
  df <- df %>%
    mutate(name = factor(name, levels = c(
      sort(unique(df$name[df$name != "longevity"])), "longevity")) ## alphabetical order for morbidities, but with longevity on the end
      )
  if (sort_nmorb == TRUE){
    nsum_order <-
      associations %>%
      dplyr::select(starts_with("target"), contains("association_score.overall")) %>%
      mutate_if(is.numeric, funs(ifelse(.>0, 1, 0))) %>%
      mutate(nsum = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
      arrange(nsum) %>%
      pull(target.gene_info.name)
    print(length(nsum_order))
    print(nsum_order[duplicated(nsum_order)])
    df <- df %>%
      mutate(target.gene_info.name = factor(target.gene_info.name, levels = unique(nsum_order)))
  }
  if (! missing(gene_order)){
    name_order <- df %>% arrange(match(target.gene_info.symbol, gene_order)) %>% pull(target.gene_info.name)
    df <- df %>%
      mutate(target.gene_info.name = factor(target.gene_info.name, levels = unique(name_order)))
  }
  heatmap <-
    ggplot(df, aes(x = name, y = target.gene_info.name, fill = value)) +
    geom_tile(colour = "black", size = 0.3) +
    coord_equal() +
    theme_bw(base_size = 10) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_gradient(
      "score",
      limits = c(0,1),
      low = "white",
      high = "steelblue",
      na.value = 'gray90',
      guide = guide_colorbar(frame.colour = "black", frame.linewidth = 0.8)
    ) +
    guides(fill = FALSE) +
    theme(
      axis.text.x = element_text(
        colour = textcol,
        angle = 45,
        hjust = 1
      ),
      axis.text.y = element_text(vjust = 0.2, colour = textcol),
      axis.ticks = element_line(size = 0.4),
      axis.title = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
      plot.title = element_text(
        colour = textcol,
        hjust = 0,
        size = 14,
        face = "bold"
      ),
      panel.border = element_rect(colour = "black", size = 0.8)
    )
  if (save == TRUE){
    ggsave(heatmap, file = "genage_heatmap.png", width=7, height=4, dpi=900)
  }
  else{
    return(heatmap)
  }
}
