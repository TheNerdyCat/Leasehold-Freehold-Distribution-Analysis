
# Freehold/Leasehold and New/Existing Analysis - Split Violin Distribution



# We'll be using these packages
library(data.table)
library(readxl)
library(httr)
library(dplyr)
library(reshape)
library(ggplot2)
library(ggthemes)
library(scales)

# Set your working directory
setwd("D:/R/Freehold & leasehold, new & existing sales")

# Download data sets
urls <- c(freehold_new_url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/housepricestatisticsforsmallareasinenglandandwalessupplementarydatasets/dataset8medianfreeholdhousepricesnewlybuiltdwellings/hpssasupplementarydataset8medianpricepaidfreeholdnewdwellingsbyadmingeographies.xls",
          freehold_exi_url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/housepricestatisticsforsmallareasinenglandandwalessupplementarydatasets/dataset9medianfreeholdhousepricesexistingdwellings/hpssasupplementarydataset9medianpricepaidfreeholdexistingdwellingsbyadmingeographies.xls",
          leashold_new_url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/housepricestatisticsforsmallareasinenglandandwalessupplementarydatasets/dataset11medianleaseholdhousepricesnewlybuiltdwellings/hpssasupplementarydataset11medianpricepaidleaseholdnewdwellingsbyadmingeographies.xls",
          leashold_exi_url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/housepricestatisticsforsmallareasinenglandandwalessupplementarydatasets/dataset12medianleaseholdhousepricesexistingdwellings/hpssasupplementarydataset12medianpricepaidleaseholdexistingdwellingsbyadmingeographies.xls")
destinations <- c("D:/R/Freehold & leasehold, new & existing sales/freehold_new_url.xls",
                  "D:/R/Freehold & leasehold, new & existing sales/freehold_exi_url.xls",
                  "D:/R/Freehold & leasehold, new & existing sales/leasehold_new_url.xls",
                  "D:/R/Freehold & leasehold, new & existing sales/leasehold_exi_url.xls")
for(i in seq_along(urls)) {
  download.file(urls[i], destinations[i], mode = "wb")
} # Download all the files from ONS website. 
rm(i, urls, destinations)       # Remove these, we no longer need them 


# Import data sets
names <- c("freehold_new",
           "freehold_exi",
           "leasehold_new",
           "leasehold_exi")
for(i in seq_along(names)) {
  assign(names[i], 
         value = read_excel(paste(names[i], "_url.xls", sep = ""), 
                            sheet = "2a", 
                            col_names = FALSE))
} # Import all data sets and assign names
rm(i, names) # Don't need these anymore


# Tidy and merge data sets
tidy_df        <- function(x) {
  remove_rows    <- function(x) {
    x[-c(1:7, 356:361), ]
  } # Remove unwanted rows above and below data
  remove_cols    <- function(x) {
    x[ , c(2,4,92)]
  } # Remove unwanted variables
  rename_cols    <- function(x) {
    colnames(x) <- c("reg_name", "la_name", "purchase_price")
    x
  } # Rename columns
  replace_colon  <- function(x) {
    x[x == ":"] <- NA
    x
  } # Replace all colons with NA
  round_price    <- function(x) {
    x$purchase_price <- round(as.numeric(x$purchase_price, 2))
    x
  } # Convert to numeric and round 2dp purchase price
  
  # Apply the functions in correct order
    x %>% remove_rows() %>% remove_cols() %>% rename_cols() %>% replace_colon() %>% round_price()
  
}           # Applies numerous tidying functions into one super function
add_free_lease <- function(df, df_name) {
  df$free_lease <- ifelse(grepl("freehold", df_name) == TRUE, "freehold", "leasehold")
  df
} # Add a variable showing "freehold" or "leasehold"
add_new_exist  <- function(df, df_name) {
  df$new_existing <- ifelse(grepl("new", df_name) == TRUE, "new", "existing")
  df
} # Add a variable showing "new" or "existing

housing_list <- list(freehold_new = freehold_new, 
                freehold_exi = freehold_exi, 
                leasehold_new = leasehold_new, 
                leasehold_exi = leasehold_exi) # Merge data sets into list
housing_list <- lapply(housing_list, tidy_df)  # Apply tidy_df function to all data sets
housing_list <- mapply(add_free_lease, housing_list, names(housing_list), SIMPLIFY = FALSE) # Add "freehold" or "leashold" variable
housing_list <- mapply(add_new_exist,  housing_list, names(housing_list), SIMPLIFY = FALSE) # Add "new" or "existing" variable
glimpse(housing_list)                   # Take a look at how we're doing so far
housing_data <- merge_all(housing_list) # Looks good! Now let's merge it all into one df
housing_data <- housing_data[order(housing_data$la_name), ] # Order the df by la_name
rownames(housing_data) <- 1:nrow(housing_data)              # Renumber the row names
rm(freehold_exi,freehold_new,leasehold_exi,leasehold_new,
   housing_list,tidy_df,add_free_lease,add_new_exist)       # We can get rid of these now




# Add new object to ggplot that creates a split violin plot/density plot
GeomSplitViolin <- ggproto(
  
  `_class` = "GeomSplitViolin", `_inherit` = GeomViolin, draw_group = function(
    self, data, ..., draw_quantiles = NULL
  ) {
    data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
    grp  <- data[1,'group']
    
    newdata <- plyr::arrange(transform(data, x = if(grp%%2 == 1) xminv else xmaxv), 
                             if(grp%%2 == 1) y else -y)
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1, nrow(newdata)-1, nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
    if (
      length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))
    ) {
      stopifnot(
        all(draw_quantiles >= 0), all(draw_quantiles <= 1)
      )
      quantiles  <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", 
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    }
    else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
  
)


geom_split_violin <- function (
  mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
  draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
  show.legend = NA, inherit.aes = TRUE
) {
  
  layer(data = data, mapping = mapping, 
        stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...)
  )
}

# Create plot
g <- ggplot(housing_data, aes(new_existing, purchase_price, fill = free_lease))
g <- g + geom_split_violin()
g <- g + labs(y = "Purchase Price", x = NULL, fill = NULL)
g <- g + scale_y_continuous(limits = c(0, 1000000), labels = dollar_format(prefix = "£"))
g <- g + scale_x_discrete(labels = c("Existing", "New"))
g <- g + scale_fill_tableau(labels = c("Freehold", "Leasehold"))
g <- g + theme_light()
g <- g + theme(panel.grid.major.x = element_blank())
g <- g + theme(axis.title.y = element_text(size = 15))
g <- g + theme(axis.text = element_text(size = 13))
g <- g + theme(legend.position = "top")
g <- g + theme(legend.text = element_text(size = 12))






png("Freehold Leasehold Split Distribution.png",
    units = "in", width = 13, height = 8, res = 700)
g
dev.off()


