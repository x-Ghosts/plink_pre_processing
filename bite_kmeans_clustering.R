install.packages(c('OptM', 'RCircos', 'SNPRelate', 'data.table', 'dplyr',
                   'gdsfmt', 'ggplot2', 'ggrepel', 'gridExtra', 'plotly',
                   'poolfstat', 'shiny','stringr'))

# Install the Package manually from this link : https://github.com/marcomilanesi/BITE/blob/master/BITEV2_2.1.2.tar.gz



library(BITEV2)

output_directory <- "/home/adam/Desktop/thesis_datasets_analysis/decisional_subsampling/pre_filter/adaptmap/CH_BOE/subsampled"


# Open the GDS file
gds.in <- bite.open(in.file = "ch_boe.bed", out.dir = output_directory)

gds.path <- gds.in$filename


subset.gds <- bite.representative.sampling(gds.path, out.dir = output_directory, n.subsample = 50, n.iter = 100, red.perc = c(10, 5, 3, 1))


# Kmean Subsampling Method

subset.gds <- bite.kmeans.sampling(gds.path, out.dir = output_directory, n.iter = 100, n.subsample = 50)

gds.in
gds_samples <- bite.getdata("bite_km_subsample.gds", "sample.id")
View(gds_samples)
