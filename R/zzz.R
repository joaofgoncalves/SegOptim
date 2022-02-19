.onLoad <- function(libname = find.package("SegOptim"), pkgname = "SegOptim"){
  
  # CRAN Note avoidance
  # due to the use of dplyr and magrittr instances
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(c(".", "RID", "SID", "int_area_sum", 
                           "int_segm_area", "n_segs", "ref_int_perc", 
                           "seg_area","seg_int_perc","FEAT_VALUE",
                           "cell_ID", "predClass","prop"))
  invisible()
}
