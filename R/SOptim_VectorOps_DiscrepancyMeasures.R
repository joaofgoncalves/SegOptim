

#' Calculate ED2 segmentation discrepancy measure
#' 
#' This function implements the ED2 discrepancy measure used to assess a given 
#' image segmentation solution. It is based on Liu et al., 2012 with some corrections 
#' introduced later by Novelli et al., 2017.
#' 
#' @param refData Reference polygons data. Either a string containing a path to a vector 
#' data set or a simple features geometry object (\code{sf}). See details below.
#' 
#' @param segmData Segmentation polygons data. Either a string containing a path to a vector 
#' data set or a simple features geometry object (\code{sf}).
#' 
#' @param thresh A threshold percentage used to select segments for each reference polygon. 
#' Calculated as: \eqn{\%SegmIntersected = (AreaIntersected / AreaSegment) \times 100}. If 
#' \%SegmIntersected > thresh then that segment will be selected (default: 50).
#' 
#' @param verbose Print progress messages? (default: FALSE)
#' 
#' @param ... Further arguments passed to \code{\link[sf]{read_sf}} function.
#' 
#' @return A numeric vector with several outputs:        
#' \itemize{      
#'     \item \strong{ED2} - The Euclidean Distance measure 2 (Liu et al., 2012);
#'     \item \strong{PSE} - Potential Segmentation Error;
#'     \item \strong{NSR} - Number-of-Segments Ratio;
#'     \item \strong{NrRefPolygons} - Total number of reference polygons; 
#'     \item \strong{NrOfRefPolsWoCandSegms} - Number of reference polygons without 
#'     candidate segments;
#'     \item \strong{NrOfCandSegments} - Number of candidate segments (for which 
#'     the % of intersected area is above thresh);
#'     \item \strong{SumUndersegAreaAbsDiff} - Sum of absolute undersegmented 
#'     area differences;
#'     \item \strong{TotalAreaRefPols} - Total area for reference polygons (with 
#'     one or more candidate segments).
#' }
#' 
#' @details 
#' 
#' A \strong{"reference polygon"} can be considered a bounded geographic region (Castilla, 2003; 
#' Lang et al., 2010). A basic region is an indivisible unit with respect to certain perspectives 
#' of human cognition (Castilla and Hay, 2008) and can often be identified easily from its surroundings. 
#' In this case a reference polygon is considered a geographic object of interest. These reference polygons 
#' are typically sampled from EO imagery and delineated through manual interpretation or from field surveys. 
#' The size of a reference polygon is typically greater than that of a pixel in an image and smaller than 
#' the whole extent of that image.
#' 
#' @references 
#' 
#' Castilla, G., 2003. Object-oriented analysis of remote sensing images for land cover mapping: 
#' conceptual foundations and a segmentation method to derive a baseline partition for 
#' classification. PhD Thesis, Polytechnic University of Madrid.              
#' 
#' Castilla, G., Hay, G.J., 2008. Image objects and geographic objects. In: Blaschke, T., Lang, S. (Eds.), 
#' Object-based Image Analysis: Spatial Concepts for Knowledgedriven Remote Sensing Applications. 
#' Springer-Verlag, Berling, Heidelberg, pp. 91-110.  
#' 
#' Lang, S., Albrecht, F., Kienberger, S., Tiede, D., 2010. Object validity for operational tasks in 
#' a policy context. Journal of Spatial Science 55 (1), 9-22. 
#' 
#' Liu, Y., L. Bian, Y. Meng, H. Wang, S. Zhang, Y. Yang, X. Shao, and B. Wang. 2012. 
#' Discrepancy measures for selecting optimal combination of parameter values in 
#' object-based image analysis. ISPRS Journal of Photogrammetry and Remote Sensing 
#' 68:144-156.
#' 
#' Novelli, A., M. Aguilar, F. Aguilar, A. Nemmaoui, and E. Tarantino. 2017. AssesSeg - 
#' A Command Line Tool to Quantify Image Segmentation Quality: A Test Carried Out in 
#' Southern Spain from Satellite Imagery. Remote Sensing 9:40.             
#' 
#' 
#' @note 
#' 
#' Compared to AssesSeg tool (from Novelli et al., 2017), this function has some 
#' slight numerical differences in cases where the number of reference polygons without 
#' candidate segments is higher than 0. Probably these derive from area calculations or 
#' specific aspects of each implementation.
#' 
#' @importFrom sf st_area
#' @importFrom sf st_geometry
#' @importFrom sf st_intersection
#' @importFrom sf st_intersects
#' @importFrom sf read_sf

#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' 
#' @importFrom magrittr %>%
#' 


calcDiscrepancyMeasure_ED2 <- function(refData, segmData, thresh = 50, verbose = FALSE, ...){
  
  
  ## ----------------------------------------- ##
  ## Load reference and segmentation data ----
  ## ----------------------------------------- ##
  
  if(is.character(refData) || is.character(segmData) ){
    
    if(verbose) cat("-> Reading input data from file.... ")
    
    if(is.character(refData)){
      if(!file.exists(refData)){
        stop("Can't find input refData file! Please check this argument.")
      }else{
        ref <- sf::read_sf(refData, ...)
      }
    }
    
    if(is.character(segmData)){
      if(!file.exists(segmData)){
        stop("Can't find input segmData file! Please check this argument.")
      }else{
        segm <- sf::read_sf(segmData, ...)
      }
    }
    
    if(verbose) cat("done.\n\n")
  }


  if(verbose) cat("-> Appending identifier and area columns to input data.... ")
  
  ## ---------------------------------------------------------------- ##
  ## Add a segmented ID column (SID) and polygon area (seg_area) ----
  ## ---------------------------------------------------------------- ##
  
  segm <- cbind(segm, SID = 1:length(sf::st_geometry(segm)), seg_area = as.numeric(sf::st_area(segm)))
  
  
  ## ------------------------------------------------------------------------ ##
  ## Add a reference polygon ID column (RID) and polygon area (ref_area) ----
  ## ------------------------------------------------------------------------ ##
  
  ref <- cbind(ref, RID = 1:length(sf::st_geometry(ref)), ref_area = as.numeric(sf::st_area(ref)))
  
  
  if(verbose) cat("done.\n\n")
  
  
  ## -------------------------------------------------------------------------------- ##
  ## Make some calculations for ED2 as Liu et al., 2012 and Novelli et al., 2017 ----
  ## -------------------------------------------------------------------------------- ##
  
  if(verbose) cat("-> Selecting and filtering candidate segments.... ")

  # Get intersected segments id (row #)
  
  int_res <- ref %>% 
    sf::st_intersects(segm) %>%
    unlist %>%
    unique %>%
    sort
  
  
  # Create a temp dataset containing only the candidate segments, i.e., segments that actually are intersected 
  # by the reference polygons
  # According to our tests this was found to improve tremedeously the processing speed of 
  # the sf::st_intersection step
  cand_segms <- segm %>% filter(rownames(segm) %in% int_res)
  
  if(verbose) cat("done.\n\n")
  
  
  if(verbose) cat("-> Intersecting candidate segments and reference polygons.... ")
  
  # Calculate the intersection between the candidate segments and the reference polygons
  int_segms <- sf::st_intersection(cand_segms, ref)
  int_segms <- cbind(int_segms, int_segm_area = as.numeric(sf::st_area(int_segms)))
  
  int_seg_df <- as.data.frame(int_segms)[,-ncol(int_segms)] # Remove the geometry column no longer required for calculations
  rm(int_segms) # Remove int_segms object since it is no longer needed
  
  if(verbose) cat("done.\n\n")
  
  ## --------------------------- ##
  
  if(verbose) cat("-> Aggregating data and making final calculations.... ")
  
  int_seg_df <- int_seg_df %>%
    dplyr::group_by(RID, SID) %>%
    dplyr::summarise(ref_area = unique(ref_area), seg_area = unique(seg_area), int_area_sum = sum(int_segm_area)) %>% 
    dplyr::mutate(ref_int_perc = round((int_area_sum / ref_area) * 100, 2)) %>% 
    dplyr::mutate(seg_int_perc = round((int_area_sum / seg_area) * 100, 2)) %>% 
    dplyr::filter(seg_int_perc > thresh | ref_int_perc > thresh) %>% 
    as.data.frame
  
  # Nr of reference polygons
  m <- nrow(ref) 
  # Effective nr of reference polygons
  m_ <- int_seg_df %>% .$RID %>% unique %>% length 
  # Nr of candidate segments (above or equal to thresh)
  v_ <- int_seg_df %>% .$SID %>% unique %>% length 
  # This is the PSE term for undersegmented area differences sum[  abs( s_i - r_k ) ]
  undersegm_area_sum <- sum(abs(int_seg_df$seg_area - int_seg_df$int_area_sum))
  # Difference between the number of segments in ref and  
  n_ <- (m - m_) 
  
  
  if(n_ == 0){
    
    # Total area of reference polygons
    ref_area <- sf::st_area(ref) %>% sum %>% as.numeric
    
    PSE_correctionTerm <- 0
    NSR_correctionTerm <- 0
    
  }else{
    
    # Total area of the (m - n) reference polygons
    ref_area <- ref %>% filter(RID %in% unique(int_seg_df$RID)) %>% .$ref_area %>% sum
    
    # Maximum number of corresponding segments found for one single reference polygon
    # Used for the NSR correction term
    vmax <- int_seg_df %>% 
      dplyr::group_by(RID) %>% 
      dplyr::summarise(n_segs = length(unique(SID))) %>% 
      dplyr::select(n_segs) %>% 
      max
    
    # Correction terms where introduced by Novelli et al., 2017
    # These authors also changed the NSR denominator from m to (m - n)
    PSE_correctionTerm <- n_ * max(int_seg_df$int_area_sum)
    NSR_correctionTerm <- (n_ * vmax)
  } 
  
  
  # Calculate the indices
  PSE_new <- (undersegm_area_sum + PSE_correctionTerm) / ref_area
  NSR_new <- abs(m - v_ - NSR_correctionTerm) / (m - n_)
  
  ED2_new <- sqrt(PSE_new^2 + NSR_new^2)
  
  out <- c(ED2_new, PSE_new, NSR_new, m, n_, v_, undersegm_area_sum, ref_area)
  names(out) <- c("ED2","PSE","NSR","NrRefPolygons","NrOfRefPolsWoCandSegms",
                  "NrOfCandSegments","SumUndersegAreaAbsDiff","TotalAreaRefPols")
  
  if(verbose) cat("done.\n\n")
  
  if(verbose) cat("[ ED2_index =",round(ED2_new,3),"] \n\n")
  return(out)
  
}





