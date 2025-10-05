make_filename <- function(
  effects,
  ext = ".csv",
  sep = "_"
) {
  # group effects
  effects <- list(
    fixed = c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl),
    varying = effects$m_var,
    interaction = c(effects$i_varsl, effects$i_varit, effects$i_varits,
                    effects$i_varsl_str, effects$i_varit_str, effects$i_varits_str)
  )


  # map each block to its initial
  initials <- c(fixed="F", varying="V", interaction="I")
  
  # helper: strip everything from "(" onward
  dist_name <- function(x) sub("\\(.*", "", x)
  
  # build one segment per block, e.g. "F-bin1-normal-bin2-normal"
  segments <- vapply(names(initials), function(block) {
    vars <- effects[[block]]
    if (is.null(vars) || length(vars) == 0) return("")
    
    pieces <- paste0(
      gsub("\\W+", "", names(vars)),    # sanitize var names
      "-", dist_name(unlist(vars))       # append dist name
    )
    
    paste0(initials[block], "-", paste(pieces, collapse = "-"))
  }, character(1), USE.NAMES = FALSE)
  
  # drop empty segments, glue prefix + segments + extension
  fname_body <- paste(c(segments[segments != ""]), collapse = sep)
  paste0(fname_body, ext)
}
