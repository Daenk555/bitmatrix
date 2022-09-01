
#' @export
bitmatrix_from_bitvector <- function(obj) {
  stopifnot("bit" %in% class(obj))
  btmtrx <- structure(list(bitvector = obj), class = "bitmatrix")
  return(btmtrx)
}

#' @export
bitmatrix_from_matrix <- function(obj) {
  stopifnot(is.matrix(obj) | !anyNA(obj))
  value <- obj |> t() |> c() |> as.logical() |> as.bit() |> bitmatrix_from_bitvector()
  attr(value, ".Dimnames") 	<- list(rownames(obj),colnames(obj))
  attr(value, ".Dim")		<- dim(obj)
  return(value)
}

#' @export
dim.bitmatrix <- function(obj){
  stopifnot("bitmatrix" %in% class(obj))
  return(attr(obj, ".Dim"))
}

#' @export
ncol.bitmatrix <- function(obj){
  stopifnot("bitmatrix" %in% class(obj))
  return(attr(obj, ".Dim")[2])
}

#' @export
nrow.bitmatrix <- function(obj){
  stopifnot("bitmatrix" %in% class(obj))
  return(attr(obj, ".Dim")[1])
}

#' @export
names.bitmatrix <- function(obj){
  stopifnot("bitmatrix" %in% class(obj))
  return(rownames.bitmatrix(obj))
}

#' @export
dimnames.bitmatrix <- function(obj){
  stopifnot("bitmatrix" %in% class(obj))
  return(attr(obj, ".Dimnames"))
}

#' @export
rownames.bitmatrix <- function(obj){
  stopifnot("bitmatrix" %in% class(obj))
  return(dimnames.bitmatrix(obj)[1])
}

#' @export
colnames.bitmatrix <- function(obj){
  stopifnot("bitmatrix" %in% class(obj))
  return(dimnames.bitmatrix(obj)[2])
}

#' @export
`[.bitmatrix` <- function(x, i=1:nrow(x),j=1:ncol(x)) {
  
  stopifnot("bitmatrix" %in% class(obj))

  l_i <- length(i)
  l_j <- length(j)
  nc <- ncol(x)

  if( l_i == 1 &  l_j == 1){return(x$bitvector[nc*(i-1) + j])}


  if(l_j > 1 & l_i > 1){
    extract_slice <- matrix(logical(l_i*l_j), l_i,l_j )
    for(k in seq_along(i)){for(t in seq_along(j)){extract_slice[k, t] <- x$bitvector[nc*(i[k]-1) + j[t]]}}
    return(extract_slice)
  }else{
    if(l_i > 1){
      extract_vec <- logical(l_i)
      for(t in 1:length(i)){extract_vec[t] <- x$bitvector[nc*(t-1) + j]}
      return(extract_vec)
    }else{
      extract_vec <- logical(l_j)
      for(t in 1:length(j)){extract_vec[t] <- x$bitvector[nc*(i-1) + t]}
      return(extract_vec)
    }
  }
}

#' @export
get_ints <- function(obj) return(unclass(obj$bitvector))

#' @export
get_slice <- function(obj, NR,NC){
  extract_slice <- matrix(F, nrow = NR, ncol = NC)
  for(k in 1:NR){for(t in 1:NC){extract_slice[k, t] <- obj$bitvector[NC*(k-1) + t]}}
  return(extract_slice)
}

#' @export
print.bitmatrix <- function(obj){
  stopifnot("bitmatrix" %in% class(obj))

  nc <- ncol(obj)
  nr <- nrow(obj)

  to_print <-
    case_when(
      nc  > 10 & nr > 10 ~ list(get_slice(obj,10,10)),
      nc  < 10 & nr < 10 ~ list(get_slice(obj,nr,nc)),
      nc  < 10 & nr > 10 ~ list(get_slice(obj,10,nc)),
      nc  > 10 & nr < 10 ~ list(get_slice(obj,nr,10))
    )
  print(to_print[[1]])
}

#' @export
sum.bitmatrix <- function(obj, na.rm = FALSE){
  stopifnot("bitmatrix" %in% class(obj))
  S <- bit:::sum.bit(obj$bitvector)
  return(S)
}

#' @export
get_sparsity <- function(obj){
  stopifnot("bitmatrix" %in% class(obj))
  S <- bit:::sum.bit(obj$bitvector)
  L <- ncol(obj)*nrow(obj)
  return(S/L)
}

#' @export
is.bitmatrix <- function(x) inherits(x, "bitmatrix")

#' @export
as.bitmatrix <- function(obj){return(bitmatrix_from_matrix(obj))}

#' @export
as.matrix.bitmatrix <- function(obj){get_slice(obj, nrow(obj), ncol(obj))}

#' @export
colSums_bitmatrix <- function(obj){return(obj |> as.matrix.bitmatrix() |> colSums())}

#' @export
rowSums_bitmatrix <- function(obj){return(obj |> as.matrix.bitmatrix() |> rowSums())}
