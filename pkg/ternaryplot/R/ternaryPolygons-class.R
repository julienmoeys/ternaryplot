
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License v 3   |
# +--------------------------------------------------------+




# ternaryPolygons-class ====================================

#'Class to hold ternary polygons (ternary grids, ternary classification, ...)
#'
#'Class (\code{S3}) to hold ternary polygons (ternary grids, 
#'  ternary classification, ...). A 
#'  \code{ternaryPolygons}-class object is 
#'  a tagged \code{\link[base]{data.frame}} to which is 
#'  associated several \code{\link[base]{attributes}}. 
#'
#'  Each row in the \code{data.frame} contains the ternary 
#'  coordinates of one vertex of one polygon. Polygons should 
#'  of course have at least 3 vertices and there can be 
#'  as many polygons as needed.
#'
#'
#'@details 
#'  The \code{S3} \code{\link[base]{class}} system is an 
#'  \emph{in}formal and lightweight class system.
#'
#'
#'@section data.frame attributes:
#'
#'  Note: see \code{\link[base]{attr}} to retrieve or change 
#'  attributes.
#'  
#'  The \code{\link[base]{data.frame}} has the following 
#'  attributes:
#'
#'\itemize{
#'  \item \bold{\code{ternarySystem}}: A 
#'    \code{\link[ternaryplot]{ternarySystem-class}} object. 
#'    The ternary system associated with the ternary polygons.
#'  \item \bold{\code{idCol}}: A single character string. 
#'    name of the column in the \code{\link[base]{data.frame}} 
#'    that contains the identifiers of the different polygons.
#'  \item \bold{\code{labels}}: A vector of character strings, 
#'    the labels to be displayed inside the polygons when 
#'    plotting them on a ternary plot. There should be 
#'    one label per polygon in the \code{\link[base]{data.frame}}, 
#'    and the labels should be given in the same order as 
#'    the polygons identifiers in the 
#'    \code{\link[base]{data.frame}}.
#'}  
#'
#'
#'@section data.frame columns:
#'  
#'  The \code{\link[base]{data.frame}} has the following 
#'  column names:
#'  
#'\itemize{
#'  \item 3 columns named after the argument \code{blrNames} 
#'    in the \code{ternarySystem} attribute. Use 
#'    \code{\link[ternaryplot]{blrNames}( attr( x, "ternarySystem" ) )} 
#'    (where \code{x} is a \code{ternaryPolygons-class} object) 
#'    to fetch the current variable names.
#'  \item 1 column named after the attribute \code{idCol}. 
#'  \item \code{\link[ternaryplot]{ternary2SpatialPolygons}} 
#'    (to convert the grid into a 
#'    \code{\link[sp]{SpatialPolygonsDataFrame}})
#'}  
#'
#'@seealso
#'\itemize{
#'  \item \code{\link[ternaryplot]{createTernaryGrid}} 
#'    to create a regular ternary grid (of triangular 
#'    polygons) and \code{\link[ternaryplot]{ternaryClasses}} 
#'    to extract the ternary classification of a ternary system 
#'    as \code{ternaryPolygons-class}. 
#'  \item \code{\link[ternaryplot]{ternaryPolygons}} (a 
#'    function, not to be confused with 
#'    \code{ternaryPolygons-class}, the class definition) to 
#'    plot ternary polygons on a ternary triangle.
#'} 
#'
#'
#'@name ternaryPolygons-class
#'
#'@rdname ternaryPolygons-class
#'
NULL 
