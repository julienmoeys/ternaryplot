
#   aa00-ternaryplot-package.R
#   Package description (in the package documentation)
#
#   aa01-ternaryplot-options.R
#       
#       .tpParList, tpParList (environments)
#       tpPar
#       getTpPar
#   
#   aa02-ternaryplot-classes.R
#       
#       ternaryCheck
#       ternaryCheck.ternaryGeometry
#       ternaryCheck.ternaryVariables
#       ternaryCheck.ternarySystem
#       
#       .generateTernaryGeometry2ndClass
#       createTernaryGeometry
#       createTernaryVariables
#       createTernarySystem
#       
#   aa03-ternaryplot-classes-utility.R
#       
#       blrNames
#       blrNames.ternarySystem
#       blrNames.ternaryVariables
#       `blrNames<-`
#       `blrNames<-.ternarySystem`
#       `blrNames<-.ternaryVariables`
#       blrLabels
#       blrLabels.ternarySystem
#       blrLabels.ternaryVariables
#       `blrLabels<-`
#       `blrLabels<-.ternarySystem`
#       `blrLabels<-.ternaryVariables`
#       blrClock
#       blrClock.ternarySystem
#       blrClock.ternaryGeometry
#       `blrClock<-`
#       `blrClock<-.ternarySystem`
#       `blrClock<-.ternaryGeometry`
#       fracSum
#       fracSum.ternarySystem
#       fracSum.ternaryGeometry
#       `fracSum<-`
#       `fracSum<-.ternarySystem`
#       `fracSum<-.ternaryGeometry`
#       tlrAngles
#       tlrAngles.ternarySystem
#       tlrAngles.ternaryGeometry
#       `tlrAngles<-`
#       `tlrAngles<-.ternarySystem`
#       `tlrAngles<-.ternaryGeometry`
#       ternaryGeometry
#       ternaryGeometry.ternarySystem
#       `ternaryGeometry<-`
#       `ternaryGeometry<-.ternarySystem`
#       print.ternaryGeometry
#       ternaryVariables
#       ternaryVariables.ternarySystem
#       `ternaryVariables<-`
#       `ternaryVariables<-.ternarySystem`
#       print.ternaryVariables
#       print.ternarySystem
#       
#   aa04-ternarysystems.R 
#       
#       ternarySystemEnv (environment)
#       getTernarySystem
#       listTernarySystem
#        
#   aa05-ternarydata.R 
#       
#       .setTernarySystem
#       ternaryData
#       ternaryData.character
#       ternaryData.ternarySystem
#       
#   aa06-ternary2xy.R
#       
#       deg2rad
#       ternary2xy (*)
#       ternary2xy.character
#       ternary2xy.ternaryData
#       ternary2xy.ternarySystem
#       .ternary2xy
#       .ternary2xy.geo_TTT
#       .ternary2xy.geo_FFF
#       .ternary2xy.geo_FTX
#       .ternary2xy.geo_TXF
#       
#   aa07-plotUtilities.R
#   
#       .nbMargin2diffXY
#       
#   otherTernaryPlots.R
#       Contains some tests of other ternary plot
#       systems.
#   
#   ternarygrid.R
#   
#       createTernaryGrid
#       createTernaryGrid.character
#       createTernaryGrid.ternarySystem
#       
#       ternary2SpatialPolygonsDataFrame
#       ternary2SpatialPolygonsDataFrame.ternaryPolygons
#       
#   ternaryplot.R
#   
#       ternaryWindow
#       ternaryWindow.character
#       ternaryWindow.ternarySystem
#       ternaryBox
#       .xy2SpatialPolygons
#       ternaryBox.ternarySystem
#       ternaryPoints
#       ternaryPoints.ternarySystem
#       ternaryText
#       ternaryText.ternarySystem
#       ternarySegments
#       .xySegments2SpatialLines
#       ternarySegments.ternarySystem
#       ternaryArrows
#       ternaryArrows.ternarySystem
#       .ternaryGridBase 
#       .ternaryGridBase.ternarySystem 
#       ._ternaryGridBase 
#       ._ternaryGridBase.geo_FFF
#       ._ternaryGridBase.geo_FFF
#       ._ternaryGridBase.geo_FTX
#       ._ternaryGridBase.geo_TXF
#       .ternaryTicks
#       .ternaryTicks.ternarySystem 
#       ._ternaryTicks
#       ._ternaryTicks.geo_FFF
#       ._ternaryTicks.geo_FFF
#       ._ternaryTicks.geo_FTX
#       ._ternaryTicks.geo_TXF
#       ternaryGrid
#       ternaryGrid.ternarySystem
#       ternaryPlot
#       ternaryPlot.character
#       ternaryPlot.ternarySystem
#       .ternaryLims
#       .ternaryLims.character
#       .ternaryLims.ternarySystem
#       .ternaryClockSwitch
#       .ternaryAxisArrowsBase
#       .ternaryAxisArrowsBase.ternarySystem (*)
#       ._ternaryAxisArrowsBase
#       ._ternaryAxisArrowsBase.geo_FFF
#       ._ternaryAxisArrowsBase.geo_FFF
#       ._ternaryAxisArrowsBase.geo_FTX
#       ._ternaryAxisArrowsBase.geo_TXF
#       .ternaryAxisArrows
#       .ternaryAxisArrows.ternarySystem 
#       ._ternaryAxisArrows
#       ._ternaryAxisArrows.geo_FFF
#       ._ternaryAxisArrows.geo_FFF
#       ._ternaryAxisArrows.geo_FTX
#       ._ternaryAxisArrows.geo_TXF
#       ternaryAxis
#       ternaryAxis.character
#       ternaryAxis.ternarySystem
#       ternaryAxis.ternarySystem
#       
#       
#       
