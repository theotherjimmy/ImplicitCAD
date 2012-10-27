{-# LANGUAGE OverloadedStrings #-}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export where

import Graphics.Implicit.Definitions
--import Graphics.Implicit.Operations (slice)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Text.Lazy (Text,pack)
import Data.Text.Lazy.IO (writeFile)
import Prelude hiding (writeFile)
import qualified Data.ByteString.Lazy as LBS

import Graphics.Implicit.Export.TextBuilderUtils
import Graphics.Implicit.ObjectUtil
import Graphics.Implicit.Export.Symbolic.Rebound3
import Graphics.Implicit.Export.Render

-- class DiscreteApproxable
import Graphics.Implicit.Export.Definitions

-- instances of DiscreteApproxable...
import Graphics.Implicit.Export.SymbolicObj2 ()
import Graphics.Implicit.Export.SymbolicObj3 ()
import Graphics.Implicit.Export.RayTrace ()

-- File formats
import qualified Graphics.Implicit.Export.PolylineFormats as PolylineFormats
import qualified Graphics.Implicit.Export.TriangleMeshFormats as TriangleMeshFormats
import qualified Graphics.Implicit.Export.NormedTriangleMeshFormats as NormedTriangleMeshFormats
import qualified Graphics.Implicit.Export.SymbolicFormats as SymbolicFormats

import qualified Codec.Picture as ImageFormatCodecs

-- Write an object in a given formet...

writeObject :: (DiscreteAproxable obj aprox) => 
        ℝ                   -- ^ Resolution
        -> (aprox -> Text)  -- ^ File Format (Function that formats)
        -> FilePath         -- ^ File Name
        -> obj              -- ^ Object to render
        -> IO ()            -- ^ Writing Action!

writeObject res format filename obj = writeFile filename $ formatObject res format obj

writeObject' :: (DiscreteAproxable obj aprox) => 
        ℝ                   -- ^ Resolution
        -> (FilePath -> aprox -> IO ())  -- ^ File Format writer
        -> FilePath         -- ^ File Name
        -> obj              -- ^ Object to render
        -> IO ()            -- ^ Writing Action!

writeObject' res formatWriter filename obj =
	let
		aprox = discreteAprox res obj
	in 
		formatWriter filename aprox

formatObject :: (DiscreteAproxable obj aprox) =>
        ℝ                   -- ^ Resolution
        -> (aprox -> Text)  -- ^ File Format (Function that formats)
        -> obj              -- ^ Object to render
        -> Text             -- ^ Resulting lazy ByteString

formatObject res format = format . discreteAprox res

writeSVG res = writeObject res PolylineFormats.svg

writeSTL res = writeObject res  TriangleMeshFormats.stl

writeBinSTL res file obj = LBS.writeFile file $ TriangleMeshFormats.binaryStl $ discreteAprox res obj

writeOBJ res = writeObject res  NormedTriangleMeshFormats.obj
writeTHREEJS res = writeObject res  TriangleMeshFormats.jsTHREE

writeGCodeHacklabLaser res = writeObject res PolylineFormats.hacklabLaserGCode

writeSCAD3 res filename obj = writeFile filename $ SymbolicFormats.scad3 res obj
writeSCAD2 res filename obj = writeFile filename $ SymbolicFormats.scad2 res obj

writePNG res = writeObject' res ImageFormatCodecs.savePngImage


writeHacklabUltimakerGCode name obj = writeFile name $ hacklabUltimakerGCode obj

{-
renderRaw :: ℝ3 -> ℝ3 -> ℝ -> String -> Obj3 -> IO()
renderRaw (x1, y1, z1) (x2, y2, z2) res name obj = 
	-- A hacky way to encode to chars, but it will do
	let convert n = if n > 1 then 'a' else if n > 0.5 then 'b' else  if n > 0.1 then 'c' else  if n == 0 then 'd' else if n > -0.5 then 'e' else 'd' in
		do
			putStrLn $ show $ length $ [ obj (x,y,z) | x <- [x1, x1+res.. x2], y <- [y1, y1+res.. y2], z <- [z1, z1+res.. z2] ]
			out <- openFile name WriteMode
			mapM_ ( (hPutChar out) . convert) $ 
				[ obj (x,y,z) | x <- [x1, x1+res.. x2], y <- [y1, y1+res.. y2], z <- [z1, z1+res.. z2] ]
			hClose out

renderRaw2D :: ℝ2 -> ℝ2 -> ℝ -> String -> Obj2 -> IO()
renderRaw2D (x1, y1) (x2, y2) res name obj = 
	-- A hacky way to encode to chars, but it will do
	let convert n = if n > 1 then 'a' else if n > 0.5 then 'b' else  if n > 0.1 then 'c' else  if n == 0 then 'd' else if n > -0.5 then 'e' else 'd' in
		do
			putStrLn $ show $ length $ [x1, x1+res.. x2]
			putStrLn $ show $ length $ [ obj (x,y) | x <- [x1, x1+res.. x2], y <- [y1, y1+res.. y2] ]
			out <- openFile name WriteMode
			mapM_ (mapM_ ( (hPutChar out) . convert)) $ 
				[[ obj (x,y) | x <- [x1, x1+res.. x2] ] | y <- [y1, y1+res.. y2] ]
			hClose out


{-writeGCodeMakerbot :: 
	ℝ3          -- ^ lower corner of bounding box
	-> ℝ3       -- ^ upper corner of bounding box
	-> ℝ        -- ^ resolution of rendering
	-> FilePath -- ^ Filename to write gcode to
	-> Obj3     -- ^ 3D object to make gcode for
	-> IO ()    -- ^ Resulting IO action that will write gcode

 writeGCodeMakerbot (x1,y1,z1) (x2,y2,z2) d name obj = 
	let 
		slices = [slice zheight obj | zheight <- [z1, z1+0.1.. z2] ]
		prep obj (x,y) = (obj (x,y), obj (x+d,y), obj (x+d,y+d), obj (x,y+d), obj (x+d/2,y+d/2) , (x,y), d ) 
		layer obj2 = (filter polylineNotNull) $ (map reducePolyline) $ orderLines $ concat $ map getLineSeg [prep obj2 (x,y) | x <- [x1, x1+d.. x2], y <- [y1, y1 +d.. y2] ]
		levelmultilines = map layer slices
		gcodeHeader = 
			   "(generated by ImplicitCAD, based of skeinforge default makerbot results)\n"
			++ "(**** Initialization ****)\n"
			++ "M104 S220 T0 (Temperature to 220 celsius)\n"
			++ "M109 S110 T0 (set heated-build-platform temperature)\n"
			++ "G21 (Metric FTW)\n"
			++ "G90 (Absolute Positioning)\n"
			++ "G92 X0 Y0 Z0 (You are now at 0,0,0)\n"
			++ "M108 S255 (Extruder speed = max; not turning it on yet!)\n"
			++ "(**** Prep the extruder... ****)\n"
			++ "G0 Z15 (Move up for test extrusion)\n"
			++ "M6 T0 (Wait for tool to heat up)\n"
			++ "G04 P5000 (Wait 5 seconds)\n"
			++ "M101 (Extruder on, forward)\n"
			++ "G04 P5000 (Wait 5 seconds)\n"
			++ "M103 (Extruder off)\n"
			++ "M01 (The heater is warming up and will do a test extrusion.  Click yes after you have cleared the nozzle of the extrusion.)\n"
			++ "G0 Z0(Go back to zero.)\n"
		gcodeFooter = 
			"M104 S0 (extruder heating off!)\n"
			++"G00 X0.0 Y0.0 (move to 0)\n"
			++"M2 (end)"
		gcodeXYZ :: ℝ3 -> [Char]
		gcodeXYZ (x,y,z) = "X"++ show x ++" Y"++ show y ++" Z"++ show z
		interpretPolyline (start:others) = 
			"G00 "++ gcodeXY start ++ "\n"
			++ "M101 (extruder forward!)\n"
			++ concat (map (\p -> "G01 " ++ (gcodeXY p) ++ "\n") others)
			++ "M103 (extruder off)\n\n"
		text = gcodeHeader
			++ (concat $ map interpretPolyline multilines)
			++ gcodeFooter
	in do 
		writeFile name text
-}
-}

hacklabUltimakerGCode :: SymbolicObj3 -> Text
hacklabUltimakerGCode symbObj =
	let
		(obj, ((x1,y1,z1),(x2,y2,z2))) = rebound3 (getImplicit3 symbObj, getBox3 symbObj)

		layerheight = 0.2
		extrudePerMM = 0.01142
		xyFval = " F600.0"
		travelFval = " F10200.0"
		xyres = 1.3
		(xLength, yLength, zLength) = (200, 200, 200)

		layers :: [(ℝ, [Polyline])]
		layers =
			let
				slice z obj = \(x,y) -> obj (x,y,z)
				getPolylines obj2 = getContour (x1, y1) (x2, y2) xyres obj2
				
				baseZ = Maybe.fromMaybe z2 $ List.find 
					(\z -> not . null . getPolylines $ slice z obj)
					[z1, z1 + 0.001 .. z2]
				zShift = 0.3 - baseZ
				(xShift, yShift) = (xLength/2 - (x1+x2)/2, yLength/2 - (y1 + y2)/2)
				
				adjust (x,y) = (x + xShift, y+yShift)
				layer z obj = (z + zShift, map (map adjust) $ getPolylines $ slice z obj)
				layers = [layer z obj | z <- [baseZ, baseZ + layerheight .. z2]]
			in
				filter (not.null.snd) layers

		gcodeHeader = ";(Some text that says we're awesome)\n\
						\M109 S218.000000\n\
						\M92 X79.1359 Y79.1359 Z492.307 E933.3940\n\
						\G21\n\
						\G90\n\
						\M107\n\
						\G28 X0 Y0\n\
						\G28 Z0\n\
						\G92 X0 Y0 Z0 E0\n\
						\G1 Z15.0 F180\n\
						\G92 E0\n\
						\G1 F200 E3\n\
						\G92 E0\n\
						\G1 F10200\n"

		gcodeFooter = "M104 S0\n\
						\M140 S0\n\
						\G91\n\
						\G1 F300 E-1\n\
						\G1 X-20.0 Y-20.0 Z0.5 F10200 E-5\n\
						\G28 X0 Y0\n\
						\M84\n\
						\G90\n"

		layerFooter = "G92 E0\n"

		gcodeXYMove :: ℝ2 -> Builder
		gcodeXYMove (x, y) = mconcat [" X", buildTruncFloat x, " Y", buildTruncFloat y]

		gcodeZMove :: ℝ -> Builder
		gcodeZMove z = mconcat [" Z", buildTruncFloat z]

		gcodeMove :: Builder -> Builder -> Builder
		gcodeMove move f = mconcat ["G1", move, f]

		gcodeExtrudeMove :: Builder -> ℝ -> Builder
		gcodeExtrudeMove move e = mconcat [move, " E", buildTruncFloat e]

		gcode :: Builder -> Builder
		gcode command = mconcat [command, "\n"]

		polylineGcode :: Polyline -> Builder
		polylineGcode points@(start@(x1,y1):others@(next@(x2,y2):_)) =
			let
				--firstPointGcode = "G1 X" <> x1 <> " Y" <> y1 <> travelFval <> "\n"
				firstPointGcode = gcode $ gcodeMove (gcodeXYMove start) travelFval
				segGcode (x1,y1) next@(x2, y2) =
					let
						distance = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
						eVal = distance * extrudePerMM
					in
						gcode $ gcodeExtrudeMove (gcodeMove (gcodeXYMove next) xyFval) eVal
						--"G1 X" <> x2 <> " Y" <> y2 <> xyFval <> " E" <> eVal <> "\n"
			in
				firstPointGcode <> mconcat (zipWith segGcode points (tail points) )

		layerGcode :: (ℝ, [Polyline]) -> Builder
		layerGcode (z, polylines) = 
			let
				firstZMove = gcode $ gcodeMove (gcodeZMove z) travelFval
				--firstZMove = "G1 Z" <> z <> travelFval <> "\n"
			in
				firstZMove <> mconcat (map polylineGcode polylines) <> layerFooter

	in
		toLazyText $ gcodeHeader <> mconcat (map layerGcode layers) <> gcodeFooter
