-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

-- This file symbolicaly renders contours and contour fillings.
-- If it can't, it passes the puck to a marching-squares-like
-- algorithm...

module Graphics.Implicit.Export.SymbolicObj2 where

import Graphics.Implicit.Definitions

import Graphics.Implicit.Export.Definitions
import Graphics.Implicit.Export.MarchingSquares
import Graphics.Implicit.Export.MarchingSquaresFill
import Graphics.Implicit.Operations
import Graphics.Implicit.Primitives

import Graphics.Implicit.Export.Symbolic.CoerceSymbolic2
import Graphics.Implicit.Export.Symbolic.CoerceSymbolic3
import Graphics.Implicit.Export.Symbolic.Rebound2
import Graphics.Implicit.Export.Symbolic.Rebound3

import qualified Graphics.Implicit.SaneOperators as S
import Graphics.Implicit.SaneOperators (norm, normalize, (⨯))
import qualified Graphics.Implicit.Geometry as Geometry
import Data.List (sortBy, minimumBy)
import Data.Maybe (isNothing)

import Debug.Trace

instance DiscreteAproxable SymbolicObj2 [Polyline] where
	discreteAprox res obj = symbolicGetContour res obj

symbolicGetOrientedContour :: ℝ ->  SymbolicObj2 -> [Polyline]
symbolicGetOrientedContour res symbObj = map Geometry.orient $ symbolicGetContour res symbObj

symbolicGetContour :: ℝ ->  SymbolicObj2 -> [Polyline]
symbolicGetContour _ (RectR 0 (x1,y1) (x2,y2)) = [[ (x1,y1), (x2,y1), (x2,y2), (x1,y2), (x1,y1) ]]
symbolicGetContour res (Circle r) = [[ ( r*cos(2*pi*m/n), r*sin(2*pi*m/n) ) | m <- [0.. n] ]] where
	n = max 5 (fromIntegral $ ceiling $ 2*pi*r/res)
symbolicGetContour res (Translate2 v obj) = map (map (S.+ v) ) $ symbolicGetContour res obj
symbolicGetContour res (Scale2 s obj) = map (map (S.* s)) $ symbolicGetContour res obj
symbolicGetContour _ (PolygonR 0 points) = 
		[points ++ [head points]]
symbolicGetContour res obj = case rebound2 (coerceSymbolic2 obj) of
	(obj, (a,b)) -> getContour a b (res,res) obj


symbolicGetContourMesh :: ℝ ->  SymbolicObj2 -> [(ℝ2,ℝ2,ℝ2)]
symbolicGetContourMesh res (Translate2 v obj) = map (\(a,b,c) -> (a S.+ v, b S.+ v, c S.+ v) )  $
	symbolicGetContourMesh res obj
symbolicGetContourMesh res (Scale2 s obj) = map (\(a,b,c) -> (a S.* s, b S.* s, c S.* s) )  $
	symbolicGetContourMesh res obj
symbolicGetContourMesh _ (RectR 0 (x1,y1) (x2,y2)) = 
	[((x1,y1), (x2,y1), (x2,y2)), ((x2,y2), (x1,y2), (x1,y1)) ]
{- symbolicGetContourMesh res (RectR r (x1,y1) (x2,y2)) = 
	(symbolicGetContourMesh _ (RectR 0 (x1-r,y1-r) (x2-r,y2-r)) 
	++ (symbolicGetContourMesh _ (RectR 0 (x1,y1) (x1+r,y1))
	++ (symbolicGetContourMesh _ (RectR 0 (x1,y1) (x2,y1+r))
	++ (symbolicGetContourMesh _ (RectR 0 (x2-r,y1) (x2,y2))
	++ (symbolicGetContourMesh _ (RectR 0 (x1,y2-r) (x2,y2))
	++ [((),(x),(x1,y1)) |  ]
		where n = max 3 (ceil $ r/res*3)
-}
symbolicGetContourMesh res symbObj@(PolygonR 0 points) = 
	let
		traceReveal a = traceShow a a
		pairs = [(points !!! n, points !!! (n+1) ) | n <- [0 .. (length points) - 1] ]
		-- A accessor that doesn't go out of bounds
		(!!!) :: Show a => [a] -> Int -> a
		l !!! n = l !! (mod n $ length l)
		-- Test the handedness of a triangle.
		-- Given an oriented loop, a triangle built from consecutive points on it
		-- can be determined to be inside or out based on its hadedness.
		righthanded (a,b,c) = case ((b S.- a),(c S.- a)) of
			((x1,y1), (x2,y2)) -> x1*y2 - x2*y1 < 0
		-- the actual magic
		-- see if we can simplify the path by adding a triangle and removing a point
		try :: ℕ -> [ℝ2] -> [(ℝ2,ℝ2,ℝ2)]
		try n (a:b:c:others) = traceShow (n, a:b:c:others) $
			if n > 20 + 20*length points +length points^2
			then [] {-case rebound2 (coerceSymbolic2 symbObj) of
				(obj, (a,b)) -> getContourMesh a b (res,res) obj -}
			else if righthanded (a,b,c) && (all isNothing $ map (Geometry.intersect (a,c)) pairs)
			then (b,a,c):(try (n+1) (a:c:others) )
			else try (n+1) (b:c:others ++ [a])
		try _ _ = []
	in
		try 0 $ Geometry.orient points 
symbolicGetContourMesh res (Circle r) = 
	[ ((0,0),
	   (r*cos(2*pi*m/n), r*sin(2*pi*m/n)), 
	   (r*cos(2*pi*(m+1)/n), r*sin(2*pi*(m+1)/n)) 
	  )| m <- [0.. n-1] ] 
	where
		n = max 5 (fromIntegral $ ceiling $ 2*pi*r/res)
symbolicGetContourMesh res obj = case rebound2 (coerceSymbolic2 obj) of
	(obj, (a,b)) -> getContourMesh a b (res,res) obj


