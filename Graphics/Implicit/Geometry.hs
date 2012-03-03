-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, UndecidableInstances  #-}

-- We're going to be working with line segments, etc, a lot.
-- Better make them nice to work with!

module Graphics.Implicit.Geometry  where

import qualified Prelude as P
import Prelude hiding ((+),(-),(*),(/))

import Graphics.Implicit.Definitions
import Graphics.Implicit.SaneOperators
import Data.List (sortBy, minimumBy)
import Debug.Trace

type LineSeg = (ℝ2,ℝ2)
-- Make open...
-- The intersection of two line segments
intersect :: LineSeg -> LineSeg -> Maybe ℝ2
intersect (a1, a2) (b1, b2) = (\s -> traceShow ((a1, a2), (b1, b2), s) s) $
	let
		-- transform coordinates so a is flat
		-- first (- a1)
		-- this makes the result of a1 be zero
		-- (we'll do these both together, but we need the transform of a2 now)
		(moda2x, moda2y) = a2 - a1
		moda2slope = moda2y/moda2x
		-- Now (x,y) -> (x,y-moda2slope*x)
		-- this makes the final result of a2 have a 0 y component.
		-- a1' = (0,0)
		-- a2' = (modax, 0)
		-- For the others, we actually have to calculate...
		transform = (\(x,y) -> (x,y-moda2slope*x)) . (\p -> p-a1)
		-- Let's havave an inverse, too
		untransform = (\p -> p-a1) . (\(x,y) -> (x,y+moda2slope*x))
		(b1'x, b1'y) = transform b1
		(b2'x, b2'y) = transform b2
		b'slope = (b2'y - b1'y)/(b2'x - b1'x)
		-- c will be the intersect
		c'x = b1'x + ((0::ℝ)-b1'y)/b'slope
		-- a convenience func for the next part
		between a b p = (a < p && p < b) || (b < p && p < a)
	in
		if between 0 moda2x c'x && between b1'x b2'x c'x
		then Just $ untransform (c'x, b1'y + (c'x-b1'x)*b'slope)
		else Nothing

-- | The distance a point p is from a line segment (a,b)
distFromLineSeg :: ℝ2 -> (ℝ2, ℝ2) -> ℝ
distFromLineSeg p@(p1,p2) (a@(a1,a2), b@(b1,b2)) = norm (closest - p)
	where
		ab = b - a
		nab = ( (1::ℝ) / norm ab) * ab
		ap = p - a
		d  = nab ⋅ ap
		closest
			| d < 0 = a
			| d > norm ab = b
			| otherwise = a + d*nab


orient points = 
	let
		-- A accessor that doesn't go out of bounds
		(!!!) :: Show a => [a] -> Int -> a
		l !!! n = l !! (mod n $ length l)
		-- Left most point. We will see how the path behaves there to determine
		-- its handedness
		leftMost :: Int
		leftMost = minimumBy 
			(\a b -> compare (fst $ points !!! a) (fst $ points !!! b) ) 
			[0.. length points P.-1]
		-- drop the nth element of an array
		dropNth :: Int -> [a] -> [a]
		dropNth n l = take (mod n $ length l) l ++ drop ((mod n $ length l) P.+ 1) l
		-- Does the path locally look like it is clockwise or counterclockwise?
		-- We will make this global by looking at leftMost
		locallyClockwiseAt :: Int -> [ℝ2] -> Bool
		locallyClockwiseAt x points =
			let
				a = (points !!! (x P.+1) - points !!! x)
				b = (points !!! (x P.-1) - points !!! x)
			in
				case compare ((\(_,_,z) -> z) $ a ⨯ b) 0 of
					GT -> False
					LT -> True
					EQ -> case (norm a, norm b, compare (norm a) (norm b) ) of
						(0,_,_ ) -> 
							locallyClockwiseAt x (dropNth x points)
						(_,0,_ ) -> 
							locallyClockwiseAt x (dropNth x points)
						(_,_,GT) ->
							locallyClockwiseAt (x P.- 1) points
						(_,_,LT) -> 
							locallyClockwiseAt (x P.+ 1) points
						(_,_,EQ) -> 
							locallyClockwiseAt x (dropNth x points)
	in
		if locallyClockwiseAt leftMost points
		then points
		else reverse points
