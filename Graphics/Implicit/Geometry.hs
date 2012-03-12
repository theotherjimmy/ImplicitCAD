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
import Data.Maybe (isNothing, isJust)

type LineSeg = (ℝ2,ℝ2)

-- | sequential pairs of elements of a list
pairs points = [(points !!! n, points !!! (n + (1::ℕ)) ) | n <- [0 .. (length points - (1::ℕ))] ]

-- | A accessor that doesn't go out of bounds
(!!!) :: Show a => [a] -> Int -> a
l !!! n = l !! (mod n $ length l)

-- | The intersection of two line segments
--   We consider them to be open line segments,
--   so if an end catches we don't count it
intersect :: LineSeg -> LineSeg -> Maybe ℝ2
intersect (a1, a2) (b1, b2) = 
	let
		-- transform coordinates so a is flat
		-- first (- a1)
		transform_part1 :: ℝ2 -> ℝ2
		transform_part1 p = (p-a1)
		transform_part1_inv :: ℝ2 -> ℝ2
		transform_part1_inv = (+a1)
		-- this makes the result of a1 be zero
		-- (we'll do these both together, but we need the transform of a2 now)
		(moda2x, moda2y) = a2 - a1
		moda2slope = moda2y/moda2x 
		-- Now (x,y) -> (x,y-moda2slope*x)
		transform_part2 :: ℝ2 -> ℝ2
		transform_part2 (x,y) = (x,y-moda2slope*x)
		transform_part2_inv :: ℝ2 -> ℝ2
		transform_part2_inv (x,y) = (x,y+moda2slope*x)
		-- this makes the final result of a2 have a 0 y component.
		-- a1' = (0,0)
		-- a2' = (modax, 0)
		-- For the others, we actually have to calculate...
		transform = transform_part2 . transform_part1
		-- Let's havave an inverse, too
		untransform :: ℝ2 -> ℝ2
		untransform = transform_part1_inv . transform_part2_inv
		(b1'x, b1'y) = transform b1
		(b2'x, b2'y) = transform b2
		b'slope = (b2'y - b1'y)/(b2'x - b1'x)
		-- c will be the intersect
		c'x = b1'x - b1'y/b'slope
		-- a convenience func for the next part
		-- (to account for arithmetic error, err)
		err = 0.00001 :: ℝ
		between :: ℝ -> ℝ -> ℝ -> Bool
		between a b p = (a' < p && p < b') || (b' < p && p < a') where (a',b') = (a+err*b, b+err*a)
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


tesselateLoopInterior points = 
	let
		-- Test the handedness of a triangle.
		-- Given an oriented loop, a triangle built from consecutive points on it
		-- can be determined to be inside or out based on its hadedness.
		righthanded (a,b,c) = (\(_,_,z) -> z < 0) $ (b - a) ⨯ (c - a) 
		-- the actual magic
		-- see if we can simplify the path by adding a triangle and removing a point
		try :: ℕ -> [ℝ2] -> [(ℝ2,ℝ2,ℝ2)]
		try n prespoints@(a:b:c:others) = traceShow prespoints $ traceShow ((a,b,c), righthanded (a,b,c),filter isJust $ map (intersect (a,c)) $ pairs prespoints ) $
			if n > (20::ℕ) + (5::ℕ)*length points + length points^2
			then [] -- in cases of invalid input, fail gracefully
			else if a == b
			then try (n+ (1::ℕ)) (a:c:others)
			else if righthanded (a,b,c) && (all isNothing $ map (intersect (a,c)) $ pairs prespoints)
			then (b,a,c):(try (n+ (1::ℕ)) (a:c:others) )
			else try (n+(1::ℕ)) (b:c:others ++ [a])
		try _ _ = []
	in
		try 0 $ orient points 


