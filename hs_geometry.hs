import Debug.Trace
import Data.List
import Data.Function

data IPoint = IPoint Int Int deriving (Eq, Ord, Show)
data Point = Point Double Double deriving Show
-- | ax + by + c = 0
-- | b = 0 => vertical line, v = 1 => non vertical line
data Line  = Line Double Double Double deriving Show
data Vec = Vec Double Double deriving (Eq, Ord, Show)
data Polygon = Polygon [Point] deriving (Show)

instance Eq Point where
  Point x1 y1 == Point x2 y2
    | abs (x1 - x2) >= epsilon = False
    | otherwise = abs (y1 - y2) < epsilon
instance Ord Point where
  Point x1 y1 <= Point x2 y2
    | abs (x1 - x2) >= epsilon = x1 <= x2
    | abs (y1 - y2) >= epsilon = y1 <= y2
    | otherwise = True
  
epsilon :: Double
epsilon = 10**(-9)

deq :: Double -> Double -> Bool
a `deq` b = abs (a-b) < epsilon
infix 4 `deq` -- same as (==)

test = IPoint 3 2 <= IPoint 2 3
test1 = Point 3 2.1 <= Point 3 2.00001

radToDeg rad = rad * 180.0 / pi
degToRad theta = theta * pi / 180.0
square (Vec x y) = x*x + y*y

-- | angle aob in radian
angle a o b = acos (dot oa ob / sqrt (square oa * square ob))
  where (oa,ob) = (toVec o a, toVec o b)
dist (Point x1 y1) (Point x2 y2) = sqrt((x1-x2)**2 + (y1-y2)**2)
dist_i p1 p2 = dist (conv p1) (conv p2)
  where conv (IPoint x1 y1) = Point (fromIntegral x1) (fromIntegral y1)
-- | rotate CCW theta (degree) around (0,0)
rotate (Point x y) theta = Point (x*c - y*s) (x*s + y*c)
  where rad = degToRad theta
        (c,s) = (cos rad, sin rad)

pointsToLine (Point x1 y1) (Point x2 y2)
  | x1 `deq` x2 = Line 1 0 (-x1)  -- vertical line
  | otherwise   = Line a b c    
  where a = (y2 - y1) / (x1 - x2)
        b = 1 -- important. fix b to 1 for later use
        c = -(a * x1) - y1

areParallel (Line a1 b1 c1) (Line a2 b2 c2) = a1 `deq` a2 && b1 `deq` b2
areSame l1@(Line a1 b1 c1) l2@(Line a2 b2 c2) = areParallel l1 l2 && c1 `deq` c2
intersect l1@(Line a1 b1 c1) l2@(Line a2 b2 c2)
  | not (areParallel l1 l2) = Nothing
  | b1 `deq` 0 = Just $ Point x y1
  | otherwise  = Just $ Point x y2
  where x = (b2*c1 - b1*c2) / (a2*b1 - a1*b2)
        y1 = -(a1*x + c1)
        y2 = -(a2*x + c2)
  
dot (Vec x1 y1) (Vec x2 y2) = x1*x2 + y1*y2
cross (Vec x1 y1) (Vec x2 y2) = x1*y2 - y1*x2 -- just magnitude. dir: right hand thumb
ccw p q r = cross (toVec p q) (toVec p r) > epsilon
collinear p q r = cross (toVec p q) (toVec p r) `deq` 0

toVec (Point x1 y1) (Point x2 y2) = Vec (x2-x1) (y2-y1)
vecLen (Vec x y) = sqrt (x*x + y*y)
toUnit v@(Vec x y) = Vec (x/l) (y/l)
  where l = vecLen v
scale (Vec x y) s = Vec (s*x) (s*y)
translate (Point x y) (Vec vx vy) = Point (x+vx) (y+vy)

distToLine p@(Point x y) a@(Point x1 y1) b@(Point x2 y2)
  = (dist p c, c)
  where ap = toVec a p
        ab = toVec a b
        v = scale (toUnit ab) (dot ap ab / vecLen ab)
        c = translate a v
distToLineSeg  p@(Point x y) a@(Point x1 y1) b@(Point x2 y2)
  | s < 0.0       = (dist p a, a)  -- closer to a
  | s > vecLen ab = (dist p b, b)  -- closer to b
  | otherwise = distToLine p a b
  where ap = toVec a p
        ab = toVec a b
        s = dot ap ab / vecLen ab

-- | note: first point is duplicated at last position
polyFromList xs = Polygon $ xs ++ [head xs]
perimeter (Polygon xs) = sum $ zipWith dist xs (tail xs)
-- | area work both for convex and concave
area (Polygon xs) = 0.5 * (sum $ zipWith go xs (tail xs))
  where go (Point x1 _) (Point _ y2) = x1 * y2
-- | isConvex checks for direction change
isConvex (Polygon xs)
  | sz <= 3 = False  -- point / line is not convex
  | otherwise = case xs of
      (a:b:c:ys) -> let isLeft = ccw a b c
                    in foldr (go isLeft) True (tails $ tail (xs ++ [b]))
  where sz = length xs
        go v (a:b:c:xs) acc = if ccw a b c == v then acc else False
        go v x acc = True
-- | winding number algorithm, works for convex and concave
inPolygon p (Polygon xs)
  | sz == 0   = False
  | otherwise = v `deq` 2*pi
  where sz = length xs
        v = sum $ zipWith go xs (tail xs)
        go p1 p2 | ccw p p1 p2 = angle p1 p p2
                 | otherwise   = -(angle p1 p p2)
-- | line segment p-q intersect with line A-B
lineIntersectSeg (Point px py) (Point qx qy) (Point ax ay) (Point bx by)
  = Point ((px*v + qx*u) / (u+v)) ((py*v + qy*u) / (u+v))
  where (a,b,c) = (by - ay, ax - bx, bx*ay - ax*by)
        (u,v) = (abs $ a * px + b * py + c, abs $ a * qx + b * qy + c)
-- | cut polygon along the line formed by ab
cutPolygon a b (Polygon xs)
  | null r = Polygon r
  | otherwise = Polygon $ r ++ [head r]
  where ab = toVec a b
        r = foldr go [] $ zip xs (tail xs)
        go (p1,p2) acc = acc''
          where left1 = cross ab (toVec a p1)
                left2 = cross ab (toVec a p2)
                pInter = lineIntersectSeg p1 p2 a b
                acc'' = if left1 > -epsilon then p1:acc' -- p1 is on the left of ab
                        else acc'
                acc'  = if left1*left2 < -epsilon then pInter:acc -- edge p1 p2 crosses  line ab
                        else acc
-- | Convex Hull: QuickHull

quickHull :: [Point] -> Polygon
quickHull xs
  | len <= 3 = Polygon $ xs ++ [head xs]
  | otherwise = Polygon $ [a] ++ findHull s1 a b ++ [b] ++ findHull s2 b a ++ [a]
  where len = length xs
        (a:xs') = sort xs
        (xs'',b) = (init xs', last xs')
        (s1,s2) = partition (ccw a b) xs''
findHull :: [Point] -> Point -> Point -> [Point]
findHull xs p q
  | traceShow (xs,p,q) False = undefined
  | null xs = []
  | otherwise = findHull s1 p c ++ [c] ++ findHull s2 c q
  where c = maximumBy go xs
        go a b = let dista = distToLine a p q
                     distb = distToLine b p q
                 in compare dista distb
        s1 = filter (ccw p c) xs
        s2 = filter (ccw c q) xs
  
-- | Convex Hull: Graham's scan algorithm
-- | angleCmp: pivot is leftmost
angleCmp pivot@(Point px py) a@(Point ax ay) b@ (Point bx by)
  | collinear pivot a b = compare (dist pivot a) (dist pivot b)
  | otherwise           = compare (atan2 d1x (-d1y)) (atan2 d2x (-d2y))
  where (d1x,d1y) = (ax-px, ay-py)
        (d2x,d2y) = (bx-px, by-py)
        alpha = degToRad 90
grahamHull :: [Point] -> Polygon
grahamHull xs
  | len <= 3 = Polygon $ xs ++ [head xs]
  | otherwise = Polygon $ scan [head xs'',pivot] (tail xs'' ++ [pivot])
  where len = length xs
        (pivot:xs') = sort xs
        xs'' = sortBy (angleCmp pivot) xs'
        scan acc [] = reverse acc
        scan (b:a:acc) (c:xs)
          | myCcw a b c = scan (c:b:a:acc) xs
          | otherwise = scan (a:acc) (c:xs)
        myCcw p q r = cross (toVec p q) (toVec p r) > epsilon

-- | Convex Hull: Andrew's monotone chain algorithm
andrewHull :: [Point] -> Polygon
andrewHull xs
  | len <= 3 = Polygon $ xs ++ [head xs]
  | otherwise = Polygon $ lower_hull ++ tail upper_hull
  where len = length xs
        xs' = sort xs
        (l,r) = (head xs', last xs')
        lower_hull = scan [l] $ tail xs'
        upper_hull = scan [r] $ tail (reverse xs')
        scan acc [] = reverse acc
        scan [a] (b:xs) = scan [b,a] xs
        scan (b:a:acc) (c:xs)
          | myCcw a b c = scan (c:b:a:acc) xs
          | otherwise = scan (a:acc) (c:xs)
        myCcw p q r = cross (toVec p q) (toVec p r) > epsilon

testPoints = [p 0 0, p 1 0, p 1 1, p 0 1, p 0.5 0, p 0 0.5, p 0.1 0.1, p 0.9 0.9]
  where p = Point
testQ = [p 0 0, p 2 0, p 0 2, p 2 2, p 1 0]
  where p = Point
testR = [p 2 0, p 1 1, p 1 (-1), p 0 0]
  where p = Point
testS = [p 622 991, p 1054 665, p 661 485]
  where p = Point
testPoly = polyFromList $ [(p 0 0), (p 1 0), (p 1.0 1.0), (p 0 1)]
  where p = Point
tt = ccw (p 0 0) (p 1 0) (p 0.1 0.1)
  where p = Point
                            


