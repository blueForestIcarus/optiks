import Text.Printf
import Graphics.Gloss

data Surface = Surface Double Double Double deriving (Show)
data Ray = Ray Double Double Double deriving (Show)
data System = System [Surface] [Ray] Double Double Double Double Double

trace :: [Surface] -> Double -> Double -> Ray -> [Ray]
trace [] n t ray = [ray]
trace ((Surface t' n' c):xs) n t ray@(Ray x y u) = 
	let 
		power = c*(n'-n)
		y' = y + u * t
		x' = x + t
		u' = (n*u - y' * power)/n'
	in	ray : (trace xs n' t' (Ray x' y' u'))

create_system :: [Surface] -> System
create_system surfaces =
	let 

p_trace :: [Ray] -> String
p_trace [] = ""
p_trace ((Ray x y u):xs) = (printf "(%.2f, %.2f) ang=%.5f\n" x y u) ++ p_trace(xs)

p_analyse :: Double -> String -> [Ray] -> IO ()
p_analyse img_dist name rays = 
	let 
		(Ray _ y u) = last rays
		pretext = "Ray " ++ name ++ ":\n"
		height = y + u*img_dist
		captext = (printf "Image Distance: %.2f, Image Height:%.2f\n" img_dist height)
	in putStrLn $ pretext ++ p_trace rays ++ captext

system = create_system
	[ Surface 6.5 1.5 0.04
	, Surface 35 1 (-0.025)
	, Surface 5 1.5 0.02
	, Surface 0 1 (-0.02) ]

object_dist = 90
go = trace system 1 object_dist
ray_m1 = go $ Ray 0 0 0.1
ray_m2 = go $ Ray 0 0 0.046376
ray_c1 = go $ Ray 0 6.5 0

--set up display

draw_system :: [Surface] -> Float -> Picture
draw_system [] _ = Blank
draw_system ((Surface t _ c):next) xpos = 
	let
		r = realToFrac (1/c) :: Float
		shape = if r > 0
			then Translate (xpos + r) 0 $ Arc 150 210 r
			else Translate (xpos + r) 0 $ Arc (-30) 30 (-r)
	in Pictures [shape, draw_system next (realToFrac t +xpos)]

draw_ray :: Float -> Double -> [Ray] -> Picture
draw_ray _ _ [] = Blank
draw_ray offsetx extend rays = 
	let 
		(Ray x y u) = last rays
		expoint = Ray (x + extend ) (y + extend*u) u
	in Translate offsetx 0 $ Line [(realToFrac x, realToFrac y) | (Ray x y _) <- (rays++[expoint])]

window :: Display
window = InWindow "Raytrace" (800, 500) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = draw_system system 0

axis :: Picture
axis = Color (greyN 0.5) $ Line [(-500,0),(500,0)]

scale_factor = 4

main = do
	print system 
	let img_dist = 
		let (Ray _ y u) = last ray_m2 in (-y)/u
	p_analyse img_dist "p-marginal" ray_m1
	p_analyse img_dist "marginal" ray_m2
	p_analyse img_dist "chief" ray_c1

	let df = draw_ray (realToFrac (-object_dist)) img_dist
		in display window background (Scale scale_factor scale_factor $ Pictures [drawing, df ray_m1, df ray_m2, df ray_c1, axis])

	

