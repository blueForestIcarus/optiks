import Text.Printf
import Graphics.Gloss

--Surface (<n> <n'>) <curvature> <thickness> <xpos> <power>
data Surface = Surface (Double, Double) Double Double Double Double deriving(Show)

--Ray <x> <y> <u>
--RayTrace ((<obj-dist> <img-dist>) (<obj-height> <img-height>) <mag>) <rays>
data Ray = Ray Double Double Double deriving(Show)
data RayTrace = RayTrace ((Double, Double), (Double, Double), Double) [Ray] deriving(Show)

--System <surfaces> (Gaussian (<P> <P'>) (<F> <F'>) (<N> <N'>) <power>) (Start, End, FFD, BFL, n, n')
--Note: principal points represented by absolute location
data System = System [Surface] Gaussian System_Data deriving(Show)
data Gaussian = Gaussian (Double, Double) (Double, Double) (Double, Double) Double deriving(Show)
type System_Data = ((Double, Double), (Double, Double), (Double, Double))

--number type alchemy
alt = realToFrac

index_s :: Surface -> (Double, Double)
index_s (Surface ns _ _ _ _) = ns

reduce :: [Surface] -> Gaussian 
reduce [] = Gaussian (0,0) (0,0) (0,0) 0
reduce all@(x:xs) = 
	let
		(n,n_in) = index_s x
		(_, n') = index_s $ last all
		
		(Surface _ _ _ xpos pow1) = x
		p1  = xpos
		p1' = xpos 

		(Gaussian (p2, p2') _ _ pow2) = reduce xs 
		t = p2 - p1'
		power = pow1 + pow2 - ((t/n_in)*pow1*pow2)
		pn 	= p1 + (pow2/power) * (n/n_in) * t 
		pn'	= p2'- (pow1/power) * (n'/n_in) * t
		fn 	= pn - n/power
		fn' 	= pn' + n'/power
		shift = (n'- n)/power
		nodal = pn + shift
		nodal'= pn' + shift
	in Gaussian (pn, pn') (fn, fn') (nodal,nodal') power

create_surfaces :: Double -> Double -> [(Double, Double, Double)] -> [Surface]
create_surfaces _ _ [] = []
create_surfaces env xpos ((n,c,t):xs) =
	let  
		power = (n-env)*c
		surface = Surface (env, n) c t xpos power
	in surface : (create_surfaces n (xpos+t) xs)

create_system :: [Surface] -> System
create_system surfaces = 
	let
		(Surface (n,_) _ _ s1 _) = head surfaces
		(Surface (_,n') _ _ sf _) = last surfaces
		gauss@(Gaussian _ (f,f') _ _) = reduce surfaces
	in System surfaces gauss ((s1, sf), (f-s1, f'-sf), (n,n'))

trace_ :: [Surface] -> Ray -> [Ray]
trace_ [] ray = [ray]
trace_ ((Surface (n,n') _ _ xpos power):xs) ray@(Ray x y u) = 
	let
		y' = y + u * (xpos-x)
		x' = xpos
		u' = (n*u - y' * power)/n'
	in	ray : (trace_ xs (Ray x' y' u'))

trace_ray :: System -> Ray -> RayTrace
trace_ray (System surfaces (Gaussian (p, p') _ _ power) ((start,end), _, (n,n')) ) ray@(Ray x y _) =
	let
		s = x-p
		s'= n'/(power + n/s)	
		m = s'/s
		rs = trace_ surfaces ray
	in RayTrace ((x-start, p'+s'-end), (y, m*y), m) rs

--text output
p_rays :: [Ray] -> String
p_rays [] = ""
p_rays ((Ray x y u):xs) = (printf "(%.2f, %.2f) ang=%.5f\n" x y u) ++ p_rays(xs)

p_raytrace :: String -> RayTrace -> IO ()
p_raytrace name (RayTrace ((obj_dist, img_dist), (obj_height, img_height), m) rays) = 
	let
		pretext = "Ray " ++ name ++ ":\n"
		captext = (printf "Image Distance: %.2f, Image Height:%.2f\nObject Distance: %.2f, Object Height:%.2f\nMagnification:%.2f\n" img_dist img_height obj_dist obj_height m)
	in putStrLn $ pretext ++ p_rays rays ++ captext

p_system :: System -> IO ()
p_system (System surfaces (Gaussian (pp,pp') (fp,fp') (np,np') power) ((start,end),(ffd,bfl),(n,n'))) = 
	let
		gaussian = (printf "Cardinal Points:\n\tFront Focal Point: %.2f\tRear Focal Point: %.2f\n\t" fp fp') ++
			(printf "Front Principal Plane: %.2f\tRear Principal Plane: %.2f\n" pp pp')
		nodal = if n==n' then "" else (printf "\tFront Nodal Point: %.2f\tRear Nodal Point: %.2f\n" np np)
		other = printf "Data:\n\tBFL = %.2f\n\tFFD = %.2f\n\tpower = %.2f\n\tefl = %.2f\n" bfl ffd power (1/power)
	in putStrLn $ gaussian ++ nodal ++ other

draw_rays :: Double -> [Ray] -> Picture
draw_rays _ [] = Blank
draw_rays extend rays = 
	let 
		(Ray x y u) = last rays
		expoint = Ray (x + extend) (y + extend*u) u
	in Line [(alt x, alt y) | (Ray x y _) <- (rays++[expoint])]

draw_raytrace :: RayTrace -> Picture
draw_raytrace (RayTrace ((_,img_dist),_,_) rays) = draw_rays img_dist rays

draw_surfaces :: [Surface] -> Picture
draw_surfaces [] = Blank
draw_surfaces ((Surface _ c _ xpos _):next) = 
	let
		r = alt (1/c) :: Float
		x = alt xpos + r :: Float
		shape = if r > 0
			then Translate x 0 $ Arc 160 200 r
			else Translate x 0 $ Arc (-20) 20 (-r)
	in Pictures [shape, draw_surfaces next]

draw_system :: System -> Picture
draw_system (System surfaces (Gaussian (pp, pp') (fp,fp') (np,np') _) ((start,end),_,_) ) = 
	let
		p1 = draw_surfaces surfaces
		p2 = Pictures 
			[Color red (Line [(alt pp,-1000),(alt pp,1000)])
			,Color blue (Line [(alt pp',-1000),(alt pp',1000)])]
		p3 = Pictures
			[Translate (alt fp ) 0 $ ThickCircle 0 1.8
			,Translate (alt fp') 0 $ ThickCircle 0 1.8
			,Translate (alt np ) 0 $ ThickCircle 0 1.8
			,Translate (alt np') 0 $ ThickCircle 0 1.8
			,Color (greyN 0.5) $ Line [(-500,0),(500,0)]]
	in Pictures [p1,p2,p3]

--load system, trace marginal/chief rays, output to terminal and draw diagram
default_system_display :: System -> Double -> Double -> IO ()
default_system_display system obj2lens obj_height = do 
	let 
		--todo: negative obj2lens reverse trace
		(System _ _ ((start,_),_,_)) = system
		rt_a = trace_ray system (Ray (start-obj2lens) 0 0.1)
		rt_b = trace_ray system (Ray (start-obj2lens) obj_height 0)

		window :: Display
		window = InWindow "Raytrace" (800, 500) (10, 10)

		background :: Color
		background = white

	--output
	p_system system
	p_raytrace "Marginal" rt_a
	p_raytrace "Chief" rt_b

	display window background (Pictures 
		[draw_system system
		,draw_raytrace rt_a
		,draw_raytrace rt_b])
	
main = do
	let
		surfaces = create_surfaces 1 0
			[(1.5, 0.01, 0)
			,(1, -0.01, 0)]
		system = create_system surfaces
		obj2lens = 200
		obj_height = 10
	default_system_display system obj2lens obj_height
	--print $ show system
		



 		

