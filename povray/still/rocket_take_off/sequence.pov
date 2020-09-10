// --------------------------------------------------------------------------------
// Globe sequence
// 
// --------------------------------------------------------------------------------

#declare m=.01;

#include "colors.inc"
#include "textures.inc"
#include "glass_old.inc"
#include "common.inc"
#include "frame.inc"
#include "night_sky.inc"

#declare seconde=0;
#declare earthType=2;
#include "earth-simple.inc"

#declare sphere_radius=Earth_Radius;

// sea water --------------------------
#declare SeaWaterTexture=texture{pigment { rgb <0.2, 0.2, 0.2> } 
               normal {pigment_pattern{ crackle  
                                        colour_map { [0, rgb 0]
                                                     [1, rgb 1] } 
                                          
                       turbulence 1.7 translate<0,0,1>} 0.5}
              finish { ambient 0.15 diffuse 0.65 
                       brilliance 6.0 phong 0.8 phong_size 120
                       reflection 0.6}
	      scale <1.2,0.7,.6>*2*m  rotate<0,10,0>
	      translate< 1.2*m,100*cm,0.6*m>*seconde/20
}// end of texture

// Earth -------------------------------------
sphere {<0,-sphere_radius,0>,sphere_radius
	//pigment {rgb <0,0,1>}
	texture {SeaWaterTexture} interior{I_Glass}
}



#declare sr=300*m;
// https://www.elonx.net/wp-content/uploads/profile_Starlink-v1-11_Infographic_EN.png

#declare P0=<  -1*km, -1*km,0>;
#declare P1=<   0*km,  0*km,0>;
#declare P2=< 220*km, 74*km,0>;
#declare P3=< 350*km,100*km,0>;
#declare P4=<3000*km,220*km,0>;
#declare P5=<3100*km,220*km,0>;

#declare th=P1.x/sphere_radius;
#declare px=sin(th)*(sphere_radius+P1.y);
#declare py=cos(th)*(sphere_radius+P1.y);
#declare P1c=<px,py,0>;

#declare th=P2.x/sphere_radius;
#declare px=sin(th)*(sphere_radius+P2.y);
#declare py=cos(th)*(sphere_radius+P2.y);
#declare P2c=<px,py,0>;

#declare th=P3.x/sphere_radius;
#declare px=sin(th)*(sphere_radius+P3.y);
#declare py=cos(th)*(sphere_radius+P3.y);
#declare P3c=<px,py,0>;

#declare th=P4.x/sphere_radius;
#declare px=sin(th)*(sphere_radius+P4.y);
#declare py=cos(th)*(sphere_radius+P4.y);
#declare P4c=<px,py,0>;

#declare th=P5.x/sphere_radius;
#declare px=sin(th)*(sphere_radius+P5.y);
#declare py=cos(th)*(sphere_radius+P5.y);
#declare P5c=<px,py,0>;

sphere_sweep {
   cubic_spline
   6  // nombre de points de la spline
   P0  ,sr,
   P1c ,sr,
   P2c ,sr,
   P3c ,sr,
   P4c ,sr,
   P5c ,sr
   translate <0,-sphere_radius,0>
   pigment{Red}
 }


 
#declare camloc   =<10*km,50*km,-100*km>;
#declare camlookat=<10*km,50*km,   0*km>;


camera {
  location camloc
  look_at  camlookat
  sky <0,1,0>
  angle 80
  right -x*image_width/image_height
}

object  {frame scale 10*m translate <1*m,0, 2*m>}

