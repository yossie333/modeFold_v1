# modeFold_v1
The first program for vocal fold oscillation simulations based on the eigenmode analysis.

   Fortran program for vocal fold oscillation modeFold ver1.0
   2023/Nov/7    by  Tsukasa Yoshinaga
   MIT License
   
   This program calculate the vocal fold oscillation from the
   eigenmodes obtained from the COMSOL eigenanalysis.
   The external forces can be chosen as forced oscillation or
   1D Bernoulli's equation.
 
   In this version, the author confirmed that the result is quite
   similar to the experiment by Kanaya et al., (2022) JASA-EL,
   when Ps = 2900 Pa, zeta = 0.06, kc1 = 9.0E-3, kc2 = 6.0E+0.
   The results showed f0 = 122.5 Hz, mean Ug = 97.26 L/min.
   The displacement image was similar to Fig. 3.
 
   Input files: Parameter file (param.txt)
                COMSOL output (VTK file)
                              (frequency text)
                Surface point list (surface.txt)
   Output files: Shape files (result/deform***.vtu)
                flowrate (result/flowrate.txt)
    
   Module file: variMode
