program main
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program calculate the vocal fold oscillation from the
! eigenmodes obtained from the COMSOL eigenanalysis.
! The external forces can be chosen as forced oscillation or
! 1D Bernoulli's equation.
!
! In this version, the author confirmed that the result is quite
! similar to the experiment by Kanaya et al., (2022) JASA-EL,
! when Ps = 2900 Pa, zeta = 0.06, kc1 = 9.0, kc2 = 6000.
! The results showed f0 = 121.9 Hz, mean Ug = 81.0785 L/min.
! The displacement image was similar to Fig. 3.
!
! Input files: Parameter file (param.txt)
!              COMSOL output (VTK file)
!                            (frequency text)
!              Surface point list (surface.txt)
! Output files: Shape files (result/deform***.vtu)
!               area and flowrate (result/flowrate.txt)
!   
! Module file: variMode
!*******************************************************************
      use variMode
      implicit none
      integer i,j,k,istep
      integer d(8),ti,tf,tr,tmax
      
      call date_and_time(values=d)
      call system_clock(ti)

      write(*,'("Solver: modeFold start")')
      write(*,'(a,i0,a,i0.2,a,i0.2,a,i0.2,a,i0.2)')&
              'Date: ',d(1),'/',d(2),'/',d(3),', Time:',d(5),':',d(6)

      !initial reading files
      call readParam
      call readFreq
      call readVTK

      !initial preparation
      call surfExtract
      call surfArea
      call initia
     
      !------------------------------------------------------
      !start time loop
      write(*,*)"!! Start time loop !!"
      do istep=2,nstep

          call step(istep)

          if (mod(istep,nwrite).eq.0)then             
              write(*,*)"MinArea: ",minHarea(istep),",  Ug: ",Ug(istep)
              call writeVTK(istep/nwrite)
          endif

      enddo
      write(*,*)"!! End time loop !!"
      write(*,'()')
      !end time loop
      !------------------------------------------------------

      !output results
      call output

      !output date and time
      call date_and_time(values=d)
      call system_clock(tf,tr,tmax)

      write(*,*) 'Successfully modeFold DONE !!'
      write(*,'(a11,i0,a,i0.2,a,i0.2,a,i0.2,a,i0.2)')&
            &' End time: ',d(1),'/',d(2),'/',d(3),'  ',d(5),':',d(6)
      if (tf < ti) then
         write(*,'(a,f10.1)')' Duration (s):',((tmax-ti)+tf+1)/dble(tr)
      else
         write(*,'(a,f10.1)')' Duration (s):',(tf-ti)/dble(tr)
      endif

  end program main
