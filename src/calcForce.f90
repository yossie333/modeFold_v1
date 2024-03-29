subroutine calcForce(t,n)
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program calculate the force on the surface of the vocal
! tract. 
!
! fx, fy, fz: force in the general coordinates
! 
! iforce = 1: force is calculated as the sinusoidal waves. The
! parameters of sinusoidal waves were defined by param.txt
! famp   :  amplitude of sin wave
! forcef :  frequency of the sin wave
! t      :  current time (input)
! n      :  current step (input)
!
! iforce = 2: force is calculated from the 1D flow equations.
! The equations are mainly based on Pelorson et al., (1994).
! The glottal flowrate was simply calculated from the minimum area
! minHarea : the minimum area for flow channel.
! Ps       : subglottal pressure (Pa)
! Ug(n)    : glottal flow at n step (cm3/s) 
! rho      : density of air (kg/m3)
! psurf    : pressure distribution along the surface
!*******************************************************************
        use variMode
        implicit none
        integer i,j,n
        double precision t,dx,h
        double precision ds,dy,dz

        do i=1,nsurfl
            do j=1,nsurfz
               fx(i,j)=0.d0
               fy(i,j)=0.d0
               fz(i,j)=0.d0
            enddo
        enddo
        do i=1,nxsup
           do j=1,nsurfz-1
               fdis(i,j)=0.d0
           enddo
        enddo

        if (iforce .eq. 1)then
                minHarea(n) = minval(harea)

                do i=1,nsurfl
                    do j=1,nsurfz
                       !fx(i,j)=famp*sin(2*pi*forcef*t)
                       fy(i,j)=famp*sin(2.d0*pi*forcef*t)
                       !fy(i,j)=famp
                       !fz(i,j)=famp*sin(2*pi*forcef*t)
                    enddo
                enddo
        elseif(iforce .eq. 0 )then

                !separation point
                minHarea(n) = minval(harea)
                do i=2,nxsup
                    if(harea(i) .eq. minHarea(n) .or. harea(i).le.0.d0)then
                            nsep = i
                            exit
                    endif
                enddo
                !write(*,*)"nsep",nsep

                if(minHarea(n) .gt. 0.d0)then
                    !Ug(n) = sqrt(2.d0*Ps/rho)*minHarea*tanh(ha*minHarea)
                    Ug(n) = sqrt(2.d0*Ps/rho)*minHarea(n)
                else
                    Ug(n) = 0.d0
                endif

                psurf(1:nxsup)=0.d0
                psurf(1) = Ps 
                if (minHarea(n) .gt. 0.d0)then
                    do i=2,nsep
                        dx = x(surfp(i,j)+1)-x(surfp(i-1,j)+1)
                        h = (harea(i)+harea(i-1))/(2.d0*zmax)
                        !surface pressure (Pa)
                        psurf(i) = psurf(i-1) + 0.5d0*rho*Ug(n)**2*(1.d0/harea(i-1)**2-1.d0/harea(i)**2)&
                                              - 12.d0*mu*dx/(zmax*h**3)*Ug(n)*1000.d0
                    enddo
                else
                    do i=2,nsep-1
                        psurf(i) = Ps
                    enddo
                endif

                do i=2,nsep-1
                    do j=2,nsurfz-1
                       dx = 0.5d0*(u(surfp(i+1,j)+1)-u(surfp(i-1,j)+1))
                       dy = 0.5d0*(v(surfp(i+1,j)+1)-v(surfp(i-1,j)+1))
                       ds = sqrt(dx**2 + dy**2)
                       dz = 0.5d0*(w(surfp(i,j+1)+1)-w(surfp(i,j-1)+1))

                       !Pa to N (mm2 -> m2)
                       fx(i,j)=psurf(i)*ds*dz*1.d-6*cos(degree(2,i,j))*sin(degree(1,i,j))
                       fy(i,j)=-psurf(i)*ds*dz*1.d-6*cos(degree(2,i,j))*cos(degree(1,i,j))
                       fz(i,j)=psurf(i)*ds*dz*1.d-6*sin(degree(2,i,j))
                       !fx(i,j)=psurf(i)*sarea(i,j)*1.d-6*cos(degree(2,i,j))*sin(degree(1,i,j))
                       !fy(i,j)=-psurf(i)*sarea(i,j)*1.d-6*cos(degree(2,i,j))*cos(degree(1,i,j))
                       !fz(i,j)=psurf(i)*sarea(i,j)*1.d-6*sin(degree(2,i,j))
                    enddo
                enddo

        endif

end subroutine
