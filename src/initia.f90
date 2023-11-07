subroutine initia
!*******************************************************************
! Fortran program for vocal fold oscillation modeFold ver1.0
! 2023/Nov/7    by  Tsukasa Yoshinaga
! 
! This program defines the initial values for the simulation.
! Variables:
! qi, qdot  : mode displacements and velocity
! qo, qodot : previous time of mode displacements and velocity
! fi        : external force on each mode
! u, v, w   : coordinates for grid points(initial value + displacement)
! Ug        : glottal flow rate 
!*******************************************************************
        use variMode
        implicit none
        integer i

        allocate(fi(nmode),qi(nmode),qidot(nmode))
        allocate(qo(nmode),qodot(nmode))
        allocate(u(nop),v(nop),w(nop))
        allocate(uf(nop),vf(nop),wf(nop))
        allocate(fx(nsurfl,nsurfz),fy(nsurfl,nsurfz),fz(nsurfl,nsurfz))

        do i=1,nmode
            fi(i)=0.d0
            qi(i)=0.d0
            qo(i)=0.d0
            qidot(i)=0.d0
            qodot(i)=0.d0
        enddo
        do i=1,nop
            u(i)=0.d0 + x(i)
            v(i)=0.d0 + y(i)
            w(i)=0.d0 + z(i)
        enddo

        Ug(1) = 0.d0

end subroutine initia
