      module bwcal 
        implicit none
      ! bwcal.f
      !
      ! This group of routines is for calculating balanced winds
      ! from mass fields

        contains

        subroutine bwgcal(r,pp,tp,ps,ts,dz,nr,np,nz,rlat, &
                        zp,vp,zz,tz,pz,rhoz,vz)
        ! This routine calculates the gradient wind as a function of radius and
        ! pressure, and as a function of radius and height, given the temperature
        ! as a function of pressure, the pressure at the surface at r=rmax,
        ! the surface temperature (assumed constant), and the latitude.
        !
        ! Input:  r   - radial coordinates (m)
        !         pp  - pressure coordinates (pa)
        !         tp  - temperature (k) as a function of r,p
        !         ps  - surface pressure (Pa) a function of r
        !         ts  - surface temperature (K) as a function of r
        !         dz  - height increment for z (m) coordinates
        !         nr  - No. of radii
        !         np  - No. of pressure levels
        !         nz  - No. of height levels
        !         rlat - latitude (deg N)
        !
        ! Output: zp   - height (m) as a function of r,p
        !         vp   - gradient wind (m/s) as a function of r,p
        !         zz   - height (m) coordinate values (m)
        !         tz   - temperature (K) as a function of r,z
        !         pz   - pressure (Pa) as a function of r,z
        !         rhoz - density (kg/m3) as a function of r,z
        !         vz   - gradient wind as a function of r,z
          use cons
          use hstatic

          integer,intent(in) :: nr,np,nz
          real,intent(in) :: dz,rlat
          real,intent(in) :: r(nr), pp(np), tp(nr,np)
          real,intent(in) :: ts(nr),ps(nr)

          real,intent(inout) :: zp(nr,np),vp(nr,np)
          real,intent(inout) :: zz(nz),tz(nr,nz),pz(nr,nz)
          real,intent(inout) :: rhoz(nr,nz),vz(nr,nz)

          real :: f,psi,t1,t2,tt,p1,p2,pt,z1,z2,zt,delz
          real :: rm1,rp1,pm1,pp1,pgrad,c,disc
          integer :: iplev
          integer :: i,k,m

          !* Calculate Coriolis parameter changed by J. Knaff (11/22/02)
          !* f  = 2.0*erot*sin(rlat*dtr)  !
          !
          ! Calculate Coriolis parameter
          ! here we use the natural coordinate system so that our statistical
          ! algorithms will work in the Southern Hemisphere. This involves using
          ! the absolute value of f for our calculation of vz.

          f  = abs( 2.0*erot*sin(rlat*dtr) )

          ! Extract ps at r=rmax
          psi = ps(nr)

          ! Calculate z at r=rmax at lowest pressure level
          t1=ts(nr)
          t2=tp(nr,np)
          p1=psi
          p2=pp(np)
          call tkness(p1,p2,t1,t2,delz)
          zp(nr,np) = zz(1) + delz

          ! Calculate the rest of the z values at r=rmax
          do k=np-1,1,-1
            t1 = tp(nr,k+1)
            t2 = tp(nr,k  )
            p1 = pp(k+1)
            p2 = pp(k  )
            call tkness(p1,p2,t1,t2,delz)
            zp(nr,k) = zp(nr,k+1) + delz
          end do

          ! Calculate z at upper most pressure (assumed constant with r)
          do i=1,nr-1
            zp(i,1) = zp(nr,1)
          end do

          ! Integrate z downward for r<rmax
          do i=1,nr-1
            do k=2,np
              t1 = tp(i,k  )
              t2 = tp(i,k-1)
              p1 = pp(k  )
              p2 = pp(k-1)
              call tkness(p1,p2,t1,t2,delz)
              zp(i,k) = zp(i,k-1) - delz
            end do
          end do

          ! Evaluate surface pressure
          pz(nr,1) = psi
          do i=1,nr-1
            t1 = tp(i,np)
            t2 = ts(i)
            z2 = zz(1)
            z1 = zp(i,np)
            p1 = pp(np)
            call p2cal(z1,z2,t1,t2,p1,p2)
            pz(i,1) = p2
          end do
 
          ! Evaluate surface temperature 
          do i=1,nr
            tz(i,1) = ts(i)
          end do

          ! Calculate t and p at remaining height levels
          do i=1,nr
            do m=2,nz
              ! Find first pressure level below current height level
              iplev = 0
              do k=1,np
                if (zp(i,k) .lt. zz(m)) then
                  iplev = k
                  exit
                end if
              end do

              ! Current height is below bottom pressure level. 
              ! Interpolate between surface and lowest pressure level.
              if (iplev .eq. 0) then
                t1 = tz(i,1)
                t2 = tp(i,1)
                z1 = zz(1)
                z2 = zp(i,1)
                p1 = pz(i,1)
                zt = zz(m)

                call tint(z1,z2,zt,t1,t2,tt)
                tz(i,m) = tt

                call p2cal(z1,zt,t1,tt,p1,pt)
                pz(i,m) = pt

              ! Current height level is above top pressure level. Assume
              !isothermal atmosphere from top pressure to current height.
              else if (iplev .eq. 1) then
                tz(i,m) = tp(i,1)

                t1 = tp(i,1)
                t2 = tz(i,m)
                z1 = zp(i,1)
                z2 = zz(m)
                p1 = pp(1)
                call p2cal(z1,z2,t1,t2,p1,p2)
                pz(i,m) = p2

              ! Current height level is between pressure levels
              else
                t1 = tp(i,iplev)
                t2 = tp(i,iplev-1)
                z1 = zp(i,iplev)
                z2 = zp(i,iplev-1)
                zt = zz(m)
                p1 = pp(iplev)

                call tint(z1,z2,zt,t1,t2,tt)
                tz(i,m) = tt

                call p2cal(z1,zt,t1,tt,p1,pt)
                pz(i,m) = pt
              end if

            end do
          end do

          ! Calculate density as a function of r,z
          do i=1,nr
            do m=1,nz
              rhoz(i,m) = pz(i,m)/(rd*tz(i,m))
            end do 
          end do

          ! Calculate gradient wind as a function of r,z

          ! Set v = 0 at r=0
          do m=1,nz
            vz(1,m) = 0.0
          end do

          do i=2,nr
            rm1 = r(i-1)
            if (i .eq. nr) then
              rp1 = r(i)
            else
              rp1 = r(i+1)
            end if

            do m=1,nz

              pm1 = pz(i-1,m)
              if (i .eq. nr) then
                pp1 = pz(i,m)
              else
                pp1 = pz(i+1,m)
              end if

              pgrad = (pp1-pm1)/( (rp1-rm1)*rhoz(i,m) )
              c = f*r(i)/2.0

              if (pgrad .ge. 0.0) then
                vz(i,m) = -c + sqrt(c*c + r(i)*pgrad)
              else
                disc = c*c + r(i)*pgrad
                if (disc .le. 0.0) then 
                  vz(i,m) = -c 
                else 
                  vz(i,m) = -c + sqrt(disc)
                end if
              end if
 
            end do

          end do 

          return

        end subroutine bwgcal

      end module bwcal
