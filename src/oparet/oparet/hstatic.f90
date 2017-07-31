      module hstatic
      ! This is a group of subroutines for evaluating the hydrostatic
      ! equation. The physical constants in common block /cons/ must
      ! be provided. Included routines:
      !
      ! ** tint
      ! ** ztint
      ! ** tkness
      ! ** p2cal
        implicit none
       
        contains

        subroutine tint(z1,z2,zt,t1,t2,tt)
        ! This routine linearly interpolates the temperature to the level
        ! zt to give tt. The level zt must be between z1 and z2, with
        ! temperatures t1 and t2. 
          use cons 

          real,intent(in) :: z1,z2,zt,t1,t2
          real,intent(inout) :: tt

          real :: slope,yint

          ! Check for zero thickness
          if (z1 .eq. z2) then
            tt = 0.0
            return
          end if

          ! Check for isothermal case
          if (t1 .eq. t2) then
            tt = t1
            return
          end if

          slope = (t2-t1)/(z2-z1)
          yint  = (t1*z2 - t2*z1)/(z2-z1)
          tt = slope*zt + yint

          return

        end subroutine tint

        subroutine ztint(p1,p2,z1,z2,t1,t2,pt,zt,tt)
        ! This routine calculates the height zt and temperature tt at a specified
        ! pressure level pt which lies between p1 and p2. The heights and temperatures
        ! at p1,p2 are z1,z2 and t1,t2, respecitively. A constant lapse rate atmosphere is assumed
        ! between p1 and p2, and it is also assumed that p1>p2 and z1<z2.
          use cons 

          real,intent(in) :: p1,p2,z1,z2,t1,t2
          real,intent(inout) :: pt,zt,tt 

          real :: dt,tbar,gamma,a,ai

          ! Check for zero pressure
          if (p1 .le. 0.0 .or. p2 .le. 0.0) then
            pt = 0.0
            zt = 0.0
            tt = 0.0
            return
          end if

          ! Check for zero absolute temperatures
          if (t1 .le. 0.0 .or. t2 .le. 0.0) then
            pt = 0.0
            zt = 0.0
            tt = 0.0
            return
          endif

          ! Check for zero thickness
          if (p1 .eq. p2 .or. z1 .eq. z2) then
            pt = 0.0
            zt = 0.0
            tt = 0.0
            return
          end if

          ! Check for nearly isothermal atmosphere
          dt = abs(t1-t2)
          if (dt .lt. 0.1) then
            tbar = 0.5*(t1+t2)
            tt  = tbar
            zt  = z1 + (rd/g)*tbar*alog(p1/pt)
            return
          end if
          
          ! General case
          gamma = (t2-t1)/(z2-z1)
          a     = g/(rd*gamma)
          ai    = 1.0/a
          zt    = z1 + (t1/gamma)*((p1/pt)**ai - 1.0)
          tt    = t1 + gamma*(zt-z1)

          return

        end subroutine ztint

        subroutine tkness(p1,p2,t1,t2,dz)
        ! This routine calculates the thickness dz (m) between pressure levels
        ! p1,p2 (Pa) given the temperatures t1,t2 (K). A constant lapse rate as a
        ! function of z is assumed between the levels. dz is always positive,
        ! unless non-physical input values were provided.
          use cons 

          real,intent(in) :: p1,p2,t1,t2
          real,intent(inout) :: dz

          real :: dt,tbar,dzt

          ! Check for zero pressure
          if (p1 .le. 0.0 .or. p2 .le. 0.0) then
            dz = 0.0
            return
          end if

          ! Check for zero absolute temperatures
          if (t1 .le. 0.0 .or. t2 .le. 0.0) then
            dz = 0.0
            return
          end if

          ! Check for zero thickness
          if (p1 .eq. p2) then
            dz = 0.0
            return
          end if

          ! Check for nearly isothermal atmosphere
          dt = abs(t1-t2)
          if (dt .lt. 0.1) then
            tbar = 0.5*(t1+t2)
            dzt = (rd/g)*tbar*alog(p1/p2)
            dz  = abs(dzt)
            return
          endif

          ! General case
          dzt = (rd/g)*(t1-t2)*alog(p1/p2)/(alog(t1/t2))
          dz  = abs(dzt)

          return

        end subroutine tkness

        subroutine p2cal(z1,z2,t1,t2,p1,p2)
        ! This routine calculates the pressure p2 (Pa) at height z2 (m)
        ! given the temperatures t1,t2 (K), the height z1 (m) and pressure
        ! p1 (Pa). A constant lapse rate as a function of z is assumed
        ! between the levels.
          use cons

          real,intent(in) :: z1,z2,t1,t2,p1
          real,intent(inout) :: p2

          real :: dt,tbar,gm,a

          ! Check for negative heights
          if (z1 .lt. 0.0 .or. z2 .lt. 0.0) then
            p2 = 0.0
            return
          end if

          ! Check for zero absolute temperatures
          if (t1 .le. 0.0 .or. t2 .le. 0.0) then
            p2 = 0.0
            return
          end if

          ! Check for zero thickness
          if (z1 .eq. z2) then
            p2 = p1
            return
          end if

          ! Check for nearly isothermal atmosphere
          dt = abs(t1-t2)
          if (dt .lt. 0.1) then
            tbar = 0.5*(t1+t2)
            p2 = p1*exp( -g*(z2-z1)/(rd*tbar) )
            return
          end if

          ! General case
          gm = -(t2-t1)/(z2-z1)
          a= g/(rd*gm)
          p2 = p1*( (t2/t1)**a )

          return

        end subroutine p2cal

      end module hstatic
