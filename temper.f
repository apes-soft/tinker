c
c
c     #############################################################
c     ##  COPYRIGHT (C) 2003 by Alan Grossfield & Jay W. Ponder  ##
c     ##                   All Rights Reserved                   ##
c     #############################################################
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine temper  --  thermostat applied at half step  ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "temper" applies a velocity correction at the half time step
c     as needed for the Nose-Hoover extended system thermostat
c
c     literature references:
c
c     D. Frenkel and B. Smit, "Understanding Molecular Simulation,
c     2nd Edition", Academic Press, San Diego, CA, 2002; see Appendix
c     E.2 for implementation details
c
c     G. J. Martyna, M. E. Tuckerman, D. J. Tobias and M. L. Klein,
c     "Explicit Reversible Integrators for Extended Systems Dynamics",
c     Molecular Physics, 87, 1117-1157 (1996)
c
c
      subroutine temper (dt)
      use sizes
      use atoms
      use bath
      use mdstuf
      use moldyn
      use units
      implicit none
      integer i,j,nc,ns
      real*8 dt,dtc,dts
      real*8 dt2,dt4,dt8
      real*8 eksum,ekt
      real*8 scale,expterm
      real*8 w(3)
      real*8 ekin(3,3)
c
c
c     make half-step velocity correction for Nose-Hoover system
c
      call kinetic (eksum,ekin)
      ekt = gasconst * kelvin
      nc = 5
      ns = 3
      dtc = dt / dble(nc)
      w(1) = 1.0d0 / (2.0d0-2.0d0**(1.0d0/3.0d0))
      w(2) = 1.0d0 - 2.0d0*w(1)
      w(3) = w(1)
      scale = 1.0d0
      do i = 1, nc
         do j = 1, ns
            dts = w(j) * dtc
            dt2 = 0.5d0 * dts
            dt4 = 0.25d0 * dts
            dt8 = 0.125d0 * dts
            gnh(4) = (qnh(3)*vnh(3)*vnh(3)-ekt) / qnh(4)
            vnh(4) = vnh(4) + gnh(4)*dt4
            gnh(3) = (qnh(2)*vnh(2)*vnh(2)-ekt) / qnh(3)
            expterm = exp(-vnh(4)*dt8)
            vnh(3) = expterm * (vnh(3)*expterm+gnh(3)*dt4)
            gnh(2) = (qnh(1)*vnh(1)*vnh(1)-ekt) / qnh(2)
            expterm = exp(-vnh(3)*dt8)
            vnh(2) = expterm * (vnh(2)*expterm+gnh(2)*dt4)
            gnh(1) = (2.0d0*eksum-dble(nfree)*ekt) / qnh(1)
            expterm = exp(-vnh(2)*dt8)
            vnh(1) = expterm * (vnh(1)*expterm+gnh(1)*dt4)
            scale = scale * exp(-vnh(1)*dt2)
            eksum = eksum * scale * scale
            gnh(1) = (2.0d0*eksum-dble(nfree)*ekt) / qnh(1)
            expterm = exp(-vnh(2)*dt8)
            vnh(1) = expterm * (vnh(1)*expterm+gnh(1)*dt4)
            gnh(2) = (qnh(1)*vnh(1)*vnh(1)-ekt) / qnh(2)
            expterm = exp(-vnh(3)*dt8)
            vnh(2) = expterm * (vnh(2)*expterm+gnh(2)*dt4)
            gnh(3) = (qnh(2)*vnh(2)*vnh(2)-ekt) / qnh(3)
            expterm = exp(-vnh(4)*dt8)
            vnh(3) = expterm * (vnh(3)*expterm+gnh(3)*dt4)
            gnh(4) = (qnh(3)*vnh(3)*vnh(3)-ekt) / qnh(4)
            vnh(4) = vnh(4) + gnh(4)*dt4
         end do
      end do
      do i = 1, n
         do j = 1, 3
            v(j,i) = scale * v(j,i)
         end do
      end do
      return
      end
c
c
c     ###############################################################
c     ##                                                           ##
c     ##  subroutine temper2  --  thermostat applied at full step  ##
c     ##                                                           ##
c     ###############################################################
c
c
c     "temper2" computes the instantaneous temperature and applies a
c     thermostat via Berendsen or Bussi-Parrinello velocity scaling,
c     Andersen stochastic collisions or Nose-Hoover extended system
c
c     literature references:
c
c     H. J. C. Berendsen, J. P. M. Postma, W. F. van Gunsteren,
c     A. DiNola and J. R. Hauk, "Molecular Dynamics with Coupling
c     to an External Bath", Journal of Chemical Physics, 81,
c     3684-3690 (1984)
c
c     G. Bussi and M. Parrinello, "Stochastic Thermostats: Comparison
c     of Local and Global Schemes", Computer Physics Communications,
c     179, 26-29 (2008)
c
c     H. C. Andersen, "Molecular Dynamics Simulations at Constant
c     Pressure and/or Temperature", Journal of Chemical Physics,
c     72, 2384-2393 (1980)
c
c
      subroutine temper2 (dt,eksum,ekin,temp)
      use sizes
      use atomid
      use atoms
      use bath
      use mdstuf
      use molcul
      use moldyn
      use units
      implicit none
      integer i,j,k,m
      integer nc,ns
      real*8 dt,dtc,dts
      real*8 dt2,dt4,dt8
      real*8 eksum,ekt
      real*8 scale
      real*8 temp,expterm
      real*8 w(3)
      real*8 ekin(3,3)
c
c
c     get instantaneous temperature from the kinetic energy
c
      call kinetic (eksum,ekin)
c
c     make full-step velocity correction for Nose-Hoover system
c
      ekt = gasconst * kelvin
      nc = 5
      ns = 3
      dtc = dt / dble(nc)
      w(1) = 1.0d0 / (2.0d0-2.0d0**(1.0d0/3.0d0))
      w(2) = 1.0d0 - 2.0d0*w(1)
      w(3) = w(1)
      scale = 1.0d0
      do i = 1, nc
         do j = 1, ns
            dts = w(j) * dtc
            dt2 = 0.5d0 * dts
            dt4 = 0.25d0 * dts
            dt8 = 0.125d0 * dts
            gnh(4) = (qnh(3)*vnh(3)*vnh(3)-ekt) / qnh(4)
            vnh(4) = vnh(4) + gnh(4)*dt4
            gnh(3) = (qnh(2)*vnh(2)*vnh(2)-ekt) / qnh(3)
            expterm = exp(-vnh(4)*dt8)
            vnh(3) = expterm * (vnh(3)*expterm+gnh(3)*dt4)
            gnh(2) = (qnh(1)*vnh(1)*vnh(1)-ekt) / qnh(2)
            expterm = exp(-vnh(3)*dt8)
            vnh(2) = expterm * (vnh(2)*expterm+gnh(2)*dt4)
            gnh(1) = (2.0d0*eksum-dble(nfree)*ekt) / qnh(1)
            expterm = exp(-vnh(2)*dt8)
            vnh(1) = expterm * (vnh(1)*expterm+gnh(1)*dt4)
            scale = scale * exp(-vnh(1)*dt2)
            eksum = eksum * scale * scale
            gnh(1) = (2.0d0*eksum-dble(nfree)*ekt) / qnh(1)
            expterm = exp(-vnh(2)*dt8)
            vnh(1) = expterm * (vnh(1)*expterm+gnh(1)*dt4)
            gnh(2) = (qnh(1)*vnh(1)*vnh(1)-ekt) / qnh(2)
            expterm = exp(-vnh(3)*dt8)
            vnh(2) = expterm * (vnh(2)*expterm+gnh(2)*dt4)
            gnh(3) = (qnh(2)*vnh(2)*vnh(2)-ekt) / qnh(3)
            expterm = exp(-vnh(4)*dt8)
            vnh(3) = expterm * (vnh(3)*expterm+gnh(3)*dt4)
            gnh(4) = (qnh(3)*vnh(3)*vnh(3)-ekt) / qnh(4)
            vnh(4) = vnh(4) + gnh(4)*dt4
         end do
      end do
      do i = 1, n
         do j = 1, 3
            v(j,i) = scale * v(j,i)
         end do
      end do
c
c     recompute kinetic energy and instantaneous temperature
c
      call kinetic (eksum,ekin)
      temp = 2.0d0 * eksum / (dble(nfree) * gasconst)
      return
      end
