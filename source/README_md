

1) How nchunk is calculated :


Variables used in nchunk-related calculations (declared inside chunks.f):

 -  nchunk     total number of spatial regions for PME grid
 -  nchk1      number of spatial regions along the a-axis
 -  nchk2      number of spatial regions along the b-axis
 -  nchk3      number of spatial regions along the c-axis
 -  ngrd1      number of grid points per region along a-axis
 -  ngrd2      number of grid points per region along b-axis
 -  ngrd3      number of grid points per region along c-axis
 -  nlpts      PME grid points to the left of center point
 -  nrpts      PME grid points to the right of center point
 -  grdoff     offset for index into B-spline coefficients
 -  pmetable   PME grid spatial regions involved for each site
 

nchunk calculation is done inside the getchunk subroutine inside the kewald.f :

**********

c
c     initialize total chunks and number along each axis
c
      nchunk = 1
      nchk1 = 1
      nchk2 = 1
      nchk3 = 1
c
c     evaluate use of two to six chunks along each axis
c
      do i = 2, 6
         if (nthread.gt.nchunk .and. mod(nfft1,i).eq.0) then
            nchk1 = i
            nchunk = nchk1 * nchk2 * nchk3
         end if
         if (nthread.gt.nchunk .and. mod(nfft2,i).eq.0) then
            nchk2 = i
            nchunk = nchk1 * nchk2 * nchk3
         end if
         if (nthread.gt.nchunk .and. mod(nfft3,i).eq.0) then
            nchk3 = i
            nchunk = nchk1 * nchk2 * nchk3
         end if
      end do
c
c     set number of grid points per chunk along each axis
c
      ngrd1 = nfft1 / nchk1
      ngrd2 = nfft2 / nchk2
      ngrd3 = nfft3 / nchk3
c
c     set grid points to left and right, and B-spline offset
c
      nlpts = (bsorder-1) / 2
      nrpts = bsorder - nlpts - 1
      grdoff = (bsorder+1)/2 + 1

************

The maximum value of nchunk is 216 (6*6*6).

************

nfft1, nfft2 and nfft3 are determined inside the kewald subroutine:

 - maxfft is set to 576
 - maxpower is set to 54


c
c     grid size must be even, with prime factors of 2, 3 and 5
c
      nfft1 = maxfft
      nfft2 = maxfft
      nfft3 = maxfft
      do i = maxpower, 1, -1
         k = multi(i)
         if (k .le. maxfft) then
            if (k .ge. ifft1)  nfft1 = k
            if (k .ge. ifft2)  nfft2 = k
            if (k .ge. ifft3)  nfft3 = k
         end if
      end do
      minfft = 16
      if (nfft1 .lt. minfft)  nfft1 = minfft
      if (nfft2 .lt. minfft)  nfft2 = minfft
      if (nfft3 .lt. minfft)  nfft3 = minfft


---> maximum value of nfft1 etc. is 576
---> mimumum value is 16 
---> possible values inbetween are : 18,20, 24, 30, 32,36,40, 48, 50, 54, 60, 
     64, 72, 80, 90, 96, 100, 108, 120, 128, 144, 150, 160, 162, 180, 192, 200,
     216, 240, 250, 256, 270, 288, 300, 320, 324, 360, 384, 400, 432, 450, 480,
     500, 512 and 540

---> the iffts are calculated : 

      ifft1 = int(xbox*dens-delta) + 1
      ifft2 = int(ybox*dens-delta) + 1
      ifft3 = int(zbox*dens-delta) + 1

      where delta= 1.0d-8 and dens is either 1.2d0 or 0.75d0(if .not. use_bounds)
      xbox is a lenght of the a-axis of periodic box in Angstroms

************
