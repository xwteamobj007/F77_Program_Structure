program fcs
!***********************************************************************
!*                                                                     *
!*                      Jiangsu University (UJS)                       *
!*     -----------------------------------------------------------     *
!*     Copyright 2018-2019 Jiangsu University all rights reserved.     *
!*                                                                     *
!***********************************************************************
!                                                                      *
! The program (FCS) is developed to analyze the structure of F77 code. *
! The first version was written by Dr. Wei XU in Jiangsu University on *
! Jan, 1st, 2018 by Fortran 90/95 language.                            *
!                                                                      *
! Revised by Dr. Wei XU in UJS on Oct, 22nd, 2019.                     *
!                                                                      *
!***********************************************************************
!*                                                                     *
!*   *   *        ****         ***        *****       ***        ***   *
!*   *   *           *        *   *       *          *   *      *   *  *
!*   *   *           *        *           *          *          *      *
!*   *   *           *         ***        ****       *           ***   *
!*   *   *           *            *       *          *              *  *
!*   *   *       *   *        *   *       *          *   *      *   *  *
!*    ***         ***          ***        *           ***        ***   *
!*                                                                     *
!***********************************************************************
  use f77mod
  implicit none
!
  write(*, *)
  write(*, *) 'Reading Fortran77 code Files is processing....'
  call read_f77()
  pause
!
  write(*, *)
  write(*, *) 'Structure analysis of Fortran77 code is processing....'
  call analyze_f77()
  pause
!
  write(*, *)
  write(*, *) 'Structure inf. of Fortran77 code is outputing....'
  call output_f77()
!
  write(*, *)
  write(*, *) 'Sturcture inf. (F77_1.txt, F77_2.txt) written completely.'
pause
end program