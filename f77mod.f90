module f77mod
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
  implicit none
  private
  public :: read_f77, analyze_f77, output_f77

! define informations of functions and subroutines
  type infor
    private
    character*28 :: fncnam = ''            ! function name
    integer :: fncnum = 0                  ! function number
    integer :: filnum = 0                  ! file number
    integer :: linnum = 0                  ! line number
    integer :: pagnum = 0                  ! page number
    type(infor), pointer :: next => null() ! point to next function information
  end type infor
  
! define common employed in each function and subroutine
  type cmnnm
    private
    character*28 :: cmnnam = ''            ! common name
    logical :: prtflg = .FALSE.            ! print out or not
    type(cmnnm), pointer :: next => null() ! point to next common
  end type cmnnm

! define invoking and invoked information of functions and subroutines
  type funct
    private
    character*28 :: fncnam = ''            ! function name
    integer :: fnctyp = 0                  ! function type, 1: program; 2: function;
                                           !                3: subroutine; 4: blockdata;
    integer :: filnum = 0                  ! file number
    integer :: lclnum = 0                  ! function number in file list
    integer :: linnum = 0                  ! line number
    integer :: pagnum = 0                  ! page number
    integer :: ivknum = 0                  ! invoking functions' number
    integer :: ivkdnm = 0                  ! number of invoked functions
    type(infor), pointer :: nink => null() ! invoking function list
    type(infor), pointer :: nind => null() ! invoked function list
    type(cmnnm), pointer :: ncmn => null() ! employed common list
    type(funct), pointer :: next => null() ! point to next function
  end type funct
!
! define functions' list and their number
!
  integer, save :: fncnum = 0
  type(funct), pointer :: fnclst, ffunct, cfunct
  type(infor), pointer :: fflink, cflink
  type(cmnnm), pointer :: cmlink, cclink

  contains

  subroutine read_f77()
    implicit none
    character*72 :: oneline
    character*1  :: onechar
    character*28 :: filname
    character*28 :: fncname
    character*28 :: cmnname, cmnnam2
    character*7  :: char7
    character*8  :: char8
    character*9  :: char9
    character*10 :: char10
    character*28 :: char30
    character*72 :: char72
    integer :: ifil, ilin, ipag, ifnc, icll, icmn, ftyp, funt, iunt, i
    integer :: ilen, ipos, jpos, iost, temp, numf77, numlpg
    integer :: indexp, indexf, indexs, indexb, indexc, indexm, indexe
    integer :: indexk, indexq, indexl, index0, index1

    funt = get_unit()
    open(funt, file = 'Flist.txt', status = 'old', iostat = iost)
    if(iost /= 0) then
      write(*, *) ''
      write(*, *) 'ERROR: Flist.txt does not exist in current folder.'
      write(*, *) ''
      pause
      stop
    endif

    write(*, *)
    write(*, *) 'Please input number of f77 code files in Flist >: '
!   read(*, '(8i)') numf77
    numf77 = 14

    write(*, *)
    write(*, *) 'Please input number of lines in one page >: '
!   read(*, '(8i)') numlpg
    numlpg = 62

    allocate(fnclst)
    cfunct => fnclst

    do ifil = 1, numf77

      ilin = 0
      ipag = 0
      ifnc = 0
      icll = 0
      icmn = 0

      read(funt, *) filname
      filname=adjustl(filname)
      iunt = get_unit()
      open(iunt, file = trim(filname), status = 'old', iostat = iost)
      if(iost /= 0) then
        write(*, *) ''
        write(*, *) 'ERROR: '//adjustr(filname)//' does not exist in current folder.'
        write(*, *) ''
        stop
      endif

      do
        indexq = 0
        indexp = 0
        indexf = 0
        indexs = 0
        indexb = 0
        indexc = 0
        indexm = 0
        indexe = 0
!
        read(iunt, '(72a)', iostat = iost) oneline
        if(iost /= 0) exit        ! The end of f77 code file
        ilin = ilin + 1
        ilen = len_trim(oneline)
        if(ilen <= 7) cycle       ! empty line
!
!       lowcase oneline
!
        do i = 1, 72
          call lwrcas(oneline(i:i),oneline(i:i))
        enddo
        onechar = oneline(1:1)    ! 1st letter of each line
!
!       f77 comment line in intel fortran complier
!
        if(onechar == 'c' .or. onechar == '*' .or. onechar == '!') cycle
        
        onechar = char72(6:6)
        char72 = adjustl(oneline)

        indexq = index(char72, "'")
        indexl = index(char72, "'", back = .true.)
        indexe = index(char72, 'end')
        indexc = index(char72, 'call')
        indexm = index(char72, 'common')
        indexp = index(char72, 'program')
        indexf = index(char72, 'function')
        indexs = index(char72, 'subroutine')
        indexb = index(char72, 'block data')
!
!       to remove 'subroutine gendset', 'call gendset' etc.
!
        if(indexc >= 1 .and. indexe > indexc+ 4) indexe = 0
        if(indexs == 1 .and. indexe > indexs+10) indexe = 0
        if(indexb == 1 .and. indexe > indexb+10) indexe = 0
        if(indexp == 1 .and. indexe > indexb+ 7) indexe = 0
        if(indexf >= 1 .and. indexe > indexf+ 8) indexe = 0
!
        if(indexc >  indexq.and.indexf < indexl) indexc = 0
        if(indexf >  1 .and. onechar /= ' '    ) indexf = 0
        if(indexf >  indexq.and.indexf < indexl) indexf = 0
!
        temp = mod(ilin, numlpg)
        ipag = ilin/numlpg
        if(temp > 0) ipag = ipag + 1
!
!       for program
!
        if(indexe == 0 .and. indexp == 1) then
          if(char72(indexp+7:indexp+7) /= ' ') cycle
          ifnc = ifnc + 1
          fncnum = fncnum + 1
          char30 = trim(adjustl(char72(indexp+7:)))
          ipos = index(char30, '(')
          if(ipos == 0) fncname = trim(char30)
          if(ipos >  0) fncname = trim(char30(1:(ipos-1)))
          fncname = adjustr(fncname)
!
          ftyp = 1
          cfunct%fncnam = fncname
          cfunct%fnctyp = ftyp
          cfunct%filnum = ifil
          cfunct%lclnum = ifnc
          cfunct%linnum = ilin
          cfunct%pagnum = ipag
!
!       for function
!
        elseif(indexe == 0 .and. indexf >= 1) then
          if(char72(indexf+8:indexf+8) /= ' ') cycle
          ifnc = ifnc + 1
          fncnum = fncnum + 1
          char30 = trim(adjustl(char72(indexf+8:)))
          ipos = index(char30, '(')
          if(ipos >  0) fncname = trim(char30(1:(ipos-1)))
          if(ipos == 0) fncname = trim(char30)
          fncname = adjustr(fncname)
!
          ftyp = 2
          cfunct%fncnam = fncname
          cfunct%fnctyp = ftyp
          cfunct%filnum = ifil
          cfunct%lclnum = ifnc
          cfunct%linnum = ilin
          cfunct%pagnum = ipag
!
!       for subroutine
!
        elseif(indexe == 0 .and. indexs == 1) then
          if(char72(indexs+10:indexs+10) /= ' ') cycle
          ifnc = ifnc + 1
          fncnum = fncnum + 1
          char30 = trim(adjustl(char72(indexs+10:)))
          ipos = index(char30, '(')
          if(ipos >  0) fncname = trim(char30(1:(ipos-1)))
          if(ipos == 0) fncname = trim(char30)
          fncname = adjustr(fncname)
!
          ftyp = 3
          cfunct%fncnam = fncname
          cfunct%fnctyp = ftyp
          cfunct%filnum = ifil
          cfunct%lclnum = ifnc
          cfunct%linnum = ilin
          cfunct%pagnum = ipag
!
!       for block data
!
        elseif(indexe == 0 .and. indexb == 1) then
          if(char72(indexb+10:indexb+10) /= ' ') cycle
          ifnc = ifnc + 1
          fncnum = fncnum + 1
          char30 = trim(adjustl(char72(indexb+10:)))
          ipos = index(char30, '(')
          if(ipos >  0) fncname = trim(char30(1:(ipos-1)))
          if(ipos == 0) fncname = trim(char30)
          fncname = adjustr(fncname)
!
          ftyp = 4
          cfunct%fncnam = fncname
          cfunct%fnctyp = ftyp
          cfunct%filnum = ifil
          cfunct%lclnum = ifnc
          cfunct%linnum = ilin
          cfunct%pagnum = ipag
!
!       for call statement
!
        elseif(indexe == 0 .and. indexc > 0) then
          if(char72(indexc+4:indexc+4) /= ' ') cycle
!
          if(icll == 0) then
            allocate(cfunct%nink)
            fflink => cfunct%nink
            cflink => cfunct%nink
          endif
!
          if(icll >= 1) then
            allocate(cflink%next)
            cflink => cflink%next
          endif
!
          icll = icll + 1
          char30 = trim(adjustl(char72(indexc+4:)))
          indexk = index(char30, '(')
          if(indexk >  0) fncname = trim(char30(1:(indexk-1)))
          if(indexk == 0) fncname = trim(char30)
          fncname = adjustr(fncname)
!
          cflink%fncnam = fncname
          cflink%filnum = ifil
          cflink%linnum = ilin
          cflink%pagnum = ipag
!
!       for common statement
!
        elseif(indexm > 0) then
          index0 = index(char72, '/')
          index1 = index(char72, '/', back = .true.)
          if(index0 == 0 .or. index1 == 0) cycle
          
          if(icmn == 0) then
            allocate(cfunct%ncmn)
            cmlink => cfunct%ncmn
            cclink => cfunct%ncmn
          endif
!
          if(icmn >= 1) then
            allocate(cclink%next)
            cclink => cclink%next
          endif
!
          icmn = icmn + 1
          char30 = trim(adjustl(char72(indexm+6:)))
          index0 = index(char30, '/')
          index1 = index(char30, '/', back = .true.)
          
          cmnname = adjustr(char30(index0+1:index1-1))
          cclink%cmnnam = cmnname
!
!       for the end of each function
!
        elseif(indexe == 1) then
          ilen = len_trim(char72)
!
!         for 'end' line
!
          if(ilen == 3) then
            cfunct%ivknum = icll
            icll = 0
            icmn = 0
            allocate(cfunct%next)
            cfunct => cfunct%next
!
!         for 'end program', 'end function', 'end subroutine', 'end block data'
!
          elseif(indexp > 0 .or. indexf > 0 .or. indexs > 0 .or. indexb > 0) then
            cfunct%ivknum = icll
            icll = 0
            icmn = 0
            allocate(cfunct%next)
            cfunct => cfunct%next
          endif
        endif
      enddo
!
      write(*, *) 'The file '//adjustr(filname)//' is read completed.'
      close(iunt)
    enddo
    if(associated(cfunct)) nullify(cfunct)

    return
  end subroutine read_f77

  subroutine analyze_f77()
    implicit none
    character*28 :: fncname
    character*28 :: fncnam1
    character*28 :: cmnname
    character*28 :: cmnnam2
    character*7  :: char7
    character*8  :: char8
    character*9  :: char9
    character*16 :: char10
    character*28 :: char30
    character*72 :: char72
    integer :: ifil, ilin, ipag, iunt, ifnc, iend
    integer :: ilen, ipos, ipag1, icll, inn, iok, icc, fnum,cnum
!
!   check the number of the invoked functions
!
    write(*,*) 'The invoked functions analysis is begining.'
    cfunct => fnclst
    do
      if(.not. associated(cfunct)) exit
      cflink => cfunct%nink
      do
        if(.not. associated(cflink)) exit
        fncname = cflink%fncnam
        inn = 0
        iok = 0
        ffunct => fnclst
        do
          if(.not. associated(ffunct)) exit
          inn = inn + 1
          char30 = ffunct%fncnam
          if(fncname == char30) then
            iok = 1
            cflink%fncnum = inn
            ffunct => null()
          endif
          if(iok == 1) exit
          ffunct => ffunct%next
        enddo
        cflink => cflink%next
      enddo
      cfunct => cfunct%next
    enddo
!
!   check the information of the invoked functions
!
    cfunct => fnclst
    do
      if(.not. associated(cfunct)) exit
      cnum = 0
      fncname = cfunct%fncnam
!
      ffunct => fnclst
      do
        if(.not. associated(ffunct)) exit
        char30 = ffunct%fncnam
        if(fncname == char30) then
          ffunct => ffunct%next
          cycle
        endif
!
        cflink => ffunct%nink
        do
          if(.not. associated(cflink)) exit
          fncnam1 = cflink%fncnam
          if(fncname == fncnam1) then
!
            if(cnum == 0) then
              allocate(cfunct%nind)
              fflink => cfunct%nind
            endif
!
            if(cnum >= 1) then
              allocate(fflink%next)
              fflink => fflink%next
            endif
!
            cnum = cnum + 1
            ifil = cflink%filnum
            ilin = cflink%linnum
            ipag = cflink%pagnum
            fflink%fncnam = char30
            fflink%filnum = ifil
            fflink%linnum = ilin
            fflink%pagnum = ipag
          endif
          cflink => cflink%next
        enddo
        ffunct => ffunct%next
      enddo
!
      cfunct%ivkdnm = cnum
      cfunct => cfunct%next
    enddo
    write(*,*) 'The invoked functions analysis is finished.'
!
!   check the number of the invoking functions
!
    cfunct => fnclst
    do
      if(.not. associated(cfunct)) exit
      cflink => cfunct%nind
      do
        if(.not. associated(cflink)) exit
        fncname = cflink%fncnam
        inn = 0
        ffunct => fnclst
        do
          if(.not. associated(ffunct)) exit
          inn = inn + 1
          char30 = ffunct%fncnam
          if(fncname == char30) then
            cflink%fncnum = inn
          endif
          ffunct => ffunct%next
        enddo
        cflink => cflink%next
      enddo
      cfunct => cfunct%next
    enddo
!
!   check common arguments employed in each function
!
    write(*,*) 'The common analysis is begining.'
    cfunct => fnclst
    do
      if(.not. associated(cfunct)) exit
!     fncname = cfunct%fncnam
      cmlink => cfunct%ncmn
!
      do
        if(.not. associated(cmlink)) exit
        cmnname = cmlink%cmnnam
        ffunct => cfunct%next
        do
          if(.not. associated(ffunct)) exit
!         fncnam1 = ffunct%fncnam
      !   if(fncname == fncnam1) then
      !     ffunct => ffunct%next
      !     cycle
      !   endif

          cclink => ffunct%ncmn
          do
            if(.not.associated(cclink)) exit
            if(cclink%prtflg) then
              cclink => cclink%next
              cycle
            endif
            
            cmnnam2 = cclink%cmnnam
            if(cmnname == cmnnam2) then
              cclink%prtflg = .TRUE.
              ffunct => ffunct%next
              exit
            endif
       
            cclink => cclink%next
          enddo
!
          ffunct => ffunct%next
        enddo

        cmlink =>cmlink%next
      enddo
!
      cfunct => cfunct%next
    enddo
    write(*,*) 'The common analysis is finished.'
    
    return
  end subroutine analyze_f77

  subroutine output_f77()
    implicit none
    character(len =  2) :: onechar
    character(len = 16) :: twochar
    character(len = 28) :: fncname
    character(len = 28) :: cmnname
    character(len =  3) :: char3
    character(len =  6) :: char5
    character(len =  6) :: char6
    integer :: ifil, ilin, ipag, iunt, junt, ifnc, ityp, kunt, lunt
    integer :: ilen, ipos, ipag1, icll, ivk1, ivk2, fnum, icm1
!
    iunt = get_unit()
    open(iunt, file = 'Function_list1.txt')
    junt = get_unit()
    open(junt, file = 'Function_list2.txt')
    kunt = get_unit()
    open(kunt, file = 'Common_list1.txt')
    lunt = get_unit()
    open(lunt, file = 'Common_list2.txt')
!
    ifnc = 0
    cfunct => fnclst
    do
      if(.not. associated(cfunct)) exit
      ifnc = ifnc + 1
      fncname = cfunct%fncnam
      ifil    = cfunct%filnum
      ilin    = cfunct%linnum
      ipag    = cfunct%pagnum
      ityp    = cfunct%fnctyp
      
      if(fncname == '                            ') then
        cfunct => cfunct%next
        cycle
      endif
!
      write(twochar, *) ifil
      twochar = adjustl(twochar)
      onechar = trim(twochar)
      if(ifil < 10) onechar= '0'//onechar
      write(twochar, *) ipag
      twochar = adjustl(twochar)
      char3 = trim(twochar)
      char5 = onechar//'P'//char3
      write(twochar, *) ifnc
      twochar = adjustl(twochar)
      char6 = trim(twochar)
      char6 = '*'//char6
      write(iunt, '(a, x, a, 3x, a, i6, i5, a)') char6, fncname, char5, ilin, ityp, '*'
      write(junt, '(a, x, a, 3x, a, i6, i5, a)') char6, fncname, char5, ilin, ityp, '*'
      write(kunt, '(a, x, a, 3x, a, i6, i5, a)') char6, fncname, char5, ilin, ityp, '*'
!
!     output invoked functions' list - function list1
!
      ivk1 = 0
      cflink => cfunct%nink
      do
        if(.not. associated(cflink)) exit
        if(ivk1 == 0) write(iunt, '(5x, a)') '---------------------------------------'
        ivk1 = ivk1 + 1
        fncname = cflink%fncnam
        fnum    = cflink%fncnum
        ifil    = cflink%filnum
        ilin    = cflink%linnum
        ipag    = cflink%pagnum
!
        write(twochar, *) ifil
        twochar = adjustl(twochar)
        onechar = trim(twochar)
        if(ifil < 10) onechar= '0'//onechar
        write(twochar, *) ipag
        twochar = adjustl(twochar)
        char3 = trim(twochar)
        char5 = onechar//'P'//char3
        write(twochar, *) fnum
        twochar = adjustl(twochar)
        char6 = trim(twochar)
        char6 = '*'//char6
        write(iunt, '(2x, i6, x, a, a, x, a, i6)') ivk1, fncname, char6, char5, ilin
        cflink => cflink%next
      enddo
!
!     output invoked functions' list - function list2
!
      ivk2 = 0
      cflink => cfunct%nind
      do
        if(.not. associated(cflink)) exit
        if(ivk2 == 0) write(junt, '(5x, a)') '---------------------------------------'
        ivk2 = ivk2 + 1
        fncname = cflink%fncnam
        fnum    = cflink%fncnum
        ifil    = cflink%filnum
        ilin    = cflink%linnum
        ipag    = cflink%pagnum
!
        write(twochar, *) ifil
        twochar = adjustl(twochar)
        onechar = trim(twochar)
        if(ifil < 10) onechar= '0'//onechar
        write(twochar, *) ipag
        twochar = adjustl(twochar)
        char3 = trim(twochar)
        char5 = onechar//'P'//char3
        write(twochar, *) fnum
        twochar = adjustl(twochar)
        char6 = trim(twochar)
        char6 = '*'//char6
        write(junt, '(2x, i6, x, a, a, x, a, i6)') ivk2, fncname, char6, char5, ilin
        cflink => cflink%next
      enddo
!
!   output common list 1 and 2
!
      icm1 = 0
      cmlink => cfunct%ncmn
      do
        if(.not.associated(cmlink)) exit
        if(icm1 == 0) write(kunt, '(5x, a)') '---------------------------------------'
        icm1 = icm1 + 1
        cmnname = cmlink%cmnnam
        write(kunt, '(2x, i6, x, a)') icm1, trim(adjustr(cmnname))
!       if(.not.cmlink%prtflg) write(lunt, '(a)') trim(adjustl(cmnname)) 
        if(.not.cmlink%prtflg) write(lunt, '(a)') '/'//trim(adjustl(cmnname))//'/'
        cmlink =>cmlink%next
      enddo
      if(ivk1 > 0) write(iunt, '(5x, a)') '---------------------------------------'
      if(ivk2 > 0) write(junt, '(5x, a)') '---------------------------------------'
      if(icm1 > 0) write(kunt, '(5x, a)') '---------------------------------------'
      cfunct => cfunct%next
    enddo
!
    close(iunt)
    close(junt)
    close(kunt)
    close(lunt)

    return
  end subroutine output_f77

  function get_unit() result(fid)
    implicit none
    integer :: id, fid, iost
    logical :: opend

    fid = 0

    do id = 11, 99
      inquire(unit = id, iostat = iost, opened = opend)
      if(iost == 0 .and. (.not.opend)) then
        fid = id
        exit
      endif
    enddo

    return
  end function get_unit

  subroutine lwrcas(dest,sour)
    implicit none
    character*1 :: dest, sour
    integer :: iauc, izuc, idis, is, id

    iauc = 65
    izuc = 90
    idis = 32

    is = ichar(sour)

    if(is >= iauc .and. is <= izuc) then
      id = is + idis
      dest = char(id)
    else
      dest = sour
    endif

    return
  end subroutine lwrcas

end module f77mod