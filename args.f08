module args
  type parg
     character(len=100) :: name
     double precision, pointer :: p
     double precision, allocatable :: values(:)
  end type parg
  double precision, target :: theta13, theta12, delta
  type(parg) variables(3)

contains

  subroutine initvariables()
    variables(1)%name = "theta13"
    variables(1)%p => theta13

    variables(2)%name = "theta12"
    variables(2)%p => theta12

    variables(3)%name = "delta"
    variables(3)%p => delta
  end subroutine initvariables

  function getarg(name, namelen) result(argid)
    character(len=namelen), intent(in) :: name
    integer :: argid
    integer i
    argid=-1
    do i=1,size(variables)
       if (name == trim(variables(i)%name)) then
          argid=i
          exit
       end if
    end do
    if (argid < 0) then
       write(*,*)"unknown name ", name
       stop
    end if
  end function getarg

  function makerange(varid, cptArg) result(res)
    integer, intent(in) :: varid, cptArg
    integer :: res
    character (len=100) smin, smax, scount
    double precision min, max, step
    integer i, count
    Call get_command_argument(cptArg+1,smin)
    read(smin,*) min
    Call get_command_argument(cptArg+2,smax)
    read(smax,*) max
    Call get_command_argument(cptArg+3,scount)
    read(scount,*) count
    res = cptArg + 3
    allocate(variables(varid)%values(count))
    step=(max-min)/(count-1)
    do i = 1,count
       variables(varid)%values(i) = min + (i-1)*step
    end do
  end function makerange

  function makesingle(varid, cptArg) result(res)
    integer, intent(in) :: varid, cptArg
    integer :: res
    character (len=100) sval
    Call get_command_argument(cptArg,sval)
    res=cptArg
    allocate(variables(varid)%values(1))
    read(sval,*) variables(varid)%values(1)
  end function makesingle
end module args
