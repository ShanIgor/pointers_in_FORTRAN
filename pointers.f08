Program pointers
  use args, only: initvariables, getarg, makerange, makesingle,&
  variables, theta13, theta12, delta
  Integer narg,cptArg,i
  character (len=100)::name, vartype
  integer varid
  integer idxes(3), overflow

  call initvariables()

  narg = command_argument_count()

  cptArg = 0
  do while (cptArg < narg)
     cptArg=cptArg+1
     call get_command_argument(cptArg,name)
     if ((len(name).ge.2).and.(name(1:2).eq."--")) then
        varid = getarg(trim(name(3:)), len_trim(name(3:)))
        cptArg=cptArg+1
        call get_command_argument(cptArg,vartype)
        select case (adjustl(vartype))
        case("range")
           cptArg = makerange(varid, cptArg)
        case default
           cptArg = makesingle(varid, cptArg)
        end select
     end if
  end do
  idxes = 1
  cidx=size(idxes)
  do i=1,size(variables)
     if (size(variables(i)%values).lt.1) then
        print*,"no values for ",variables(i)%name
        stop
     end if
     variables(i)%p = variables(i)%values(idxes(i))
  end do
  overflow=0
  do while (overflow.eq.0)
     do i=1,size(variables)
        variables(i)%p = variables(i)%values(idxes(i))
     end do
     print*, "th13 = ",theta13, "th12 = ", theta12, "delta = ",delta
     do i=size(idxes),1,-1
        if (idxes(i).lt.size(variables(i)%values)) then
           idxes(i)=idxes(i)+1
           overflow=0
           exit
        else
           idxes(i)=1
           overflow=1
        end if
     end do
  end do

End Program pointers
