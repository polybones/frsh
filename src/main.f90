program main
  use iso_c_binding, only: c_int, c_null_char, c_ptr, c_f_pointer, c_size_t
  use lex
  use paths
  use proc
  use rl
  implicit none

  character(len=:), allocatable :: errno
  type(c_ptr) :: line_ptr
  character(len=:), allocatable :: cwd
  integer :: err
  integer :: i

  allocate(character(len=get_path_max()) :: cwd)
  do
    err = getcwd(cwd)
    if(err == -1) exit
    if(cwd(:6) == "/home/") then
      i = 6
      do
        if(cwd(i:i) == "/") exit
        i = i + 1
      end do
      i = i - 1
      cwd(i:i) = "~"
    end if

    line_ptr = readline(trim(cwd(i:)) // " > " // c_null_char)
    block
      type(token), allocatable :: toks(:)
      err = tokenize(line_ptr, toks, errno)
      if(err /= 0) print *, errno
    end block

    ! err = chdir("/bin" // c_null_char)
    ! if(err == -1) exit
  end do
  ! deallocate(cwd)
end program main
