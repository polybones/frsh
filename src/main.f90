program main
  use iso_c_binding, only: c_int, c_null_char, c_ptr, c_f_pointer, c_size_t, c_funloc
  use lex
  use paths
  use proc
  use rl
  use sig
  implicit none

  character(len=:), allocatable :: errno
  type(c_ptr) :: line_ptr
  character(len=:), allocatable :: cwd
  integer :: err
  integer :: i
  procedure(sighandler), pointer :: handler
  type(c_ptr) :: old_handler
  logical :: balls

  allocate(character(len=get_path_max()) :: cwd)
  handler => handle_signals
  old_handler = c_signal(SIGINT, c_funloc(handler))
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
    if(balls .eqv. .true.) then
      balls = .false.
      exit
    end if
    block
      type(token), allocatable :: toks(:)
      err = tokenize(line_ptr, toks, errno)
      if(err /= 0) print *, errno
    end block

    ! err = chdir("/bin" // c_null_char)
    ! if(err == -1) exit
  end do
contains
   subroutine handle_signals(sig) bind(C)
     use iso_c_binding, only: c_int, c_null_char
     integer(c_int), value :: sig

     print *
     call rl_replace_line(c_null_char, 0)
     call rl_on_new_line()
     call rl_redisplay()
   end subroutine
end program main
