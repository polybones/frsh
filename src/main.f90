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

    line_ptr = readline("\x1B[34m" // trim(cwd(i:)) // " >\x1B[0m " // c_null_char)
    block
      type(token), allocatable :: toks(:)
      integer :: j
      allocate(toks(0))
      err = tokenize(line_ptr, toks, errno)
      if(err /= 0) print *, "lexer error: " // errno
      do j = 1, size(toks)
        print *, toks(j)%value
      end do
    end block
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
