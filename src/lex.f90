module lex
    use utf8
    implicit none

    type :: token
        character(len=:), allocatable :: value
    end type

    interface
        function strlen(str) bind(C) result(len)
            use iso_c_binding, only: c_ptr, c_int
            type(c_ptr), value :: str
            integer(c_int) :: len
        end function
    end interface
contains
    function tokenize(line_ptr, toks, errno) result(err)
        use iso_c_binding, only: c_char, c_null_char, c_ptr, c_f_pointer, c_int, c_associated, c_int8_t
        implicit none

        integer :: err
        character(len=:), allocatable :: errno
        type(c_ptr), intent(in) :: line_ptr
        type(token), allocatable, intent(out) :: toks(:)
        character(len=:, kind=c_char), allocatable, target :: line
        integer(c_int) :: ilength

        err = 0
        allocate(toks(0))
        if(c_associated(line_ptr)) then
            ilength = strlen(line_ptr)
            block
                character(kind=c_char, len=ilength), pointer :: f_str
                call c_f_pointer(cptr=line_ptr, fptr=f_str)
                line = f_str
                f_str => null()
            end block
        else
            err = -1
            return
        end if
        
        block
            type(utf8_iter) :: iter
            character(kind=c_char, len=:), allocatable :: tok
            iter = iterator(line)
            do while(iter%has_next())
                tok = iter%next()
                select case(tok)
                    case default
                    err = -1
                    ! TODO: custom error type
                    errno = "lexer error: invalid token '" // tok // "'"
                    return
                end select
            end do
        end block
    end function

    subroutine reallocate(a, dl_new)
      type(token), dimension(:), allocatable, intent(inout) :: a
      type(token), dimension(:), allocatable :: temp
      integer, intent(in) :: dl_new
      integer :: dl_old
    
      dl_old = size(a)
      allocate(temp(dl_new))
      temp(1:dl_old) = a
      call move_alloc(temp, a)
  end subroutine reallocate
end module lex
