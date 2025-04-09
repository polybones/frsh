module lex
    use iso_c_binding, only: c_char
    use utf8
    implicit none

    enum, bind(C)
        enumerator :: kind_string
    end enum

    type :: token
        character(kind=c_char, len=:), allocatable :: value
        integer :: type
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
        type(token), allocatable, intent(inout) :: toks(:)
        character(len=:, kind=c_char), allocatable, target :: line
        integer(c_int) :: ilength

        err = 0
        ! allocate(toks(0))
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
            errno = "c_associated err"
            return
        end if
        
        block
            type(utf8_iter) :: iter
            character(kind=c_char, len=:), allocatable :: tok

            iter = iterator(line)
            do while(iter%has_next())
                tok = iter%next(.false.)
                ! print *, "scan: " // tok
                select case(tok)
                    case('"')
                    block
                        character(kind=c_char, len=:), allocatable :: str
                        character(kind=c_char, len=:), allocatable :: t
                        integer :: i
                        str = ""
                        i = 1
                        t = iter%next(.true.)
                        do
                            if(.not. iter%has_next()) then
                                err = -1
                                errno = "unclosed string"
                                return
                            end if
                            t = iter%next(.true.)
                            if(t == '"') exit
                            str = str // t
                            i = i + 1
                        end do
                        call push_token(toks, str, kind_string)
                    end block
                    case(" ")
                    tok = iter%next(.true.)
                    continue
                    case default
                    block
                        character(kind=c_char, len=:), allocatable :: str
                        character(kind=c_char, len=:), allocatable :: t
                        integer :: i
                        str = ""
                        i = 1
                        do
                            if(.not. iter%has_next()) exit
                            t = iter%next(.false.)
                            if(t == " ") then
                                t = iter%next(.true.)
                                exit
                            end if
                            str = str // t
                            t = iter%next(.true.)
                            i = i + 1
                        end do
                        call push_token(toks, str, kind_string)
                    end block
                end select
            end do
        end block
    end function

    subroutine push_token(toks, value, type)
        use iso_c_binding, only: c_char
        type(token), allocatable, intent(inout) :: toks(:)
        character(kind=c_char, len=:), allocatable, intent(in) :: value
        integer, intent(in) :: type
        type(token) :: tk
        integer :: sz

        ! allocate space for new token
        call reallocate(toks, size(toks)+1)
        tk%value = value
        tk%type = type
        toks(size(toks)) = tk
    end subroutine

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
