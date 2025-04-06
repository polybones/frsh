module paths
    use iso_c_binding, only: c_char
    implicit none
    
    type :: path_table
        character(kind=c_char, len=:), allocatable :: entries(:)
    end type path_table

    interface
        function getcwd_helper(buf, size) bind(C)
            use iso_c_binding, only: c_int, c_char
            integer(kind=c_int) :: getcwd_helper
            character(kind=c_char), intent(out) :: buf(*)
            integer(kind=c_int), value :: size
        end function getcwd_helper

        function get_path_max() bind(C)
            use iso_c_binding, only: c_int
            integer(kind=c_int) :: get_path_max
        end function get_path_max

        function chdir(path) bind(C)
            use iso_c_binding, only: c_int, c_char
            integer(kind=c_int) :: chdir
            character(kind=c_char) :: path(*)
        end function chdir
    end interface
contains
    function getcwd(buf)
        use iso_c_binding, only: c_char, c_null_char, c_int
        integer :: getcwd
        character(*), intent(out) :: buf
        integer :: i, size
        character(len=len(buf), kind=c_char) :: buf_copy

        getcwd = getcwd_helper(buf_copy, len(buf_copy, kind=c_int))
        if(getcwd /= 0) then
            buf = ''
            getcwd = -1
            return
        end if

        size = index(buf_copy, c_null_char) - 1
        do i = 1, size
            buf(i:i) = char(ichar(buf_copy(i:i)))
        end do
        buf(size+1:) = ''
    end function getcwd
end module paths
