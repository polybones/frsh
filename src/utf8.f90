module utf8
    use iso_c_binding, only: c_char
    implicit none
    
    integer, dimension(*), parameter :: utf8_seq = [ &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
        0, 0, &
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
        4, 4, 4, 4, 4, &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
    ]
    private :: utf8_seq

    type :: utf8_iter
        private
        character(len=:, kind=c_char), pointer :: str_ptr => null()
        integer :: cur = 1
    contains
        procedure :: has_next => iter_has_next
        procedure :: next => iter_next
    end type
contains
    function iterator(str) result(iter)
        character(len=:, kind=c_char), allocatable, target, intent(in) :: str
        type(utf8_iter) :: iter
        iter%str_ptr => str
    end function

    pure function iter_has_next(self) result(res)
        class(utf8_iter), intent(in) :: self
        logical :: res
        res = self%cur <= len(self%str_ptr)
    end function

    function iter_next(self) result(res)
        use iso_c_binding, only: c_int8_t
        class(utf8_iter), intent(inout) :: self
        character(len=:, kind=c_char), allocatable :: res
        integer(kind=c_int8_t) :: f_byte
        integer :: byte_len

        f_byte = transfer(self%str_ptr(self%cur:self%cur), f_byte)
        byte_len = utf8_seq(iand(int(f_byte, 4), int(z'000000FF', 4)))
        if(byte_len == 0) byte_len = 1
        res = self%str_ptr(self%cur:self%cur + byte_len - 1)
        self%cur = self%cur + byte_len
    end function
end module utf8
