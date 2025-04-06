module sig
    use iso_c_binding, only: c_int, c_ptr
    implicit none

    integer(c_int), parameter :: SIGINT = 2

    type(c_ptr), public :: sighandler_t

    interface
        subroutine sighandler(sig) bind(C)
            import :: c_int
            integer(c_int), value :: sig
        end subroutine

        function c_signal(sig, handler) bind(C, name="signal")
            import :: c_int, c_ptr
            integer(c_int), value :: sig
            type(c_ptr), value :: handler
            type(c_ptr) :: c_signal
        end function
    end interface
end module
