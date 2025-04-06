module proc
    interface
        function fork() bind(C) result(pid)
            use iso_c_binding, only: c_int
            integer(c_int) :: pid
        end function fork
    end interface
end module proc
