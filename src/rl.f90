module rl
    interface
        function readline(prompt) bind(C) result(line_read)
            use iso_c_binding, only: c_char, c_ptr
            character(kind=c_char) :: prompt(*)
            type(c_ptr) :: line_read
        end function readline
    end interface
end module rl
