module rl
    interface
        function readline(prompt) bind(C) result(line_read)
            use iso_c_binding, only: c_char, c_ptr
            character(kind=c_char) :: prompt(*)
            type(c_ptr) :: line_read
        end function

        subroutine rl_on_new_line() bind(C)
        end subroutine

        subroutine rl_redisplay() bind(C)
        end subroutine

        subroutine rl_cleanup_after_signal() bind(C)
        end subroutine

        subroutine rl_replace_line(text, clear_undo) bind(C)
            use iso_c_binding, only: c_char, c_int
            character(kind=c_char) :: text(*)
            integer(c_int) :: clear_undo
        end subroutine
    end interface
end module
