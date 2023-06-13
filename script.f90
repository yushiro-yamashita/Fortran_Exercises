module constants
    implicit none
    integer(4), parameter  :: ikind=4, rkind=8
    real(rkind), parameter :: zero=0.0, one=1.0, half=0.5
end module constants

module integ
    use constants
    integer(ikind) :: n

    contains

    real(rkind) function f1(x)
        use constants, only : rkind
        implicit none
        real(rkind), intent(in) :: x
        f1 = x**2 + sin(x)
    end function f1

    real(rkind) function f2(x)
        use constants, only : rkind
        implicit none
        real(rkind), intent(in) :: x
        f2 = exp(-x**2)
    end function f2

    subroutine trapezoidal_integral(x_0, x_n, s, f)
        use constants, only : ikind, rkind, half
        implicit none
        real(rkind), intent(in)    :: x_0, x_n
        real(rkind), intent(out)   :: s
        real(rkind)    :: f
        integer(ikind) :: i
        real(rkind)    :: dx, x_sum

        dx = (x_n - x_0) / real(n, rkind)
        x_sum = half*(f(x_0) + f(x_n))
        do i = 1, n-1
            x_sum = x_sum + f(x_0 + i*dx)
        end do
        s = x_sum*dx
    end subroutine trapezoidal_integral
end module integ

program kadai
    use constants
    use integ
    implicit none
    integer(ikind) :: init_n=1, max_n=9999, n_print=100, n_
    real(rkind)    :: s1, s2, s_prev, diff1, diff2, diff_thresh=1.0e-12
    
    s_prev = zero
    do n = init_n, max_n
        call trapezoidal_integral(zero, one, s1, f1)
        diff1 = s1 - s_prev
        if (abs(diff1) < diff_thresh) exit
        if (mod(n, n_print)==0) then
            print*, "n = ", n
            print*, "s = ", s1
            print*, "diff = ", diff1
            print*
        endif
        s_prev = s1
    end do
    n_ = n

    s_prev = zero
    do n = init_n, max_n
        call trapezoidal_integral(zero, 5.0_rkind, s2, f2)
        diff2 = s2 - s_prev
        if (abs(diff2) < diff_thresh) exit
        if (mod(n, n_print)==0) then
            print*, "n = ", n
            print*, "s = ", s2
            print*, "diff = ", diff2
            print*
        endif
        s_prev = s2
    end do

    print "(a)", "Result for function_1 x^2+sin(x)"
    print "(a, i4)", "  Calculation converged at n = ", n_
    print "(SP, a, e16.9)", "  Integrated value = ", s1
    print "(SP, a, e16.9)", "  Difference from n-1 = ", diff1
    print*
    print "(a)", "Result for function_2 e^(-x^2)"
    print "(a, i4)", "  Calculation converged at n = ", n
    print "(SP, a, e16.9)", "  Integrated value = ", s2
    print "(SP, a, e16.9)", "  Difference from n-1 = ", diff2
end program kadai
