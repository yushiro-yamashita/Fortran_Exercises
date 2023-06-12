module constants
    implicit none
    integer(4), parameter  :: ikind=4, rkind=8
    real(rkind), parameter :: zero=0.0, one=1.0, half=0.5
end module constants

real(rkind) function f(x)
    use constants, only : rkind
    implicit none
    real(rkind) :: x
    f = x**2 + sin(x)
end function f

program kadai
    use constants
    implicit none
    integer(ikind) :: i, n, init_n=1, max_n=9999, n_print=100
    real(rkind)    :: dx, x_sum, s, s_prev, diff, diff_thresh=1.0e-12, x_0=zero, x_n=one
    real(rkind)    :: f

    s_prev = zero
    do n = init_n, max_n
        dx = (x_n - x_0) / real(n, rkind)
        x_sum = zero
        do i = 0, n
            x_sum = x_sum + f(x_0 + i*dx)
        end do
        x_sum = x_sum  - half*(f(x_0) + f(x_n))
        s = x_sum*dx
        diff = s - s_prev
        if (mod(n, n_print)==0) then
            print*, "n = ", n
            print*, "s = ", s
            print*, "diff = ", diff
            print*
        endif
        if (abs(diff) < diff_thresh) exit
        s_prev = s
    end do
    print "(a, i4)", "Calculation converged at n = ", n
    print "(SP, a, e16.9)", "Integrated value = ", s
    print "(SP, a, e16.9)", "Difference from n-1 = ", diff
end program kadai
