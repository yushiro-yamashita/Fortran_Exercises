program kadai
    implicit none
    integer(4), parameter :: r8 = 8
    integer(4) :: N, i
    real(8) :: dx, x, S, S_ref, e
    N = 10
    e = 1
    do while (e > 0.001_r8)
        dx = (1.0_r8)/N
        x = 0.0
        S = 0.0

        do i = 1, N, 1
            S = ((x*x + sin(x)) + ((x + dx)*(x + dx) + sin(x + dx)))*dx/2.0_r8 + S
            x = x + dx
        end do

        S_ref = 4.0_r8/3.0_r8 - cos(1.0_r8)
        e = abs(S - S_ref)
        print *, 'N = ', N
        print *, 'result = ', S
        print *, 'S = ', S_ref
        print *, 'e = ', e
        N = N+1

    end do
end program
