module constants
    implicit none
    integer(4), parameter  :: ikind=4, rkind=8
    real(rkind), parameter :: zero=0.0, one=1.0, half=0.5, c1p5=1.5, two=2.0
end module constants

program kadai
    use constants
    implicit none
    integer(ikind) :: n_div=10, n_elem, i, info, unit
    integer(ikind), allocatable :: ipiv(:)
    real(rkind)    :: a, b, c, dx
    real(rkind)    :: xb0, xb1, yb0, yb1 ! boundary conditions
    real(rkind), allocatable :: mat(:,:), rhs_vec(:), y_vec(:), x_vec(:)

    xb0 = 0
    xb1 = 1
    yb0 = 1
    yb1 = 1

    dx = (xb1 - xb0)/real(n_div, rkind)

    ! y'' + 3y' + 2y = 0
    ! d^2y/dx^2|i = 1/(dx^2)*(y_i+1 - 2y_i + y_i-1)
    ! dy/dx|i = 1/(2dx)*(y_i+1 - y_i-1)
    ! y|i = y_i
    ! (y'' + 3y' + 2y)_i = (1/dx - 3/2)/dx y_i-1 + (-2/dx^2 + 2) y_i + (1/dx + 3/2)/dx y_i+1
    a = (one/dx - c1p5)/dx
    b = -two/dx**2 + two
    c = (one/dx + c1p5)/dx

    n_elem = n_div - 1
    allocate(rhs_vec(n_elem), mat(n_elem, n_elem), ipiv(n_elem))
    rhs_vec(:) = zero
    rhs_vec(1) = -a*yb0
    rhs_vec(n_elem) = -c*yb1

    mat(:,:) = zero
    do i = 1, n_elem
        if (i/=1) mat(i, i-1) = a
        mat(i, i) = b
        if (i/=n_elem) mat(i, i+1) = c
    end do
    allocate(y_vec(0:n_div))
    y_vec(1:n_elem) = rhs_vec(1:n_elem)
    call dgesv(n_elem, 1, mat, n_elem, ipiv, y_vec(1:n_elem), n_elem, info)
    y_vec(0) = yb0
    y_vec(n_div) = yb1

    allocate(x_vec, mold=y_vec)
    x_vec(:) = [(i*dx, i=0, n_div)]

    open(newunit=unit, file="result.csv", form="formatted")
    write(unit, "(a)") "x,y"
    do i = 0, n_div
        write(unit, "(SP, ES16.9, ',', ES16.9)") x_vec(i), y_vec(i)
    end do
    close(unit)

    print*, "N = ", n_div
    print*, "y = ", y_vec(:)

end program kadai
