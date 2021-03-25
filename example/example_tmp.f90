program example_tmp

    use fqs,  only: wp
    use fqs,  only: vect_t
    use fqs,  only: quat_t
    use fqs,  only: check_division


    implicit none

    real(wp)                  :: x
    real(wp)                  :: y
    type(vect_t)              :: v
    type(quat_t)              :: q
    logical                   :: ok 
    real(wp),     allocatable :: a(:)
    type(vect_t), allocatable :: va(:)
    type(quat_t), allocatable :: qa(:)
    logical,      allocatable :: ok_a(:)
    integer,      parameter   :: n = 10

    allocate(a(n))
    allocate(ok_a(n))
    allocate(va(n))
    allocate(qa(n))

    x = 10000.0_wp
    y = epsilon(0.0_wp)
    print *, maxexponent(y)
    print *, exponent(x)
    print *, exponent(1.0/y)

    v = vect_t(1.0_wp, 2.0_wp, 3.0_wp)
    ok = check_division(v,y)
    print *, ok

    q = quat_t(1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp)
    ok = check_division(q,y)
    print *, ok

    ok_a = check_division(a,y)
    print *, ok_a

    va = vect_t(1.0_wp, 2.0_wp, 3.0_wp)
    ok_a = check_division(a,y)
    print *, ok_a

end program example_tmp
