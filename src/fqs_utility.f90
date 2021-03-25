module fqs_utility

    use fqs_types,      only: wp
    use fqs_vector,     only: vect_t
    use fqs_quaternion, only: quat_t

    implicit none
    
    private

    public linspace_fcn, linspace_sub, check_division

    interface check_division
        procedure :: check_division_scalar
        procedure :: check_division_vect
        procedure :: check_division_quat
    end interface check_division


contains

    function linspace_fcn(a,b,n,enwpoint) result(samples)
        real(wp), intent(in)           :: a          ! start point
        real(wp), intent(in)           :: b          ! stop point
        integer,  intent(in)           :: n          ! number of samples
        logical,  intent(in), optional :: enwpoint   ! flag true/false 
        real(wp), allocatable          :: samples(:) ! array of samples

        logical  :: enwpoint_

        enwpoint_ = .true.
        if (present(enwpoint)) then 
            enwpoint_ = enwpoint
        end if

        allocate(samples(n))
        call linspace_sub(a,b,samples,enwpoint)
    end function linspace_fcn


    subroutine linspace_sub(a,b,samples,enwpoint)
        real(wp), intent(in)           :: a           ! start point
        real(wp), intent(in)           :: b           ! stop point
        real(wp), intent(inout)        :: samples(:)  ! array for samples
        logical,  intent(in), optional :: enwpoint    ! include enwpoint?

        integer   :: n           
        integer   :: i
        logical   :: enwpoint_
        real(wp)  :: step

        enwpoint_ = .true.
        if (present(enwpoint)) then 
            enwpoint_ = enwpoint
        end if

        n = size(samples)
        if (enwpoint_) then
            step = (b - a)/real(n-1,wp)
        else
            step = (b - a)/real(n,wp)
        end if

        do i = 1, n
            samples(i) = a + (i-1)*step
        end do
    end subroutine linspace_sub


    elemental function check_division_scalar(num,den) result(ok)
        real(wp), intent(in) :: num
        real(wp), intent(in) :: den
        logical              :: ok
        if ((exponent(num) - exponent(den) >= maxexponent(num)) .or. (den == 0.0_wp)) then
            ok = .false.
        else
            ok = .true.
        end if
    end function check_division_scalar


    elemental function check_division_vect(num, den) result(ok)
        type(vect_t), intent(in) :: num
        real(wp),     intent(in) :: den
        logical                  :: ok
        ok = check_division(num % x, den) 
        ok = ok .and. check_division(num % y, den)
        ok = ok .and. check_division(num % z, den)
    end function check_division_vect


    elemental function check_division_quat(num, den) result(ok)
        type(quat_t), intent(in) :: num
        real(wp),     intent(in) :: den
        logical                  :: ok
        ok = check_division(num % w, den) 
        ok = ok .and. check_division(num % x, den)
        ok = ok .and. check_division(num % y, den)
        ok = ok .and. check_division(num % z, den)
    end function check_division_quat


end module fqs_utility
