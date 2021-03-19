module fqs_utility

    use fqs_types, only: wp

    implicit none
    
    private

    public linspace_fcn, linspace_sub

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

end module fqs_utility
