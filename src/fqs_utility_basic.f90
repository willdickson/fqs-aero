module fqs_utility_basic

    use fqs_types,      only: wp

    implicit none
    
    private

    public check_division_scalar

contains

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

end module fqs_utility_basic
