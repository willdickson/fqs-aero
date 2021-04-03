module fqs_fake_kine

    use fqs_types,        only: wp
    use fqs_constants,    only: pi
    use fqs_euler_angle,  only: euler_t
    use fqs_utility,      only: deg2rad

    implicit none

    type, public :: fake_kine_param_t
        real(wp) :: phi_amplitude = 0.0_wp
        real(wp) :: alpha_amplitude = 0.0_wp
        real(wp) :: alpha_slope = 0.0_wp
        real(wp) :: alpha_overshoot_size = 0.0_wp
        real(wp) :: alpha_overshoot_power = 0.0_wp
        real(wp) :: theta_amplitude = 10.0_wp
        real(wp) :: theta_power = 0.0_wp
        real(wp) :: period = 0.0_wp
    end type fake_kine_param_t

    public fake_kine_period
    public fake_param_type_1
    public fake_param_type_2
    public fake_param_type_3
    public fake_param_type_4
    public fake_kine
    real(wp), parameter :: fake_kine_period  = 0.005_wp


contains

    subroutine fake_kine(t, euler, param)
        real(wp),                intent(in)            :: t(:)
        type(euler_t),           intent(out)           :: euler(:)
        type(fake_kine_param_t), intent(in), optional  :: param

        ! Local variables
        type(fake_kine_param_t) :: param_tmp
        real(wp)                :: cos_tmp(size(t))
        real(wp)                :: sin_tmp(size(t))
        real(wp)                :: attitude_erf(size(t))
        real(wp)                :: attitude_base(size(t))
        real(wp)                :: attitude_overshoot(size(t))

        if (present(param)) then
            param_tmp = param
        else
            param_tmp = fake_param_type_4()
        end if

        cos_tmp = cos( 2.0_wp*pi*t/(param_tmp % period) )
        sin_tmp = sin( 2.0_wp*pi*t/(param_tmp % period) )

        attitude_base = (param_tmp % alpha_amplitude) *  erf( (param_tmp % alpha_slope) * sin_tmp ) 
        attitude_overshoot = sign(1.0_wp,cos_tmp)*(abs(cos_tmp) ** (param_tmp % alpha_overshoot_power))
        attitude_overshoot = (param_tmp % alpha_overshoot_size)*attitude_overshoot

        euler % heading  = (param_tmp % phi_amplitude) * cos_tmp
        euler % attitude = attitude_base  + attitude_overshoot 
        euler % bank = (param_tmp % theta_amplitude) * abs( cos_tmp ) ** (param_tmp % theta_power )    

    end subroutine fake_kine


    function fake_param_type_1() result(param)
        type(fake_kine_param_t) :: param
        param % phi_amplitude = deg2rad(70.0_wp)
        param % alpha_amplitude = deg2rad(50.0_wp)
        param % alpha_slope = 3.0_wp
        param % alpha_overshoot_size = deg2rad(0.0_wp)
        param % alpha_overshoot_power = 12.0_wp
        param % theta_amplitude = deg2rad(0.0_wp)
        param % theta_power = 10.0_wp
        param % period  = fake_kine_period 
    end function fake_param_type_1


    function fake_param_type_2() result(param)
        type(fake_kine_param_t) :: param
        param % phi_amplitude = deg2rad(70.0_wp)
        param % alpha_amplitude = deg2rad(50.0_wp)
        param % alpha_slope = 3.0_wp
        param % alpha_overshoot_size = deg2rad(0.0_wp)
        param % alpha_overshoot_power = 12.0_wp
        param % theta_amplitude = deg2rad(10.0_wp)
        param % theta_power = 10.0_wp
        param % period  = fake_kine_period 
    end function fake_param_type_2


    function fake_param_type_3() result(param)
        type(fake_kine_param_t) :: param
        param % phi_amplitude = deg2rad(70.0_wp)
        param % alpha_amplitude = deg2rad(50.0_wp)
        param % alpha_slope = 3.0_wp
        param % alpha_overshoot_size = deg2rad(30.0_wp)
        param % alpha_overshoot_power = 12.0_wp
        param % theta_amplitude = deg2rad(0.0_wp)
        param % theta_power = 10.0_wp
        param % period  = fake_kine_period 
    end function fake_param_type_3

    
    function fake_param_type_4() result(param)
        type(fake_kine_param_t) :: param
        param % phi_amplitude = deg2rad(70.0_wp)
        param % alpha_amplitude = deg2rad(50.0_wp)
        param % alpha_slope = 3.0_wp
        param % alpha_overshoot_size = deg2rad(30.0_wp)
        param % alpha_overshoot_power = 12.0_wp
        param % theta_amplitude = deg2rad(10.0_wp)
        param % theta_power = 10.0_wp
        param % period  = fake_kine_period 
    end function fake_param_type_4

end module fqs_fake_kine
