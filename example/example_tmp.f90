program example_tmp

    use fqs,  only: wp
    use fqs,  only: pi
    use fqs,  only: euler_t
    use fqs,  only: rad2deg
    use fqs,  only: linspace_sub
    use fqs,  only: fake_kine
    use fqs,  only: fake_kine_period 
    use fqs,  only: fake_kine_param_t
    use fqs,  only: fake_param_type_1
    use fqs,  only: fake_param_type_2
    use fqs,  only: fake_param_type_3
    use fqs,  only: fake_param_type_4
    use pyplot_module, only: pyplot

    implicit none

    integer, parameter      :: num_pts = 500
    type(euler_t)           :: euler(num_pts)
    real(wp)                :: phi(num_pts)
    real(wp)                :: alpha(num_pts)
    real(wp)                :: theta(num_pts)
    real(wp)                :: t(num_pts)
    real(wp)                :: t0 
    real(wp)                :: tn 
    integer                 :: i
    type(pyplot)            :: plt
    type(fake_kine_param_t) :: param


    param = fake_param_type_4()
    t0 = 0.0_wp
    tn = 1.0_wp*param % period

    call linspace_sub(t0, tn, t)
    call fake_kine(t, euler, param=param)
    phi = rad2deg(euler % heading)
    alpha = rad2deg(euler % attitude)
    theta = rad2deg(euler % bank)

    call plt % initialize(grid=.true.,xlabel='t (sec)',ylabel='(deg)')
    call plt % add_plot(t,phi,label='phi',linestyle='b',markersize=5,linewidth=2)
    call plt % add_plot(t,alpha,label='alpha',linestyle='g',markersize=5,linewidth=2)
    call plt % add_plot(t,theta,label='alpha',linestyle='r',markersize=5,linewidth=2)
    call plt % showfig()


contains




end program example_tmp
