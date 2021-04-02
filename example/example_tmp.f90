program example_tmp

    use fqs,  only: wp
    use fqs,  only: quat_t
    use fqs,  only: euler_t
    use fqs,  only: axis_angle_t
    use fqs,  only: axis_angle_from_euler
    use fqs,  only: pi

    implicit none

    type(euler_t)      :: euler
    type(axis_angle_t) :: axis_angle

    euler % heading = 0.0_wp*pi
    euler % attitude = 0.0_wp*pi
    euler % bank = 0.5_wp*pi
    axis_angle = axis_angle_from_euler(euler)

    print *, euler
    print *, axis_angle


end program example_tmp
