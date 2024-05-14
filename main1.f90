Program Satellite_velocity
    
    implicit none
    
    call calculation
    
    write(*,*) "                       "
    write(*,*) "Calculations Completed."
    
    end Program Satellite_velocity
    
    
    subroutine calculation
    
    Implicit none
    
    ! constants
    real, parameter     :: g          = 9.8         ! gravitational acceleration at the earth's surface in m/s^2
    real, parameter     :: Re         = 6.378e6     ! the radius of the earth in meters
    real, parameter     :: pi         = 3.145       ! pi
    real, parameter     :: Fe         = 1.1574e-5   ! frequency
    
    ! variables
    integer, parameter  :: max_height = 40000000    ! max height above the earth's surface in meters
    integer, parameter  :: increment  = 2000000     ! increment in meters
    integer             :: h                        ! height in meters of the satellite's orbit measured from earth's surface
    real                :: velocity_abs             ! absolute velocity
    real                :: velocity_app             ! apparent velocity
    real                :: timep                    ! time period
    real                :: timep_app                ! apparent time period
    real                :: freq                     ! frequency
    real                :: freq_app                 ! apparent frequency
    real                :: Re_g                     ! Re divided by g
    real                :: h_Re                     ! h divided by Re
    real                :: g_Re                     ! g divided by Re
    real                :: half                     ! half
    real                :: energy                   ! energy
    real                :: power                    ! the power 3/2
    
    ! Printing Headers
    print*, " Height               Velocity (m/s)            Apparent       Energy"
    print*, "  (km)           Absolute   |   Apparent       Period(hrs)     (kj/MJ)"
    print*, "----------------------------------------------------------------------------"
    
    ! starting the do loop
    do h = 0, max_height, increment
        
        ! calculating time period
        Re_g            = Re/g
        h_Re            = h/Re
        power           = 1.5
        timep           = 2 * pi * sqrt( Re_g ) * ((1 + h_Re) ** (power)) 
        
        ! calculating frequency
        freq            = 1/timep
        
        ! calculating apparent frequency
        g_Re            = g/Re
        freq_app        = ((1/(2 * pi)) * sqrt( g_Re ) * ((1 + h_Re) ** (-power) )) - Fe
        
        ! calculating apparent time period
        timep_app       = 1/freq_app
        timep_app       = timep_app/3600
        
        ! calculating absolute velocity
        velocity_abs    = 2 * pi * (Re + h) * freq
        
        ! calculating apparent velocity
        velocity_app    = 2 * pi * (Re + h) * freq_app
        
        ! calculating energy
        half            = 0.5
        energy          = g * Re * (0.9966 - half * (1 + h_Re) ** -1)
        energy          = energy / 1e6
        
        ! printing the results per line
        write (*,*) (real(h)/1000), velocity_abs, velocity_app, timep_app, energy
    
    ! end do loop    
    end do
    
    
end subroutine
    