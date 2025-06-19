module source
      use numerics

      CONTAINS

            ! Fonction gaussienne
            FUNCTION gauss_t(n,dt)
                  IMPLICIT NONE
                  ! REour
                  REAL(8) :: gauss_t, dt

                  ! Arguments
                  INTEGER, intent(in) :: n

                  gauss_t = a0 * exp( - ( ( n * dt - t0 ) / T )**2 )
            ENDFUNCTION gauss_t

            REAL(8) FUNCTION sinusoidal_wave(n,dt)

                  IMPLICIT NONE
                  ! Arguments
                  REAL(8), intent(in) :: dt
                  INTEGER, intent(in) :: n

                  ! Variables locales
                  REAL(8) :: omega0

                  omega0 = 2.0d0 * PI * fmax

                  sinusoidal_wave = sin(omega0 * n * dt)

            END FUNCTION

            SUBROUTINE compute_gauss(E,base, Nt)
                  IMPLICIT NONE
                  ! Arguments
                  REAL(8), intent(inout) :: base(LBD:UBD)
                  REAL(8), intent(inout) :: E(LBD:UBD)
                  INTEGER, intent(in) :: Nt

                  ! Variables locales
                  INTEGER :: n


                  print *,"Lower bound / Upper bound / Nt :", LBD, UBD, Nt

                  ! Intervalles de définition de la gaussienne temporelle
                  DO n = LBD, UBD
                        base(n) = n * dt
                  end do

                  ! Calcul de la gaussienne
                  DO n = LBD, UBD
                        E(n) = gauss_t(n, dt)
                  END DO
            ENDSUBROUTINE compute_gauss

            SUBROUTINE sinusoidal_source(E,base_t, Nt)

                  IMPLICIT NONE
                  ! Arguments
                  REAL(8), intent(inout) :: E(0:Nt - 1)
                  REAL(8), intent(inout) :: base_t(0:Nt - 1)
                  INTEGER, intent(in) :: Nt

                  ! Variables locales
                  INTEGER :: n

                  DO n = 0, Nt - 1
                        base_t(n) = n * dt
                  end do

                  ! Calcul de la source sinusoïdale
                  DO n = 0, Nt -1 
                        ! Calcul de la source sinusoïdale
                        E(n) = sinusoidal_wave(n, dt)
                  END DO

            END SUBROUTINE sinusoidal_source

end module source