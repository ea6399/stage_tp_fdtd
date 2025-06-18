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
            
end module source