module source
    
    implicit none
        ! Calcul de la fonction d'excitation : une gaussienne
        REAL(8) :: attfmax = 10.d0
        REAL(8) :: att0 = 1000.d0
        REAL(8) :: T = sqrt( log(attfmax) ) / (PI * fmax)               ! Largeur de l'impulsion à mi-hauteur
        REAL(8) :: t0 = T * sqrt(log(att0))                             ! Retards
        REAL(8) :: a0 = 1.d0

  


    CONTAINS
        ! Fonction gaussienne
        FUNCTION gauss_t(n,dt)
            IMPLICIT NONE
            ! Retour
            REAL(8) :: gauss_t

            ! Arguments
            INTEGER, intent(in) :: n


            gauss_t = a0 * exp( - ( ( n * dt - t0 ) / T )**2 )
      ENDFUNCTION gauss_t

      SUBROUTINE compute_gauss(et,base_t, Nt)
            IMPLICIT NONE
            ! Arguments
            REAL(8), ALLOCATABLE, intent(in) :: base_t(:)
            REAL(8), ALLOCATABLE, intent(out) :: et(:)
            INTEGER, intent(in) :: Nt

            ! Variables locales
            INTEGER :: n


            ! Intervalles de définition de la gaussienne temporelle
            DO n = Lbound(base_t), Ubound(base_t)
                base_t(n) = n * dt
            end do

            ! Calcul de la gaussienne
            DO n = LBOUND(base_t), UBOUND(base_t)
                et(n) = gauss_t(n, base_t(n))
            END DO
      ENDSUBROUTINE compute_gauss


    

end module source