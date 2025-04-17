Module numerics
! Déclaration de variables à portée globale
      IMPLICIT NONE 
            INTEGER, PARAMETER :: Nt = 1200                                              ! Nombre d'échantillons en temps
            INTEGER, PARAMETER :: Nx = 500                                             ! Nombre d'échantillons en espace
            REAL(8), PARAMETER :: epsilon_0 = 8.854187817e-12                           ! Permittivité du vide
            REAL(8), PARAMETER :: mu_0 = 1.256637061e-7                                 ! Perméabilité du vide
            REAL(8), PARAMETER :: PI = atan(1.0d0) * 4.0d0                              ! Constante pi

            ! Déclaration des variables
            REAL(8), PARAMETER :: fmax     = 1.0d9
            REAL(8), PARAMETER :: c        = 1.0d0 / sqrt(epsilon_0 * mu_0)
            REAL(8), PARAMETER :: attfmax  = 10.0d0
            REAL(8), PARAMETER :: att0     = 1000.0d0
            REAL(8), PARAMETER :: a0       = 1.0d0
            REAL(8), PARAMETER :: T        = sqrt(log(attfmax)) / (PI * fmax)
            REAL(8), PARAMETER :: t0       = T * sqrt(log(att0))
            REAL(8), PARAMETER :: dx       = (c / fmax) / 30.0d0
            REAL(8), PARAMETER :: dt       = 0.98d0 * dx / c
            INTEGER, PARAMETER :: LBD      = 0
            INTEGER, PARAMETER :: UBD      = Nt - 1
            INTEGER, PARAMETER :: idfile   = 10


            ! Déclaration des matrices et vecteurs
            REAL(8), ALLOCATABLE, DIMENSION(:) :: Esrc, base_t


      CONTAINS

            subroutine init_vectors()
                  ! Allocation des vecteurs
                  ALLOCATE(Esrc(LBD:UBD), base_t(LBD:UBD))
            end subroutine init_vectors

end module numerics