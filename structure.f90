module structure
    implicit none

    ! Déclaration de variables
    INTEGER, PARAMETER :: Nt = 1200                                             ! Nombre d'échantillons en temps
    INTEGER, PARAMETER :: Nx = 500                                              ! Nombre d'échantillons en espace
    REAL(8), PARAMETER :: epsilon_0 = 8.854187817e-12                           ! Permittivité du vide
    REAL(8), PARAMETER :: mu_0 = 1.256637061e-7                                 ! Perméabilité du vide
    REAL(8), PARAMETER :: PI = atan(1.0d0) * 4.0d0                              ! Constante pi



    ! FDTD variables
    REAL(8), ALLOCATABLE :: E(:), H(:)                                          ! Champs E et H
    REAL(8), ALLOCATABLE :: c_E(:), c_H(:)                                      ! Coefficient E et H
    INTEGER :: Nres                                                             ! Nombre de résultats
    INTEGER :: pres                                                             ! Position du point d'observation pour chaque résultat
    REAL(8), ALLOCATABLE :: Eres(:,:), Hres(:,:)                                ! Tableau 2D hébergeant les résultats E et H




    CONTAINS


    SUBROUTINE resultat_init(Nx,dt,dx)
        ! Initialisation des variables
        INTEGER :: Nx
        REAL(8) :: dt, dx

        ALLOCATE( E(Nx + 1), H(Nx + 1), c_E(Nx + 1), c_H(Nx + 1) )

        Nres = 4

        E = 0.0d0
        H = 0.0d0

        c_E = 1.0d0 / (epsilon_0 * c_H)
        c_H = 1.0d0 / (mu_0 * c_E)

        Nres = 10
        pres = 100


    END SUBROUTINE resultat_init  



endmodule structure