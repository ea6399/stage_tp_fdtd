module structure
    implicit none

    ! Déclaration de variables
    INTEGER, PARAMETER :: Nt = 1200                                             ! Nombre d'échantillons en temps
    INTEGER, PARAMETER :: Nx = 500                                              ! Nombre d'échantillons en espace
    REAL(8), PARAMETER :: epsilon_0 = 8.854187817e-12                           ! Permittivité du vide
    REAL(8), PARAMETER :: mu_0 = 1.256637061e-7                                 ! Perméabilité du vide
    REAL(8), PARAMETER :: PI = atan(1.0d0) * 4.0d0                              ! Constante pi



    ! Type FDTD1D 
    type :: FDTD1D
        ! FDTD variables
        REAL(8), ALLOCATABLE :: E(:), H(:)                                          ! Champs E et H
        REAL(8), ALLOCATABLE :: c_E(:), c_H(:)                                      ! Coefficient E et H
        INTEGER :: Nres                                                             ! Nombre de résultats
        INTEGER, ALLOCATABLE :: pres(:)                                                             ! Position du point d'observation pour chaque résultat
        REAL(8), ALLOCATABLE :: Eres(:,:), Hres(:,:)                                ! Tableau 2D hébergeant les résultats E et H
    CONTAINS    
        procedure :: resultat_init
        ! procedure :: init 
        ! procedure :: calcule
        ! procedure :: resultat_stockage
    end type FDTD1D


    CONTAINS


    SUBROUTINE resultat_init(fd, Nx, Nt)
        ! Initialisation des arguments
        class(FDTD1D), intent(inout) :: fd
        INTEGER, intent(in) :: Nx, Nt
        
        ! Initialisation des variables locales
        INTEGER :: i, n

        fd%Nres = 4 ! Nombre de résultats

        ALLOCATE(fd%pres(fd%Nres)) ! Allocation de la mémoire pour le tableau de positions
        ALLOCATE(fd%Eres(Nt,fd%Nres)) ! Allocation de la mémoire pour le tableau de résultats E, champs éléctrique
        ALLOCATE(fd%Hres(Nt,fd%Nres)) ! Allocation de la mémoire pour le tableau de résultats H, champs magnétique

        fd%pres(1) = 1
        fd%pres(2) = 100
        fd%pres(3) = 200
        fd%pres(4) = Nx

        


        fd%E = 0.0d0
        fd%H = 0.0d0

        fd%c_E = 1.0d0 / (epsilon_0 * fd%c_H)
        fd%c_H = 1.0d0 / (mu_0 * fd%c_E)

        fd%Nres = 10
        fd%pres = 100


    END SUBROUTINE resultat_init  





endmodule structure