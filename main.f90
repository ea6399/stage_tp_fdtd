PROGRAM FDTD_1D
    use structure
    use source

    IMPLICIT NONE
    ! Déclaration des variables
    REAL(8) :: fmax, c
    REAL(8) :: t0, T
    REAL(8) :: dt, dx
    ! Classe FDTD1D
    type(FDTD1D) :: fd

    !Initialisation des variables

    fmax = 1.0e9                                        ! Fréquence max d'étude
    c = 1.d0 / sqrt(epsilon_0 * mu_0)                   ! Vitesse de la lumière

    ! Calcul des pas spatio-temporels
    dx = (c / fmax) / 30                                ! 30 cellules pour la longueur d'onde min
    dt = 0.98d0 * dx / c                                ! Critère de stabilité dt <= dx/c
    
    ! Calcul de la fonction d'excitation : une gaussienne
    T = sqrt( log(10.0d0) ) / (PI * fmax)               !
    t0 = T * sqrt(log(1000.0d0))                        ! Retards





    !----------------------------------------!
    ! 1D FDTD simulation of a Gaussian pulse !
    !----------------------------------------!

    ! Initialisation des résultats
    call resultat_init(fd, Nx, Nt)
    
    ! Initialisation des champs
    call init(fd, Nx, dt, dx)

    

    

    


END PROGRAM FDTD_1D