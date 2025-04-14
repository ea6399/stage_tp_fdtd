PROGRAM FDTD_1D
    use structure
    use source

    IMPLICIT NONE
    ! Déclaration des variables
  
    ! Classe FDTD1D
    type(FDTD1D) :: fd



    !----------------------------------------!
    ! 1D FDTD simulation of a Gaussian pulse !
    !----------------------------------------!

    ! Initialisation des résultats
    call resultat_init(fd, Nx, Nt)
    WRITE(*,'(/,T5,A,/)') "Initialisation des résultats terminée"

    

    


END PROGRAM FDTD_1D