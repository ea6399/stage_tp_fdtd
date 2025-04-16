module structure

     use numerics


     implicit none

     ! Type FDTD1D 
     type :: FDTD1D
          ! FDTD variables
          REAL(8), ALLOCATABLE :: E(:), H(:)                                          ! Champs E et H
          REAL(8), ALLOCATABLE :: c_E(:), c_H(:)                                      ! Coefficient E et H
          INTEGER :: Nres                                                             ! Nombre de résultats
          INTEGER, ALLOCATABLE :: pres(:)                                             ! Position du point d'observation pour chaque résultat
          REAL(8), ALLOCATABLE :: Eres(:,:), Hres(:,:)                                ! Tableau 2D hébergeant les résultats E et H
     CONTAINS    
          procedure :: resultat_init
          procedure :: init 
          procedure :: calcule
          procedure :: resultat_stockage
          procedure :: freememory
     end type FDTD1D


    CONTAINS



                    !----------------------------------------!
                    ! Definition des points d'observation    !
                    !----------------------------------------!
     SUBROUTINE resultat_init(fd, Nx, Nt)
          ! Initialisation des arguments
          IMPLICIT NONE
          class(FDTD1D), intent(inout) :: fd
          INTEGER, intent(in) :: Nx, Nt
          INTEGER :: i

          fd%Nres = 10 ! Nombre de résultats

          ALLOCATE(fd%pres(1 : fd%Nres)) ! Allocation de la mémoire pour le tableau de positions
          ALLOCATE(fd%Eres(0 : Nt - 1,fd%Nres)) ! Allocation de la mémoire pour le tableau de résultats E, champs éléctrique
          ALLOCATE(fd%Hres(0 : Nt - 1,fd%Nres)) ! Allocation de la mémoire pour le tableau de résultats H, champs magnétique

          ! Initialisation des positions de résultats
          Do i = 1, fd%Nres
               fd%pres(i) = int( i *  Nx / fd%Nres ) ! Position de l'observation 
          END DO
          ! Initialisation des tableaux de résultats
          fd%Eres = 0.0d0
          fd%Hres = 0.0d0

          WRITE(*,'(/,T5,A,/)') "Initialisation des points d'observation terminée."
     END SUBROUTINE resultat_init  



                    !------------------------------------------------!
                    ! Allocation des tableaux et calcul coeff champ  !
                    !------------------------------------------------!
     SUBROUTINE init(fd, Nx, dt, dx)
          IMPLICIT NONE
          class(FDTD1D), intent(inout) :: fd
          INTEGER, intent(in) :: Nx
          REAL(8), intent(in) :: dt, dx

          ALLOCATE( fd%E(0 : Nx), fd%H(0 : Nx), fd%c_E(0 : Nx), fd%c_H(0 : Nx)) ! Allocation de la mémoire pour les champs E et H
          WRITE(*,'(/,T5,A,ES10.3,/)') "dt = ", dt

          ! Initialiation des champs E et H ainsi que des coefficients
          fd%E = 0.0d0
          fd%H = 0.0d0
          fd%c_E = dt / (epsilon_0 * dx)
          fd%c_H = dt / (mu_0 * dx)

          PRINT *, "c_E(0) =", fd%c_E(0)
          PRINT *, "C_h(0) =", fd%c_H(0)

          WRITE(*,'(/,T5,A,/)') "Initialisation des champs ainsi que des coefficents terminée."
     ENDSUBROUTINE init



     ! Calcul pendant les itérations temporelles
     SUBROUTINE calcule(fd, Nx, Nt, Esrc)
          IMPLICIT NONE
          class(FDTD1D), intent(inout) :: fd
          INTEGER :: Nt, Nx
          INTEGER :: i, n
          REAL(8), ALLOCATABLE :: Esrc(:)
          REAL(8) :: boundary_coef
          REAL(8) :: Etemp1, Etemp2

          WRITE(*, '(/,T5,A,I4,/)') "Nombre d'itérations temporelles : ", Nt

          boundary_coef = 0.0d0
          Etemp1 = 0.0d0
          Etemp2 = 0.0d0 

          DO n = 1, Nt - 1
               ! On applique la source
               fd%E(0) = Esrc(n-1)

               ! Calcul spatial des champs E et H
               if (n == Nt - 2 ) then
                    Etemp1 = fd%E(Nx)
                    Etemp2 = fd%E(Nx - 1)
               end if
               DO i = 1, Nx
                    ! Calcule de E(n+1)
                    if (i < Nx - 1) then
                         fd%E(i) = fd%E(i) + fd%c_E(i) * (fd%H(i) - fd%H(i - 1))
                    else
                         boundary_coef = (c * dt - dx) / (c * dt + dx)
                         fd%E(i) = Etemp2 + boundary_coef * (fd%E(i-1) - Etemp1)
                    end if
               END DO
               DO i = 0, Nx
                    ! Calcule de H(n+1)
                    fd%H(i) = fd%H(i) + fd%c_H(i) * (fd%E(i + 1) - fd%E(i))
                    !print *, "H(",i,") = ", fd%H(i)
               END DO

               DO i = 1, fd%Nres
                    ! On stocke les résultats
                    !print *, i
                    fd%Eres(n, i) = fd%E(fd%pres(i))
                    fd%Hres(n, i) = fd%H(fd%pres(i))
               END DO
          END DO 
     END SUBROUTINE calcule

     SUBROUTINE resultat_stockage(fd, Nt, dt)
          IMPLICIT NONE
          class(FDTD1D), intent(inout) :: fd
          INTEGER, intent(in) :: Nt
          real(8), intent(in) :: dt

          INTEGER :: i, n 
          INTEGER :: idfile_E, idfile_H

          ! Ouverture des fichiers de résultats
          idfile_E = 20
          idfile_H = 30
          open(idfile_E, file="E_t.txt", status='replace', action='write', form = 'formatted')
          open(idfile_H, file="H_t.txt", status='replace', action='write', form = 'formatted')

            ! Boucle sur le temps et sur les colonnes de résultats
            DO n = 0, Nt - 1
                  write(idfile_E,*) n * dt, (fd%Eres(n, i), i = 1, fd%Nres)
                  write(idfile_H,*) n * dt, (fd%Hres(n, i), i = 1, fd%Nres)
            END DO

          ! Fermeture des fichiers
          close(idfile_E)
          close(idfile_H)
          WRITE(*,'(/,T5,A,/)') "Stockage des résultats dans les fichiers terminée."
     ENDSUBROUTINE resultat_stockage
     

     subroutine freeMemory(fd)
          class(FDTD1D), intent(inout) :: fd

          if (allocated(fd%E))    deallocate(fd%E)
          if (allocated(fd%H))    deallocate(fd%H)
          if (allocated(fd%c_E))  deallocate(fd%c_E)
          if (allocated(fd%c_H))  deallocate(fd%c_H)
          if (allocated(fd%pres)) deallocate(fd%pres)
          if (allocated(fd%Eres)) deallocate(fd%Eres)
          if (allocated(fd%Hres)) deallocate(fd%Hres)
     end subroutine freeMemory






endmodule structure