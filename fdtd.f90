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
               fd%pres(i) = int( ((i-1) *  Nx) / fd%Nres +1) ! Position de l'observation 
          END DO

          ! Initialisation des tableaux de résultats
          fd%Eres = 0.0d0
          fd%Hres = 0.0d0

          !WRITE(*,'(/,T5,A,/)') "Initialisation des points d'observation terminée."
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
          !WRITE(*,'(/,T5,A,ES10.3,/)') "dt = ", dt

          ! Initialiation des champs E et H ainsi que des coefficients
          fd%E = 0.0d0
          fd%H = 0.0d0
          fd%c_E = dt / (epsilon_0 * dx)
          fd%c_H = dt / (mu_0 * dx)

          !PRINT *, "c_E(0) =", fd%c_E(0)
          !PRINT *, "C_h(0) =", fd%c_H(0)

          !WRITE(*,'(/,T5,A,/)') "Initialisation des champs ainsi que des coefficents terminée."
     ENDSUBROUTINE init



     ! Calcul pendant les itérations temporelles
     SUBROUTINE calcule(fd, Nx, Nt, Esrc)
          IMPLICIT NONE
          class(FDTD1D), intent(inout) :: fd
          INTEGER :: Nt, Nx, m
          INTEGER :: i, n, snapshot
          REAL(8), ALLOCATABLE :: Esrc(:) ! [0, Nt - 1 ] : source temporelle

          ! WRITE(*, '(/,T5,A,I4,/)') "Nombre d'itérations temporelles : ", Nt
          ! WRITE(*, '(/,T5,A,I4,/)') "Nombre de points spatiaux : ", Nx
          ! WRITE(*, '(/,T5,A,ES10.3,/)') "dx = ", dx
          ! WRITE(*, '(/,T5,A,ES10.3,/)') "dt = ", dt


          !--- Construire les noms "data/E.txt" et "data/H_5.txt" ---
          fnameE = trim(nameE) // ".txt"
          fnameH = trim(nameH) // ".txt"

          OPEN (100, file = fnameE, status = "replace", action = "write", form = "formatted")
          OPEN (200, file = fnameH, status = "replace", action = "write", form = "formatted")

          ! Ecriture de l'intervalle spatial
          DO i = 0 , Nx
               WRITE(100,*) i * dx
               WRITE(200,*) i * dx
          END DO

          !WRITE(*, '(/,T5,A,I4,/)') " Injection de la source en : ", i_src
          snapshot = 5

          !WRITE(*, '(/,T5,A,/)') " Enregistrement des paramètres dans les fichiers "
          OPEN(UNIT = 10, file = "params.txt", status = "replace", action = "write", form = "formatted")
               WRITE(10, *) Nt, Nx + 1, dx, dt, snapshot
          CLOSE(10)
          m = 0 ! Compteur pour les itérations
          DO n = 0, Nt - 1
               ! IF ( MOD(n, 20*snapshot) == 0 ) THEN
               !      WRITE(*, '(/,T5,A,I4,/)') "Itération temporelle : ", n
               ! END IF 
               ! On applique la source
               fd%E(0) = Esrc(n)

               ! Calcul spatial des champs E et H
               DO i = 1, Nx
                    ! Calcule de E au temps n
                    fd%E(i) = fd%E(i) + fd%c_E(i) * (fd%H(i) - fd%H(i - 1))
               END DO 

               DO i = 0, Nx - 1 
                    ! Calcule de H au temps n
                    fd%H(i) = fd%H(i) + fd%c_H(i) * (fd%E(i + 1) - fd%E(i))
               END DO
               ! condition au bord
               fd%H(Nx) = fd%H(Nx - 1)

               IF (MOD(n, snapshot) == 0) THEN
                    ! Ecriture des champs dans les fichiers
                    DO i = 0, Nx
                         WRITE(100,*) fd%E(i)
                         WRITE(200,*) fd%H(i)
                    END DO
                    m = m + 1
               END IF
               
               DO i = 1, fd%Nres
                    ! On stocke les résultatsg
                    fd%Eres(n, i) = fd%E(fd%pres(i))
                    fd%Hres(n, i) = fd%H(fd%pres(i))
               END DO


               
          END DO 

          ! Fermeture des fichiers
          CLOSE(100)
          CLOSE(200)

         ! WRITE(*, '(/,T5,A,/)') "Calcul des champs E et H terminé."
          !WRITE(*, '(/,T5,A,I4,/)') "Nombre d'itérations effectuées : ", m
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