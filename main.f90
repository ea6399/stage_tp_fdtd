PROGRAM FDTD_1D
      use numerics
      use structure
      use source

      IMPLICIT NONE
      ! Classe FDTD1D
      type(FDTD1D) :: fd
      real(8) :: R

      ! Initialisation de vecteurs
      call init_vectors()






      !----------------------------------------!
      ! 1D FDTD simulation of a Gaussian pulse !
      !----------------------------------------!

      ! Initialisation des résultats
      call fd%resultat_init( Nx, Nt)
      
      ! Initialisation des champs
      call fd%init(Nx, dt, dx)

      !Initialisation gaussienne
      call compute_gauss(Esrc, base_t, Nt)

      ! Affichage de la gaussienne
      CALL display_gauss("gauss.txt", Esrc, base_t)

      ! Calcul des champs E et H
      call fd%calcule(Nx, Nt, Esrc)

      ! Stockage des résultats
      call fd%resultat_stockage(Nt, dt)

      ! Affichage reflexion et transmission
      R = 1 - sqrt(4.d0 * epsilon_0)
      R = R / (1 + sqrt(4.d0 * epsilon_0))
      CALL reflexion(R)
      CALL transmission(R)


      ! Libération de la mémoire
      DEALLOCATE(Esrc)
      DEALLOCATE(base_t)
      ! Dans le type FDTD1D
      call fd%freeMemory()
      WRITE(*, '(/,T5,A,/)') "Libération de la mémoire terminée."



      CONTAINS

      ! Ecriture des résultats dans un fichier
      SUBROUTINE display_gauss(file_name, e, base)
            IMPLICIT NONE
            ! Arguments
            CHARACTER(len=*), intent(in) :: file_name
            REAL(8), intent(in) :: e(LBD: UBD)
            REAL(8), intent(in) :: base(LBD: UBD)

            ! Variables locales
            INTEGER :: n
            

            ! Affichage de la gaussienne temporelle
            open(idfile, file=file_name, status='replace', action='write', form = 'formatted')
                  DO n = LBD, UBD
                        write(idfile,*) base(n) , e(n)
                  END DO
            close(idfile)

      END SUBROUTINE display_gauss

      subroutine reflexion(R)
            REAL(8), intent(in) :: R

            write(*, '(/,T5,A,ES10.3,/)') "Reflexion de la gaussienne temporelle :", R

      end subroutine reflexion

      subroutine transmission(R)
            ! Variables locales     
            REAL(8), intent(in) :: R        
                                                           ! T = 1 + R
            write(*, '(/,T5,A,ES10.3,/)') "Transmission de la gaussienne temporelle :", 1.d0 + R

      end subroutine transmission   


END PROGRAM FDTD_1D