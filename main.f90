PROGRAM FDTD_1D
      use numerics
      use structure
      use source

      IMPLICIT NONE
      ! Classe FDTD1D
      type(FDTD1D) :: fd
      real(8) :: R, e_max, e_min, T_trms
      INTEGER :: pos

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

      pos = 50 
      write(*, '(/,T5,A,I3,/)') "Position de l'observateur : ", pos
      ! CALL reflexion(R)
      ! CALL transmission(R)
      e_max = maxval(fd%Eres(:,pos))
      e_min = minval(fd%Eres(:,pos))
      R = e_min/e_max
      write(*, '(/,T5,A,F17.14,/)') "reflexion : ", abs(R)

      call fresnel_reflexion()

      T_trms = 1.d0 + R
      write(*, '(/,T5,A,F17.14,/)') "Transmission : ", T_trms

      call fresnel_transmission()



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

      subroutine fresnel_reflexion()
            implicit none       
            Real(8) :: R

            R = 1 - sqrt(4.D0)
            R = R / (1 + sqrt(4.D0))
            Write(*, '(/,T5,A,F17.14,/)') "Fresnel Reflexion : ", abs(R)
      end subroutine fresnel_reflexion

      subroutine fresnel_transmission()
            ! Variables Arguments 
            implicit none
            Real(8) :: R
            Real(8) :: T

            R = 1 - sqrt(4.D0)
            R = R / (1 + sqrt(4.D0))
            T = 1.d0 + R                                               ! T = 1 + R
            write(*, '(/,T5,A,F17.14,/)') "Fresnel Transmission : ", T

      end subroutine fresnel_transmission  


END PROGRAM FDTD_1D