
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <math.h>
using namespace std;

const int  Nt = 1200; // Nbre d'�chantillons temps
const int  Nx = 500;  // Nbre de cellules en espace
const float  EPSILON_0 = 8.854187818e-12;
const float  MU_0      = 12.5663706144e-7;
const float  PI = 3.141592654;

class Fdtd1d
{
  public :
    float  *E,*H;
    float  *c_E, *c_H; // coefficient pour E et H
    int Nres; // Nombre de r�sultats
    int *pres;  // position du point d'observation pour chaque r�sultat
    float **Eres,**Hres; // tableau 2D h�bergeant les r�sultats
  
    void resultat_init(int Nx, int Nt);
    void init(int Nx, float dt, float dx);
    void calcul(int Nx, int Nt,float *Esrc);
    void resultat_stockage(int Nt, float dt);
	      

};

//---------------------------------------------------------------------
//---------------------------------------------------------------------
int main(int argc, char *argv[])
{
  Fdtd1d  fd;
  int i;
  float Esrc[Nt];
  cout <<"==================== FDTD 1D =================" << endl;
  // Fr�quence max d'�tude
  float fmax    = 1e9;

  //--- Calcul du pas spatial et du pas temporel
  float c  = 1 / sqrt(EPSILON_0*MU_0);
  // 30 cellules pour la longueur d'onde min
  float dx = (c/fmax) / 30 ;
  // crit�re de stabilit� dt <= dx/c
  float dt = 0.98*dx / c;

  // Calcul de la fonction d'excitation : une gaussienne  
  float T  = sqrt(log(10.0))/(PI*fmax);
  float t0 = T * sqrt(log(1000.0));
  // Esrc[i] = ???????????
  //?????????
  //??????????
  
  //--- Stockage de la gaussienne ---
  ofstream fichier_sortie("gauss.dat");
  fichier_sortie.setf(ios::scientific);
  for (i=0;i<Nt;i++)
    fichier_sortie << i*dt<<"   "<< Esrc[i] <<endl;
  fichier_sortie.close();  
  
  //--- Initialisation ---
  fd.resultat_init(Nx,Nt);
  fd.init(Nx,dt,dx);
  
  //--- Calcul ---
  fd.calcul(Nx,Nt,Esrc);
  
  //--- Stockage des r�sultats ---
  fd.resultat_stockage(Nt,dt);

}

//---------------------------------------------------------------------
//---------------------------------------------------------------------


//-------------------------------------------------------------------
//--------------- D�finition des points d'observation -----------------
//-------------------------------------------------------------------
void Fdtd1d::resultat_init(int Nx, int Nt)
{
  int n,i;
  
  Nres = 4;
  pres = new int [Nres];
  Eres = new float *[Nres];
  Hres = new float *[Nres];
  for (i=0;i<Nres;i++)
  {
    Eres[i] = new float [Nt];
    Hres[i] = new float [Nt];
  }
  //Eres = new **float [Nt][Nres];
//  Hres = new float [Nt][Nres];
  pres[0] = 1;
  pres[1] = 100;
  pres[2] = 200;
  pres[3] = Nx;

  for (i=0;i<Nres;i++)
  {  
    for (n=0;n<Nt;n++)
    {
      Eres[i][n] = 0.0;
      Hres[i][n] = 0.0;
    }
  }
}

//-------------------------------------------------------------------
//---------- Allocation des tableaux et calcul coeff champ  -----------
//-------------------------------------------------------------------
void Fdtd1d::init(int Nx, float dt, float dx)
{
  int i;
  E = new float [Nx+1];
  H = new float [Nx+1];
  c_E = new float [Nx+1];
  c_H = new float [Nx+1];
  cout <<"dt = "<<dt<< endl;
  for (i=0;i<Nx+1;i++)
    {
      E[i]=0.0;
      H[i]=0.0;
      c_E[i] = dt / (dx*EPSILON_0);
      c_H[i] = dt / (dx*MU_0);
    }
  cout <<"c_E[0]  = "<<c_E[0]<< endl;
  cout <<"c_H[0]  = "<<c_H[0]<< endl;
}



//-------------------------------------------------------------------
//------------ Calcul pendant les it�rations temporelles  -----------
//-------------------------------------------------------------------
void Fdtd1d::calcul(int Nx, int Nt,float *Esrc)
{
  int i,n;
//--- D�but des traitements ---

//-------------- boucle sur le temps -----------------
cout <<"Nombre d''it�rations temporelles :"<<Nt<< endl;
for (n=0;n<Nt;n++)
{  
  //----------- boucles sur l'espace  ------------
  //?????????????????
  
  
  //------------- Pour le stockage -----------
  for (i=0;i<Nres;i++)
  {
    Eres[i][n] = E[pres[i]];
    Hres[i][n] = H[pres[i]];
  }
} // fin boucle sur le temps
  
  
}

//-------------------------------------------------------------------
//-------------------------------------------------------------------
void Fdtd1d::resultat_stockage(int Nt, float dt)
{
  int n,i;
  //--- Stockage champ E et H ---
  ofstream fichier_sortie1("E_t.dat");
  ofstream fichier_sortie2("H_t.dat");
  fichier_sortie1.setf(ios::scientific);
  fichier_sortie2.setf(ios::scientific);
  for (n=0;n<Nt;n++)
  {
    fichier_sortie1 <<n*dt;
    fichier_sortie2 <<n*dt;
    for (i=0;i<Nres;i++)
    {
      fichier_sortie1 << "  " <<Eres[i][n];
      fichier_sortie2 << "  " <<Hres[i][n];
    }
    fichier_sortie1 <<endl;
    fichier_sortie2 <<endl;
  }
  fichier_sortie1.close();  
  fichier_sortie2.close();  
  
}


