// KRabTPR.cpp: определяет точку входа для консольного приложения.
//

#include "stdafx.h"
#include "stdafx.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <locale.h>
#include "conio.h"
#include <fstream>
#include <iostream> 
#include <windows.h>
//const double eps = 1e-6;
const double PI = 3.141592653589793238463;
#pragma warning(disable : 4996)
using namespace std;

static DWORD a;
HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

double NormalRaspr(float matOz, float SKO) {
    double y = 0;
    float a = (matOz - sqrtf(3) * SKO) / 10;
    float b = (matOz + sqrtf(3) * SKO) / 10;

    for (int i = 0; i < 10; i++) {
        y += a + (b - a) * rand() / RAND_MAX;
    }
    return y;
}

double RavnRaspr(double a, double b) {
	double f = a + ((rand()) / double(RAND_MAX)) * (b - a);//a+(b-a)*ξ_i
	return f;
}

double ExpRaspr(float matOz) {
    float r = (float)rand() / RAND_MAX;
    if (r < 0.00000000001) r = 0.00000000001;
	double f = -matOz * log(r);
    return f;
}

double PolomkaGruz(){
	//поломка грузовика при движении.
	double timeRemont = 0;
	double a = (double)rand() / RAND_MAX;
	int polomka = 0; //1 = 1й тип, 2 = 2й тип поломки
	if (a < 0.4) { polomka = 1; /*printf("поломка 1го типа \n");*/ }
	else if (a < 0.6 + 0.4) { polomka = 2; /*printf("поломка 2го типа \n");*/ }

	if (polomka = 1){
		timeRemont = ExpRaspr(10);
	}else if (polomka = 2){
		timeRemont = ExpRaspr(40);
	}
	
	//функция возвращает время ремонта грузовика, в зависимости от типа поломки.
	return timeRemont;
}

int minTimeOsv(float* mas, int n) {
    int m = 0;
    for (int i = 1; i < n; i++)
        if (mas[m] > mas[i])
            m = i;
    return m;
}

void Faza(float** mas, int n, int m, int* tekc, int* f) {
    float min = 1000000;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (mas[i][j] < min) {
                min = mas[i][j];
                *f = i;
                *tekc = j;
            }
        }
    }

}

int maxx(float* alt, int n) {
    int tmpI = 0;
    for (int i = 1; i < n; i++)
        if (alt[tmpI] < alt[i])
            tmpI = i;
    return tmpI;
}
int minni(float* mas, int n) {
    int tmpI = 0;
    for (int i = 1; i < n; i++)
        if (mas[tmpI] > mas[i])
            tmpI = i;
    return tmpI;
}
float** Normaliz12(float** alt, int nA, int nC, bool* up) {
    float** norm = new float* [nC];
    for (int i = 0; i < nC; i++)
        norm[i] = new float[nA]();

    for (int i = 0; i < nC; i++) {
        float maxA = alt[i][maxx(alt[i], nA)];
        float minA = alt[i][minni(alt[i], nA)];
        if (up[i])
            for (int j = 0; j < nA; j++)
                norm[i][j] = (alt[i][j] - minA) / (maxA - minA);
        else
            for (int j = 0; j < nA; j++)
                norm[i][j] = (maxA - alt[i][j]) / (maxA - minA);
    }
    return norm;
}
float** Normaliz34(float** alt, int nA, int nC, bool* up) {
    float** norm = new float* [nC];
    for (int i = 0; i < nC; i++)
        norm[i] = new float[nA]();

    for (int i = 0; i < nC; i++) {
        if (up[i]) {
            float maxA = alt[i][maxx(alt[i], nA)];
            float minA = alt[i][minni(alt[i], nA)];
            float delta = minA <= 0 ? abs(minA) + 1 : 0;
            for (int j = 0; j < nA; j++)
                norm[i][j] = (alt[i][j] + delta) / (maxA + delta);
        }
        else {
            float minA = alt[i][minni(alt[i], nA)];
            float delta = minA <= 0 ? abs(minA) + 1 : 0;
            for (int j = 0; j < nA; j++) {
                norm[i][j] = (minA + delta) / (alt[i][j] + delta);
            }
        }
    }
    return norm;
}
short getY() {
    CONSOLE_SCREEN_BUFFER_INFO csbi = { 0 };
    GetConsoleScreenBufferInfo(hConsole, &csbi);
    return csbi.dwCursorPosition.Y;
}
void MultiSvertka(float** norm, int nA, int nC, float* prior) {
    short Y = getY();
    float maxim = 0;
    int maxI = 0;
    printf(" Мультипликативная свертка\n     |");
    for (int i = 0; i < nC; i++) {
        printf("  C%-2i |", i + 1);
    }
    printf(" sumA\n");
    for (int i = 0; i < nA; i++) {
        printf("A%-2i  |", i + 1);
        float sum = 1;
        for (int j = 0; j < nC; j++) {
            printf("% 3.2f |", norm[j][i]);
            sum *= pow(norm[j][i], prior[j]);
        }
        printf(" %3.2f\n", sum);
        if (sum > maxim) {
            maxim = sum;
            maxI = i;
        }
    }
	COORD invalid = { 0,(short)(maxI + 2 + Y) };
    FillConsoleOutputAttribute(hConsole, BACKGROUND_GREEN , 39, invalid , &a);
    printf("Alfa |");
    for (int j = 0; j < nC; j++)
        printf(" %3.2f |", prior[j]);
    printf("\n\nОптимальным решением является альтернатива А%i\n\n", maxI + 1);
}
void AdditivSvertka(float** norm, int nA, int nC, float* prior) {
    short Y = getY();
    float maxim = 0;
    int maxI = 0;
    printf(" Аддитивная свертка\n     |");
    for (int i = 0; i < nC; i++) {
        printf("  C%-2i |", i + 1);
    }
    printf(" sumA\n");
    for (int i = 0; i < nA; i++) {
        printf("A%-2i  |", i + 1);
        float sum = 0;
        for (int j = 0; j < nC; j++) {
            printf("% 3.2f |", norm[j][i]);
            sum += norm[j][i] * prior[j];
        }
        printf(" %3.2f\n", sum);
        if (sum > maxim) {
            maxim = sum;
            maxI = i;
        }
    }
	COORD invalid = { 0, (short)(maxI + 2 + Y) };
    FillConsoleOutputAttribute(hConsole, BACKGROUND_GREEN , 39, invalid, &a);
    printf("Alfa |");
    for (int j = 0; j < nC; j++)
        printf(" %3.2f |", prior[j]);
    printf("\n\nОптимальным решением является альтернатива А%i\n\n", maxI + 1);
}
void MaxiMin(float** norm, int nA, int nC) {
    short Y = getY();
    float maxim = 0;
    int maxI = 0;
    printf("  Максиминный метод\n     |");
    for (int i = 0; i < nC; i++) {
        printf(" C%-2i |", i + 1);
    }
    printf(" min\n");
    for (int i = 0; i < nA; i++) {
        printf("A%-2i  |", i + 1);
        int minJ = 0;
        for (int j = 0; j < nC; j++) {
            printf("%3.2f |", norm[j][i]);
            if (norm[minJ][i] > norm[j][i])
                minJ = j;
        }
		COORD invalid = { (short)(6 + minJ * 6),(short)(i + 2 + Y) };
        FillConsoleOutputAttribute(hConsole, BACKGROUND_GREEN , 5, invalid, &a);
        printf("%3.2f\n", norm[minJ][i]);
        if (maxim < norm[minJ][i]) {
            maxim = norm[minJ][i];
            maxI = i;
        }
    }
	COORD invalid = { 0,(short)(maxI + 2 + Y) };
    FillConsoleOutputAttribute(hConsole, BACKGROUND_INTENSITY, 6 + nC * 7, invalid , &a);
    printf("\n\nОптимальным решением является альтернатива А%i\n\n", maxI + 1);
}
void CelProg(float** norm, int nA, int nC, float p) {
    short Y = getY();
    float minim = 10;
    int minI = 0;
    float* f = new float[nC]();
    for (int i = 0; i < nC; i++) {
        f[i] = norm[i][maxx(norm[i], nA)];
    }
    printf(" Целевое программирование\n     |");
    for (int i = 0; i < nC; i++) {
        printf("  C%-2i |", i + 1);
    }
    printf(" sumA\n");
    for (int i = 0; i < nA; i++) {
        printf("A%-2i  |", i + 1);
        float sum = 0;
        for (int j = 0; j < nC; j++) {
            printf("% 3.2f |", norm[j][i]);
            sum += pow(abs(norm[j][i] - f[j]), p);
        }
        sum = powf(sum, 1.0 / p);
        printf(" %3.2f\n", sum);
        if (sum < minim) {
            minim = sum;
            minI = i;
        }
    }
	COORD invalid ={0,(short)(minI + 2 + Y)};
    FillConsoleOutputAttribute(hConsole, BACKGROUND_GREEN , 39, invalid, &a);
    printf("\n\nОптимальным решением является альтернатива А%i\n\n", minI + 1);
    delete[]f;
}
void GlavCrit(float** alt, int nA, int nC, bool* up, int mainC) {
    short Y = getY();
    printf(" Главный критерий\n      |");
    for (int i = 0; i < nC; i++)
        up[i] ? printf("    max  ") : printf("    min  ");
    printf("\n      |");
    for (int i = 0; i < nC; i++) {
        printf("     C%i  ", i + 1);
    }
    for (int i = 0; i < nA; i++) {
        printf("\n A%-2i  |", i + 1);
        for (int j = 0; j < nC; j++)
            printf("%7.2f |", alt[j][i]);
    }
    printf("\n Опт  |");
    float* C = new float[nC - 1]();
    for (int i = 0, j = 0; i < nC; i++) {
        if (i != mainC - 1) {
            float ma = alt[i][maxx(alt[i], nA)];
            float mi = alt[i][minni(alt[i], nA)];
            if (up[i]) {
                printf("%7.2f |", ma);
                C[j] = ma - (ma - mi) / 3;
            }
            else {
                printf("%7.2f |", mi);
                C[j] = mi + (ma - mi) / 3;
            }
            j++;
        }
        else printf("        |");
    }
    int res = -1;


    while (res == -1) {
        float glCr = up[mainC - 1] ? -999999999 : 999999999;
        for (int i = 0; i < nA; i++) {
            bool correct = true;
            for (int j = 0, h = 0; j < nC && correct; j++) {
                if (j != mainC - 1) {
                    if ((up[j] && C[h] > alt[j][i]) || (!up[j] && C[h] < alt[j][i]))
                        correct = false;
                    h++;
                }
            }
            if (correct && ((up[mainC - 1] && glCr < alt[mainC - 1][i]) || (!up[mainC - 1] && glCr > alt[mainC - 1][i]))) {
                glCr = alt[mainC - 1][i];
                res = i;
            }
        }
        for (int i = 0, j = 0; i < nC; i++) {
            if (i != mainC - 1) {
                if (up[i]) {
                    printf("\nC%i >= %f", i + 1, C[j]);
                    float mi = alt[i][minni(alt[i], nA)];
                    C[j] -= (C[j] - mi) / 3;

                }
                else {
                    printf("\nC%i <= %f", i + 1, C[j]);
                    float ma = alt[i][maxx(alt[i], nA)];
                    C[j] += (ma - C[j]) / 3;

                }
                j++;

            }
        }
        printf("\n");
    }
	COORD invalid = {0,(short)(res + 3 + Y)};
    FillConsoleOutputAttribute(hConsole, BACKGROUND_GREEN , 43, invalid, &a);
    printf("\nОптимальным решением является альтернатива А%i\n\n", res + 1);

}
void Ustupok(float** alt, int nA, int nC, float* prior, bool* up) {
    short Y = getY();

    float* tmp = new float[nC];
    int* im = new int[nC];

    printf(" Метод уступок\n      |");
    for (int i = 0; i < nC; i++) {
        printf("   C%-2i   ", i + 1);
    }
    for (int i = 0; i < nA; i++) {
        printf("\n A%-2i  |", i + 1);
        for (int j = 0; j < nC; j++)
            printf("%7.2f |", alt[j][i]);
    }
    printf("\n\n");
    for (int i = 0; i < nC; i++)
        tmp[i] = prior[i];

    for (int i = 0; i < nC; i++) {
        int k = maxx(tmp, nC);
        im[i] = k;
        tmp[k] = -1;
    }
    delete[] tmp;

    bool* res = new bool[nA];
    for (int i = 0; i < nA; i++)res[i] = true;

    int r, otk;
    for (int i = 0; i < nC; i++) {
        int minI = minni(alt[im[i]], nA);
        int maxI = maxx(alt[im[i]], nA);
        if (up[im[i]])
            printf("C%i -> max = %f\n", im[i] + 1, alt[im[i]][maxI]);
        else
            printf("C%i -> min = %f\n", im[i] + 1, alt[im[i]][minI]);

        float ustupka = (alt[im[i]][maxI] - alt[im[i]][minI]) / 2.5;
        do {
            ustupka *= 1.2;
            printf("Назначим уступку Z%i = %.2f\n", i + 1, ustupka);
            if (up[im[i]])
                printf("C%i >= %f ( ", im[i] + 1, alt[im[i]][maxI] - ustupka);
            else
                printf("C%i <= %f ( ", im[i] + 1, alt[im[i]][minI] + ustupka);

            otk = 0;
            for (int j = 0; j < nA; j++) {
                if (up[im[i]]) {
                    if (alt[im[i]][j] >= alt[im[i]][maxI] - ustupka && res[j]) {
                        printf("A%i ", j + 1);
                        r = j;
                    }
                    else otk++;
                }
                else {
                    if (alt[im[i]][j] <= alt[im[i]][minI] + ustupka && res[j]) {
                        printf("A%i ", j + 1);
                        r = j;
                    }
                    else otk++;
                }
            }
            printf(")\n");
        } while (otk == nA);
        if (otk == nA - 1) {
            printf("\nОптимальным решением является альтернатива А%i\n\n", r + 1);
			COORD invalid = { 0,(short)(r + 2 + Y) };
            FillConsoleOutputAttribute(hConsole, BACKGROUND_GREEN , 43, invalid, &a);
            break;
        }
        for (int j = 0; j < nA; j++) {
            if (up[im[i]]) {
                if (alt[im[i]][j] < alt[im[i]][maxI] - ustupka) res[j] = false;
            }
            else {
                if (alt[im[i]][j] > alt[im[i]][minI] + ustupka) res[j] = false;
            }
        }
    }
    if (nA - otk >= 2) {
        int zz = r;
        for (int i = 0; i < nA; i++) {
            if (up[im[0]]) {
                if (res[i] && alt[im[0]][zz] < alt[im[0]][i]) zz = i;
            }
            else
                if (res[i] && alt[im[0]][zz] > alt[im[0]][i]) zz = i;
        }
        printf("\nОптимальным решением является альтернатива А%i\n\n", zz + 1);
		COORD invalid = { 0,(short)(zz + 2 + Y) };
        FillConsoleOutputAttribute(hConsole, BACKGROUND_GREEN , 43, invalid, &a);
    }

    delete[]res;
    delete[]im;
}
void PrintResults(float** alt, int nA, int nC) {

    for (int i = 0; i < nC; i++) {
        printf("    C%-4i  |", i + 1);
    }
    for (int i = 0; i < nA; i++) {
        printf(" \n A%-2i  |", i + 1);
        for (int j = 0; j < nC; j++)
            printf("%10.2f |", alt[j][i]);
    }
    printf("\n\n");
}
void Obrabotka(float** alt, int nA, int nC, bool* up, float* prior) {
    printf("C1 - стоимость грузовиков\n");
	printf("C2 - время простоя экскаваторов (мин.)\n");
	printf("C3 - время ожидания разгрузки (мин.)\n");
	printf("\nРезультат:\n      |");
    PrintResults(alt, nA, nC);
    float** norm12 = Normaliz12(alt, nA, nC, up);
    float** norm34 = Normaliz34(alt, nA, nC, up);
    printf("Нормализация (1,2):\n      |");
    PrintResults(norm12, nA, nC);
    printf("Нормализация (3,4):\n      |");
    PrintResults(norm34, nA, nC);
    AdditivSvertka(norm12, nA, nC, prior);
    MultiSvertka(norm34, nA, nC, prior);
    MaxiMin(norm34, nA, nC);
    CelProg(norm34, nA, nC, 2);
    GlavCrit(alt, nA, nC, up, 1);
    Ustupok(alt, nA, nC, prior, up);

    for (int i = 0; i < nC; i++) {
        delete[]norm12[i];
        delete[]norm34[i];
    }
    delete[] norm12;
    delete[] norm34;
}

int _tmain(int argc, _TCHAR* argv[])
{
	setlocale(LC_ALL, "Rus");
	fstream file;
    file.open("file.txt", ios::out);
    streambuf* stream_buffer_cout = cout.rdbuf();
    streambuf* stream_buffer_file = file.rdbuf();
    cout.rdbuf(stream_buffer_file);
    cout.precision(3);
    cout.setf(ios::fixed);
	const int COUNT_GRUZ = 12;//до скольки грузовиков будем моделировать?
	const int PRICE = 480; //цена одного грузовика
	const int KOL_SMEN = 100;
	const int SMENA_MINUTES = 480; // 480 - это количество минут в 8-ми часовой смене * 100 <- моделируем для 100 смен
	const float INFIN = 900000000;

	int f=0, fl=0, nGruz, tekGruz = 0, tekRazgr = 0, tekExav = 0, rezervExav = -1;
	float prostoiEx = 0, ozidanieRazgr = 0, razgr=0;
	float pogr_osv[6] = {}, razgr_osv[2] = {};
	float ozidRazgr[COUNT_GRUZ] = { 90000000 }, prEx[COUNT_GRUZ] = { 9000000 };
	double verPolGruz = 0, verPolExkav = 0;

	//цикл по количеству грузовых машин
	for( nGruz = 2; nGruz < COUNT_GRUZ; nGruz++ ){
		
		f = 0;//фаза движения ( 0- погрузка+движение туда, 1 - разгрузка+движение назад )
		float** trucks = new float* [2]; // массив на две строки: 1 - приезд на погрузку, 2 - приезд на разгрузку
        // цикл по количеству поездок
        for (int s = 0; s < 2; s++) {
            trucks[s] = new float[nGruz];
        }
        for (int s = 0; s < nGruz; s++) {
            trucks[1][s] = INFIN;
            trucks[0][s] = 0;
        }
		tekExav = 0;//экскаватор
		ozidanieRazgr = 0;//ожидание разгрузки 
		prostoiEx = 0; //простой экскаваторов
		for(int i=0; i<6; i++)
			pogr_osv[i] = 0;  //время освобождения мест на погрузку
		for(int i=0; i<2; i++)
			razgr_osv[i] = 0;  //время освобождения мест на разгрузку
		
		cout << "#===========================================================================#\n";
		cout << "#                  Задействовано грузовиков: " << nGruz << "                 \n";
		cout << "#                  Смена длится: " << SMENA_MINUTES/60 << " часов            \n";
		cout << "#                  Количество смен: " << KOL_SMEN << " смен                  \n";
		cout << "#===========================================================================#\n";
		fl = 0;//конец смены 8 часов (480 минут)               
		do {
			// выбираем фазу путем поиска минимального числа в массиве грузовиков
            Faza(trucks, 2, nGruz, &tekGruz, &f); // нашли минимальный элемент, номер строки - фаза, номер столбца - номер грузовика
			
			if (f == 0) {//если фаза 1я
				cout <<  " Едет грузовик № " << tekGruz + 1 << "-->" << trucks[f][tekGruz]<< "";
				cout << "\n";
				tekExav = minTimeOsv(pogr_osv, 6);//выбираем место погрузки (экскаватор)
				rezervExav = -1;//резервный экскаватор не задан
				// если место занято, то машина обслуживается с его освобождения
                if (pogr_osv[tekExav] > trucks[f][tekGruz]) {
                    trucks[f][tekGruz] = pogr_osv[tekExav];
                    cout << "Экскаватор занят, грузовик загрузится с: " << trucks[f][tekGruz] << "\n";
                }
				// конец моделирования? стоп. нет? работаем дальше
                if (trucks[f][tekGruz] > SMENA_MINUTES*KOL_SMEN) { //кол-во минут в смене*кол-во смен
					fl = 1;
					cout << "#============================ Смена завершена ==============================#\n\n"; 
					break; 
				} 
				//если место было свободно, а машины не было долго, то увеличиваем простой экскаваторов
                if (trucks[f][tekGruz] > pogr_osv[tekExav] && pogr_osv[tekExav] > 0)  
					prostoiEx += trucks[f][tekGruz] - pogr_osv[tekExav];
                cout << "Грузовик становится на погрузку (экскаватор № " << tekExav + 1 << ") в " << trucks[f][tekGruz] << "мин.\n";

				if (tekExav < 3){//если экскаватор первого типа
					trucks[f][tekGruz] += NormalRaspr(30, 10);// погрузка 
				}
				else if (tekExav >= 3){//если экскаватор второго типа
					trucks[f][tekGruz] += NormalRaspr(20, 5);// погрузка
				}

				//во время погрузки с вероятностью 0.1 экскаватор может выходить из строя
				verPolExkav = (double)rand() / RAND_MAX;
				if(verPolExkav <= 0.1){//если поломался

					float RavnRasp = RavnRaspr(10,30);//сформируем время по равномерному закону
					
					for(int i=0; i<6; i++){//ищем свободный
						if(pogr_osv[i] < trucks[f][tekGruz])//если есть экскаватор свободный
						{
							rezervExav = i; //то он становится резервным
							if(tekExav != rezervExav)//но исключаем то, что это тотже самый экскаватор
								break;
						}
						
						
					}
					if( rezervExav != -1 ){//есть свободные экскаваторы?
						//замена экскаватора , время увеличивается на треть
						
						if (rezervExav < 3){//если экскаватор первого типа
							trucks[f][tekGruz] += NormalRaspr(30, 10)/3;// погрузка/3 
						}
						else if (rezervExav >= 3){//если экскаватор второго типа
							trucks[f][tekGruz] += NormalRaspr(20, 5)/3;// погрузка/3
						}
						
						cout << "!---Экскаватор  " << tekExav+1 << "сломался." << "Найден свободный экс №" << rezervExav+1 << ".\n";
					}
					else{//нет свободных экскаваторов
						trucks[f][tekGruz] += RavnRasp;//то + ремонт
					}
					// место погрузки резервного снова свободно с нового времени 
					pogr_osv[rezervExav] = trucks[f][tekGruz];
					//а поломанный сделают и освободят
					pogr_osv[tekExav] = trucks[f][tekGruz] + RavnRasp;
				}
				else {//если не ломался
					// место погрузки снова свободно с нового времени 
					pogr_osv[tekExav] = trucks[f][tekGruz];
				}
                cout << "Погрузка завершена " << trucks[f][tekGruz] << "мин.\n";
               
				// движение (туда)
				trucks[f][tekGruz] += ExpRaspr(30); 
				verPolGruz = (double)rand() / RAND_MAX;
				if(verPolGruz <= 0.1) { //если поломался
					trucks[f][tekGruz] += PolomkaGruz(); //то + ремонт
				}
                cout << "Прибытие в отвал " << trucks[f][tekGruz] << "мин.\n";

				trucks[1][tekGruz] = trucks[f][tekGruz];
                trucks[f][tekGruz] = INFIN;
                
                cout << "---------------------------------------------------------------- \n";
			}
			else if(f == 1) {//если фаза 2я
				cout << " Разгружается грузовик № " << tekGruz + 1 << "---> " << trucks[f][tekGruz] << "\n";
               
				//выбор места с мин. временем освобождения
				tekRazgr = minTimeOsv(razgr_osv, 2);//выбираем место разгрузки
				// если место занято, то машина обслуживается с его освобождения
                if (razgr_osv[tekRazgr] > trucks[f][tekGruz]) {
					ozidanieRazgr += razgr_osv[tekRazgr] - trucks[f][tekGruz];//считаю ожидание
					trucks[f][tekGruz] = razgr_osv[tekRazgr];//грузовик разгрузится с нового времени
                    cout << "Место " << (tekRazgr+1) <<" занято, грузовик разгрузится с: " << trucks[f][tekGruz] << "\n";
                }
				
                cout << "Грузовик становится на разгрузку (место № " << tekRazgr + 1 << ") в " << trucks[f][tekGruz] << "мин.\n";
				trucks[f][tekGruz] += ExpRaspr(20); // разгрузка
                cout << "Разгрузка завершена " << trucks[f][tekGruz] << "мин.\n";
                // место погрузки снова свободно с нового времени 
                razgr_osv[tekRazgr] = trucks[f][tekGruz];
			
				// машина сразу уезжает назад на погрузку 
				trucks[f][tekGruz] += ExpRaspr(20);  // движение
				verPolGruz = (double)rand() / RAND_MAX;
				if(verPolGruz <= 0.05) { //если поломался
					trucks[f][tekGruz] += PolomkaGruz(); // то + ремонт
				}
                cout << "Грузовик вернулся на карьер в " << trucks[f][tekGruz] << " мин.\n";
                trucks[0][tekGruz] = trucks[f][tekGruz];
                trucks[f][tekGruz] = INFIN;
				cout << "---------------------------------------------------------------- \n";
			}

		}while(fl == 0);//пока не конец смены - все работают

		ozidRazgr[nGruz - 1] = ozidanieRazgr;//ожидание разгрузки 
        prEx[nGruz - 1] = prostoiEx;//простой экскаваторов
		delete[] trucks;//очистка памяти
	}
	cout.rdbuf(stream_buffer_cout);
	for (int d = 1; d < COUNT_GRUZ - 1; d++) {
        cout << "Кол-во грузовиков: " << d + 1 << ", ср.ожидание разгрузки: " << ozidRazgr[d]/KOL_SMEN << ", ср.простой 6-ти экскаваторов: " << prEx[d]/KOL_SMEN << " (за смену)\n";
    }
	
	//массив результаты (для обработки методами ТПР)
	float** rezultat = new float* [3];
    for (int i = 0; i < 3; i++)
        rezultat[i] = new float[COUNT_GRUZ - 1];
    for (int j = 0; j < COUNT_GRUZ - 2; j++) {
        rezultat[0][j] = (j + 2) * PRICE;//кол-во автомобилей
        rezultat[1][j] = prEx[j + 1]/KOL_SMEN;//время простоя экскаваторов
        rezultat[2][j] = ozidRazgr[j + 1]/KOL_SMEN;//время ожидания разгрузки на местах
    }
	bool v[3] = { false, false, false };
    float prior[3] = { 0.4, 0.5, 0.1 };
	cout << "\n Стоимость грузовика: " << PRICE <<" (тыс. руб.)" <<"\n";
    Obrabotka(rezultat, COUNT_GRUZ - 2, 3, v, prior);
	file.close();
	for (int i = 0; i < 3; i++) {
        delete[] rezultat[i];
    }


	_getch();
	return 0;
}



