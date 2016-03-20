# v.5.1.0 2016-03-20

W poprzednich deweloperskich wersjach zostały dodane wszyskie funkcje tworzone poza pakietem. Równocześnie zostały wprowadzone różne drobne poprawki i zmiany. Tym samym kończy się etap porządkowania i jest to moment na zmianę wersji pakietu z 4 na 5.

Zmiany nie zostały wytestowane. Testy odbędą się w boju.  

# v.5.0.0.9005 2016-03-20

* Zmiana nazwy parametru na plt_type w plotCalibr.
* Dodany plik preliminary i funkcje prel*.
* Drobne zmiany w funkcjach reg_nieparam i plotCalibr
*  w reg_nieparam zmiana nazwy parametru target na plt_type
*  w dopasowanie_do_zmiennej zmiana koloru i typu punktu


# v.5.0.0.9004 xxxxxx

* Dodanie w funkcji reg_nieparam rysowania dopasowania modelu logistycznego do zmiennej
* Dodanie funkcji dopasowanie_do_zmiennej
* Dodanie funkcji make_model_formula
* Dodanie funkcji przypisz_z_listy
* Zmiana nazwy z przypisz_woe na przypisz_woe_z_listy
* Dodanie funkcji step_bez_kor

# v.5.0.0.9002 2016-03-09

* Dodanie funkcji univariate_loop 
* Obsłużenie zbyt dużej liczby poziomów zmiennej kategorycznej
* inne pomniejsze rzeczy

# v.5.0.0.9002 2016-03-01
Dodane funkcje do generowania kodu SQL.

# v.5.0.0.9001 2016-02-29
Dużo małych rzeczy.

# v.5.0.0.9000 2016-02-24
Przenoszenie funkcji z pliku "funkcje do analiz.r"

* Rozbicie pliku bdclassing na classing_assign i classing_make

* Stworzenie plików:
  * classing_make_tree - do tworzenia drzew
  * raport - do generowania raportu HTML 
  * univariate - funkcje zaczynające się na univariate
  * meta_i_sterujace - parametry sterujące funkcjami i procesem oraz funkcje zarządzające metadanymi
  * util - drobne funkcje pomocnicze 
  * ScoreCard - funkcje dla klasy ScoreCard
  * other - pozostałe funkcje, które nie wiem jeszcze gdzie umieścić

# v.4.5.1 2016-02-22

* Usunięte z repozytorium git:
  * katalog ver 4.4
  * plik arch.zip

Tym samym pozbyłem się całej historii pakietu. W razie potrzeby można będzie ją znaleźć w poprzednich wersjach na GitHub.

* Dodanie 1 do numeru wersji, żeby być zgodnym ze standardem. 
  
# v.4.5 2016-02-22

* buckety_br - dodana wersja z wagami
* usun_konce - dodana wersja z wagami
* wydzielenie pliku bdclassing
* dodanie lub nowa wersja funkcji do pliku bdclassing:
  * przypisz2
  * mapuj
  * polacz_buckety

# Wcześniejsze wersje

Zmiany w poprzednich wersjach pakietu można znaleźć w pliku README.old. 