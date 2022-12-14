; =============================================================================
;
; CEL:  Zmiana kolejnosci wyswietlania atrybutow w blokach, zgodnie z kluczem
; podanym w pliku tekstowym.
;
; ROZWIAZANIE:  Prezentowana w blokach z atrybutami kolejnosc atrybutow zalezy
; od porzadku, w jakim ich definicje zostaly dodane do bazy danych dwg / dxf.
; Program wykorzystuje ta wlasnosc. Istniejace na rysunku definicje atrybutow
; (ATTDEF) kopiuje sie w sposob uporzadkowany, by nadac im wlasciwa kolejnosc.
; Najpierw program wybiera z rysunku istniejace definicje atrybutow (ATTDEF),
; sczytuje ich etykiety, by sprawdzic czy zostaly wpisane na liste porzadkowa
; w pliku. Wskazuje to, ze powinny byc skopiowane. W nastepnym kroku, wykonuje
; sie kopie odpowiednich entycji, przy czym kolejnosc kopiowania jest odwrotna
; od podanej w pliku kolejnosci etykiet. Na koniec usuwa sie te entycje ATTDEF,
; dla ktorych zostaly wykonane kopie.
;
; ROZRUCH:
; 1) Otworz plik rysunkowy z definicjami atrybutow, ktore chcesz uporzadkowac.
;    Sprawdz, czy leza one na odblokowanych warstwach oraz czy maja etykiety
;    unikalne w calym rysunku.
;
; 2) Utworz plik tekstowy i wypisz w kolejnych liniach te etykiety atrybutow,
;    ktore chcesz uszeregowac - zgodnie z wpisana kolejnoscia.
;
; 3) Zapisz ww. plik w odpowiedniej lokalizacji albo dostosuj wpis do zmiennej
;    KeyFile (w kodzie skyptu ponizej), podajac pelna sciezke dostepu i nazwe
;    tego pliku.
;
; 4) Jesli to konieczne, zmodyfikuj wpis do zmiennej SelFilter, aby okreslic
;    (za pomoca listy grup DXF) inne kryterium wyboru entycji przewidzianych
;    do uporzadkowania. Np. lista '((0 . "ATTDEF") (8 . "0") (7 . "ATTR"))
;    zapewni wyselekcjonowanie z rysunku wszystkich definicji atrybutow, ktore
;    leza na warstwie "0" oraz maja nadany styl tekstu "ATTR".
;
; 5) Zapisz ewentualne zmiany w skrypcie
;
; 6) Wczytaj ten skrypt w AutoCAD (Narzedzia -> Wczytaj aplikacje) i uruchom
;    polecenie RECREATE
;
; AUTOR: Artur Cyrwus                                      DATA: 14-12-2022 r.
;
; =============================================================================




; --- Ustawienia --------------------------------------------------------------

; Lista filtrujaca, do wyboru entycji z bazy danych graficznych dwg / dxf.
(setq SelFilter '((0 . "ATTDEF")))

; Domyslna pelna sciezka dostepu i nazwa pliku z uporzadkowana lista etykiet.
(setq KeyFile "d:\\attorder.txt")




; === Klasa COLLECTION ========================================================
;
; W AutoLISP prawie wszystko jest lista.
; Klasa nazywam zestaw funkcji przeznaczonych do operowania na danych-listach
; o okreslonej, mozliwie jednolitej strukturze.  W ciele funkcji nadrzednej 
; (tzw. klasie) zdefiniowane sa funkcje wykonawcze (tzw. metody) realizujace
; operacje typowe dla danej struktury i przeznaczenia klasy.
;
; Klasa COLLECTION zawiera metody realizujace typowe operacje na kolekcjach,
; tj.: dodawanie, wstawianie, usuwanie, wyszukiwanie i modyfikowanie elementow.
; Przyklad uzycia:
;   (COLLECTION 'Delete (list Users 13))
;
; Uruchamianie metod ma jednolita konwencje:
; - nazwa funkcji nadrzednej (nazwa klasy)
; - 1szy argument funkcji-klasy - zawsze nazwa metody
; - 2gi arugment funkcji-klasy - zawsze lista argumentow dla funkcji-metody
;   przy czym "glowa" tej listy to dane na ktorych metoda ma pracowac.
;

(defun COLLECTION (method args / items)

  ; --- Zwraca element z okreslonej pozycji kolekcji --------------------------

  (defun At (clist index)
    (nth index clist)
  )

  ; --- Dodaje element na koncu kolekcji --------------------------------------

  (defun Add (clist item)
    (append clist (list
      item
    ))
  )

  ; --- Zwraca pozycje elementu w kolekcji ------------------------------------

  (defun Find (clist item / result)
    (setq result
      (member item clist)
    )
    (if (/= result nil)
      (-
        (length clist)
        (length result)
      )
    )
  )

  ; --- Usuwa element z okreslonej pozycji kolekcji ---------------------------

  (defun Delete (clist index / result i)
    (setq i 0)
    ; Przepisanie elementow znajdujacych sie przed usuwanym ...
    (repeat index
      (setq
        result (append result (list
         (nth i clist)
        ))
        i (1+ i)
      )
    )
    ; ... pominiecie elementu usuwanego ...
    (setq i (1+ i))
    ; ... i przepisanie wszystkich pozostalych elementow kolekcji zrodlowej
    (repeat (- (length clist) i)
      (setq
        result (append result (list
          (nth i clist)
        ))
        i (1+ i)
      )
    )
    result
  )

  ; --- Dodaje element w okreslonej pozycji kolekcji --------------------------

  (defun Insert (clist index item / result i)
    (setq i 0)
    ; Przepisanie elementow znajdujacych sie przed dodawanym ...
    (repeat index
      (setq
        result (append result (list
          (nth i clist)
        ))
        i (1+ i)
      )
    )
    ; ... dodanie elementu wstawianego ...
    (setq result
      (append result (list 
        item
      ))
    )
    ; ... i przepisanie wszystkich pozostalych elementow kolekcji zrodlowej
    (repeat (- (length clist) i)
      (setq
        result (append result (list
          (nth i clist)
        ))
        i (1+ i)
      )
    )
    result
  )

  ; --- Modyfikuje element w okreslonej pozycji kolekcji ----------------------

  (defun Modify (clist index item / result i)
    (setq i 0)
    ; Przepisanie elementow znajdujacych sie przed modyfikowanym ...
    (repeat index
      (setq
        result (append result (list
          (nth i clist)
        ))
        i (1+ i)
      )
    )
    ; ... dopisanie nowej wersji elementu ...
    (setq result
      (append result (list
        item
      ))
    )
    ; ... pominiecie starej wersji elementu ...
    (setq i (1+ i))
    ; ... i przepisanie wszystkich pozostalych elementow kolekcji zrodlowej
    (repeat (- (length clist) i)
      (setq
        result (append result (list
          (nth i clist)
        ))
        i (1+ i)
      )
    )
    result
  )

  ; --- ... -------------------------------------------------------------------
  (setq items (nth 0 args))
  (cond
    ; --- Metody publiczne klasy COLLECTION ---
    ((= method 'At)     (At items (nth 1 args)))
    ((= method 'Add)    (Add items (nth 1 args)))
    ((= method 'Find)   (Find items (nth 1 args)))
    ((= method 'Delete) (Delete items (nth 1 args)))
    ((= method 'Insert) (Insert items (nth 1 args) (nth 2 args)))
    ((= method 'Modify) (Modify items (nth 1 args) (nth 2 args)))
  )
)




; === Odczytanie pliku tekstowego =============================================

(defun FileRead (name / file data record)
  ; Otwarcie pliku do odczytu
  (setq file (open name "r"))
  (if (/= file nil)
    (progn
      ; Odczytywanie rekordow dopoki nie osiagnieto konca pliku
      (while (setq record (read-line file))
        (setq data
          (append data (list
            record
          ))
        )
      )
      ; Zamkniecie pliku
      (close file)
    )
  )
  ; Zwrocenie listy rekordow
  data
)




; === Funkcja polecenia =======================================================


(defun C:RECREATE ( / TagList AssocList SelSet EntName AttTag i j )

  ; --- Etap 1 : Uwtorzenie roboczych zbiorow z danymi ------------------------
  
  ; Odczytanie z pliku tekstowego uporzadkowanej listy etykiet atrybutow ...
  (setq TagList (FileRead KeyFile))
  ; ... ze sprawdzeniem, czy udalo sie cokolwiek wczytac, ...
  (if (= TagList nil)
    ; ... bo jesli nie, to ...
    (progn
      ; ... wypisanie komunikatu na konsoli ...
      (print (strcat "Nie udalo sie wczytac listy etykiet z pliku " KeyFile))
      ; ... i zakonczenie programu
      (exit)
    )
  )

  ; Utworzenie zbioru entycji zgodnych z lista filtrujaca ...
  (setq SelSet (ssget "X" SelFilter))
  ; ... ze sprawdzeniem, czy cokolwiek udalo sie znalezc, ...
  (if (= SelSet nil)
    ; ... bo jesli nie, to ...
    (progn
      ; ... wypisanie komunikatu na konsoli ...
      (print "Nie znaleziono definicji atrybutow w biezacym rysunku")
      ; ... i zakonczenie programu
      (exit)
    )
  )

  ; --- Etap 2 : Przygotowanie listy asocjacji --------------------------------

  ; Zainicjowanie pustej listy asocjacji
  (setq AssocList (list))

  ; Ze wszystkich elementow w zbiorze selekcji wybranie tylko istotnych entycji
  ; i utworzenie z nich listy par etykieta atrybutu <-> nazwa entycji, czyli: ...
  (repeat (setq i (sslength SelSet))
    (setq
      i (1- i)
      ; ... pobranie nazwy entycji ATTDEF ...
      EntName (ssname SelSet i)
      ; ... sczytanie etykiety atrybutu ...
      AttTag (cdr (assoc 2 (entget EntName)))
    )
    ; ... wyszukanie tej etykiety na liscie porzadkowej ...
    (setq j (COLLECTION 'Find (list TagList AttTag)))
    ; ... i jesli znaleziono ja tam ...
    (if (/= j nil)
      ; ... dopisanie nowej pary na liste asocjacji
      (setq AssocList (COLLECTION 'Add (list AssocList
        (cons
          AttTag    ; etykieta atrybutu
          EntName   ; nazwa entycji
        )
      )))
    )
  )

  ; --- Etap 3 : Wykonanie kopii atrybutow wpisanych na liste asocjacji -------
  ;             (w kolejnosci odwrotnej niz podano na liscie porzadkowej)

  ; Zapamietanie biezacych ustawien srodowiska ...
  (setq
    _osmode (getvar "osmode")
    _cmdecho (getvar "cmdecho")
  )
  ; ... i zmiana tych ustawien, na czas wykonywania programu
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)

  ; Utworzenie pomocniczego punktu/wektora zerowego (przyda sie w kopiowaniu)
  (setq zero (list 0 0 0))

  ; Dla wszystkich wpisow z listy porzadkowej ...
  (repeat (setq i (length TagList))
    (setq
      i (1- i)
      ; ... pobranie etykiety atrybutu ...
      AttTag (nth i TagList)
      ; ... wyszukanie pary etykieta atrybutu <-> nazwa entycji
      EntName (cdr (assoc AttTag AssocList))
    )
    (if (/= EntName nil)
      (progn
        ; ... i jesli entycja istnieje - wykonanie jej kopii ...
        (command "._copy" (cons EntName (list zero)) "" zero "")
        ; ... z wypisaniem etykiety na konsoli
        (princ (strcat "przetwarzanie: " AttTag "\n"))
      )
    )
  )

  ; --- Etap 4 : Usuniecie enycji, dla ktorych wykonano kopie -----------------

  ; Dla wszystkich wpisow na liscie asocjacji ...
  (repeat (setq i (length AssocList))
    (setq
      i (1- i)
      ; ... pobranie nazwy entycji ...
      EntName (cdr (nth i AssocList))
    )
    ; ... i usuniecie jej z rysunku
    (entdel EntName)
  )

  ; Przywrocenie ustawien srodowiska
  (setvar "osmode" _osmode)
  (setvar "cmdecho" _cmdecho)

  (princ (strcat 
    (itoa (length AssocList))
    " atrybutow przetworzonych."
  ))
)


; =============================================================================