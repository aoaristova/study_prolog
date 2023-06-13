% Copyright

implement main
    open core, file, stdio

domains
    diagnosis = перелом; грипп; вывих; пульпит; covid19.
    specialization = терапевт; хирург; дантист.
    база = база(string Фамилия, specialization Специализация).
    диагнозы = диагнозы(diagnosis Диагноз, string Фамилия).
    дбольничный = дбольничный(string Начало, string Конец).

class facts - med
    пациент : (integer ID_pac, string ФИО, string Адрес, integer Телефон).
    врач : (integer ID_doc, string Фамилия, specialization Специализация, integer Цена_приема).
    больничный : (integer ID_pac, string Дата_открыт, string Дата_закрыт, integer ID_doc, diagnosis Диагноз).
    рецепт : (integer ID_pac, string Дата_назначения, integer ID_doc, string Препарат).

class predicates
    длина : (A*) -> integer N.
    сумма_элем : (real* List) -> real Sum.
    среднее_списка : (real* List) -> real Average determ.

clauses
    длина([]) = 0.
    длина([_ | T]) = длина(T) + 1.

    сумма_элем([]) = 0.
    сумма_элем([H | T]) = сумма_элем(T) + H.

    среднее_списка(L) = сумма_элем(L) / длина(L) :-
        длина(L) > 0.

class predicates  /* КАВО */
    диагнозы_пациента : (string ФИО) -> диагнозы* Диагнозы determ.
    дата_больничного : (string ФИО) -> дбольничный* Больничный determ.
    куда_везти : (string Препарат) -> string* Адрес determ.
    врачи_пациента : (string ФИО) -> база* Врачи determ.
    средняя_цена : (specialization Специализация) -> real Цена determ.
    количество_пациентов : (string Фамилия) -> integer Пациент.

clauses
% Диагнозы пациента
    диагнозы_пациента(Имя) = Spisok1 :-
        пациент(ID, Имя, _, _),
        !,
        Spisok1 =
            [ диагнозы(Диагноз, Врач) ||
                больничный(ID, _, _, ID_врача, Диагноз),
                врач(ID_врача, Врач, _, _)
            ].

% Дата больничного
    дата_больничного(Имя) = Spisok2 :-
        пациент(ID, Имя, _, _),
        !,
        Spisok2 = [ дбольничный(Дата_начала, Дата_конца) || больничный(ID, Дата_начала, Дата_конца, _, _) ].

    куда_везти(Препарат) = Spisok3 :-
        пациент(ID, _, Адрес, _),
        рецепт(ID, _, _, Препарат),
        !,
        Spisok3 = [ Адрес || пациент(ID, _, Адрес, _) ].

    врачи_пациента(Имя) = Spisok4 :-
        пациент(ID, Имя, _, _),
        !,
        Spisok4 =
            [ база(Врач, Специализация) ||
                больничный(ID, _, _, ID_врача, _),
                врач(ID_врача, Врач, Специализация, _)
            ].

    средняя_цена(Специализация) = среднее_списка([ Цена || врач(_, _, Специализация, Цена) ]).

    количество_пациентов(Врач) =
        длина(
            [ ФИО ||
                врач(Id_D, Врач, _, _),
                больничный(Id_P, _, _, Id_D, _),
                пациент(Id_P, ФИО, _, _)
            ]).

class predicates
    output_diagnosis : (диагнозы* Диагноза_и_Фамилия_Врача).
    output_basa : (база* База_пациентов).
    output_boleu : (дбольничный* Даты_больничного).

clauses
    output_diagnosis(L) :-
        foreach диагнозы(Диагноз, Врач) = list::getMember_nd(L) do
            writef("\t%\t\t%\n", Диагноз, Врач)
        end foreach.

    output_basa(L) :-
        foreach база(Врач, Специализация) = list::getMember_nd(L) do
            writef("\t%\t%\n", Врач, Специализация)
        end foreach.

    output_boleu(L) :-
        foreach дбольничный(Начало, Конец) = list::getMember_nd(L) do
            writef("\t%\t%\n", Начало, Конец)
        end foreach.

clauses
    run() :-
        console::init(),
        reconsult("..\\doc.txt", med),
        fail.

    run() :-
        % X = stdio::readLine(), % ввод в консоли
        X = "Иванов Иван Иванович",
        write("Пациент: ", X),
        write("Диагнозы пациента:\n"),
        writef("\t%\t\t%\n", "Диагноз", "Врач"),
        writef("\t%\n", "---------------------------"),
        output_diagnosis(диагнозы_пациента(X)),
        write("\n\n"),
        fail.

    %диагнозы_пациента(X),
    run() :-
        X = "Иванов Иван Иванович",
        write("Даты больничного у пациента ", X, ":\n"),
        writef("\t%\t\t%\n", "Начало", "Конец"),
        writef("\t%\n", "---------------------------"),
        output_boleu(дата_больничного(X)),
        write("\n\n"),
        fail.

    run() :-
        X = "мазь",
        write("Препарат везти\n"),
        L = куда_везти(X),
        write(L, "\n\n"),
        fail.

    run() :-
        X = "Волков Сергей Сергеевич",
        write("Пациент: ", X),
        write("\nВрачи пациента:\n"),
        writef("\t%\t\t%\n", "Врач", "Специализация"),
        writef("\t%\n", "-----------------------------"),
        output_basa(врачи_пациента(X)),
        write("\n\n"),
        Y = терапевт,
        L = средняя_цена(Y),
        write("Средняя цена за прием у врача со специализацией ", Y, " = ", L, "\n\n"),
        fail.

    run() :-
        X = "Морозов",
        L = количество_пациентов(X),
        write("Количество пациентов у врача ", X, "a: ", L, "\n"),
        fail.

    run() :-
        succeed.

end implement main

goal
    console::runUtf8(main::run).
