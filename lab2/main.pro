% Copyright

implement main
    open core, file, stdio

domains
    diagnosis = перелом; грипп; вывих; пульпит; covid19.

class facts - med
    пациент : (integer ID_pac, string ФИО, string Адрес, integer Телефон).
    врач : (integer ID_doc, string Фамилия, string Специализация).
    больничный : (integer ID_pac, string Дата_открыт, string Дата_закрыт, integer ID_doc, diagnosis Диагноз).
    рецепт : (integer ID_pac, string Дата_назначения, integer ID_doc, string Препарат).

class facts
    k : (integer Sum) single.

clauses
    k(0).

class predicates  /* КАВО */
    диагнозы_пациента : (string ФИО) nondeterm.
    дата_больничного : (string ФИО) nondeterm.
    куда_везти : (string Препарат) nondeterm.
    врачи_пациента : (string ФИО) nondeterm.
    количество_пациентов : (string Фамилия) nondeterm.

clauses
% Диагнозы пациента
    диагнозы_пациента(Имя) :-
        пациент(ID, Имя, _, _),
        больничный(ID, _, _, ID_врача, Диагноз),
        врач(ID_врача, Врач, _),
        write(Диагноз, ", врач: ", Врач),
        nl,
        fail.
    диагнозы_пациента(Имя) :-
        пациент(_, Имя, _, _),
        write("\n"),
        nl.

% Дата больничного
    дата_больничного(Имя) :-
        пациент(ID, Имя, _, _),
        больничный(ID, Дата_начала, Дата_конца, _, Диагноз),
        write("с ", Дата_начала, " до ", Дата_конца, " с диагнозом ", Диагноз),
        nl,
        fail.
    дата_больничного(Имя) :-
        пациент(_, Имя, _, _),
        write("\n"),
        nl.

    куда_везти(Препарат) :-
        пациент(ID, _, Адрес, _),
        рецепт(ID, _, _, Препарат),
        write(Адрес),
        nl,
        fail.
    куда_везти(Препарат) :-
        рецепт(_, _, _, Препарат),
        write("\n"),
        nl.

    врачи_пациента(Имя) :-
        пациент(ID, Имя, _, _),
        больничный(ID, _, _, ID_врача, _),
        врач(ID_врача, Врач, Специализация),
        write(Специализация, " ", Врач),
        nl,
        fail.
    врачи_пациента(Имя) :-
        пациент(_, Имя, _, _),
        write("\n"),
        nl.

    количество_пациентов(Врач) :-
        врач(ID_врача, Врач, _),
        больничный(_, _, _, ID_врача, _),
        k(Sum),
        assert(k(Sum + 1)),
        fail.
    количество_пациентов(Врач) :-
        врач(_, Врач, _),
        k(Sum),
        write(Sum, "\n"),
        nl.
    количество_пациентов(Врач) :-
        врач(_, Врач, _),
        write("\n"),
        nl.

    run() :-
        console::init(),
        reconsult("..\\doc.txt", med),
        % X = stdio::readLine(), % ввод в консоли
        write("Диагнозы пациента:\n"),
        диагнозы_пациента("Иванов Иван Иванович"),
        %диагнозы_пациента(X),
        fail.

    run() :-
        console::init(),
        reconsult("..\\doc.txt", med),
        write("Даты больничного:\n"),
        дата_больничного("Иванов Иван Иванович"),
        fail.

    run() :-
        console::init(),
        reconsult("..\\doc.txt", med),
        write("Препарат везти\n"),
        куда_везти("мазь"),
        fail.

    run() :-
        console::init(),
        reconsult("..\\doc.txt", med),
        write("Врачи пациента:\n"),
        врачи_пациента("Волков Сергей Сергеевич"),
        fail.

    run() :-
        console::init(),
        reconsult("..\\doc.txt", med),
        write("Количество пациентов \n"),
        количество_пациентов("Морозов"),
        fail.

    run() :-
        succeed.

end implement main

goal
    console::runUtf8(main::run).
