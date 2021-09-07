TModules - список модулей (массив логически выделеных таблиц)
	TModule  - Логиче выделеный объект (со своим набором таблицы и методов)
		TTables - список таблиц
			TTable - таблица
				Fields: TCrFields         - список полей, таблицы
					TCrField
				Indexs: TCrIndexs         - список индексов
					TCrIndex              - индекс
						Fields: TCrFields - список полей
							TCrField
		TMethods - Список методов
			TMethod - метод
				-> Тип данных возрощаемых 
				InputFields: TCrFields  - Список входящих параметров
				InputTables: TCrTables  - Список входящих таблиц
					TTable - таблица
						Fields: TCrFields         - список полей, таблицы
							TCrField
						Indexs: TCrIndexs         - список индексов
							TCrIndex              - индекс
								Fields: TCrFields - список полей
									TCrField
				OutputTables: TCrTables - Список ихсодящих таблиц
					TTable - таблица
						Fields: TCrFields         - список полей, таблицы
						Indexs: TCrIndexs         - список индексов
							TCrIndex              - индекс
								Fields: TCrFields - список полей
									TCrField