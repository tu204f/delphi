-- Удаление системных таблиц
drop table if exists version_db;
drop table if exists doc_value_text;
drop table if exists doc_value_number;
drop table if exists doc_value_bool;
drop table if exists doc;
drop table if exists folder;

-- ----------------------------------------------------------------------------
-- Версия базы данных
create table if not exists version_db(
    name text default "1.0",
    datetime text default "2021-05-04"
);
insert into version_db default values;

-- ----------------------------------------------------------------------------
-- значение, которое храниться в таблиц
-- Для текстовых данных
create table if not exists doc_value_text(
    doc_id text,
    value text   
);

-- для числовых данных
create table if not exists doc_value_number(
    doc_id text,
    value float 
);

-- Для бинарных данных
create table if not exists doc_value_blob(
    doc_id text,
    value blob 
);

-- ----------------------------------------------------------------------------
-- Таблица документа 
-- id        - документ
-- folder_id - принадлежность, к папке
-- status    - статуст документа
create table if not exists doc(
    id text not null,
    folder_id text,
    status integer default 0
);

-- ----------------------------------------------------------------------------
-- Таблица пакок, где хрониться документ
-- внутри папики может храниться другая папка
create table if not exists folder(
    id text not null,
    parent_id text,
    name text,       -- Наименование папки
    rus_name text,   -- Наименование папки - на русском 
    descript text    -- Описание папки
);