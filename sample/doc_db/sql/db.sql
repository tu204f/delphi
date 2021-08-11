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
    datetime text default "2021-05-04",
);
insert into version_db default values;

-- ----------------------------------------------------------------------------
-- значение, которое храниться в таблиц
-- Для текстовых данных
create table if not exists doc_value_text(
    doc_id text,
    name text not null,
    type_value integer not null,
    value text   
);
drop index if exists ind_doc_value_txt_id;
drop index if exists ind_doc_value_txt_value;
create index if not exists ind_doc_value_txt_id on doc_value_text(doc_id);
create index if not exists ind_doc_value_txt_value on doc_value_text(value,type_value);
create unique index if not exists uq_value_text_name on doc_value_text(doc_id,name);

-- для числовых данных
create table if not exists doc_value_number(
    doc_id text,
    name text not null,
    type_value integer not null,
    value float 
);
drop index if exists ind_doc_value_number_id;
drop index if exists ind_doc_value_number_value;
create index if not exists ind_doc_value_number_id on doc_value_number(doc_id);
create index if not exists ind_doc_value_number_value on doc_value_number(value,type_value);
create unique index if not exists uq_value_number_name on doc_value_number(doc_id,name);

-- Для бинарных данных
create table if not exists doc_value_blob(
    doc_id text,
    name text not null,
    value blob 
);
drop index if exists ind_doc_value_blob_id;
create index if not exists ind_doc_value_blob_id on doc_value_blob(doc_id);
create unique index if not exists uq_value_blob_name on doc_value_blob(doc_id,name);

-- ----------------------------------------------------------------------------
-- Таблица документа 
-- id        - документ
-- folder_id - принадлежность, к папке
-- status    - статуст документа
create table if not exists doc(
    id text not null,
    folder_id text not null,
    status integer default 0
);

drop index if exists uq_doc_id;
create unique index if not exists uq_doc_id on doc(id);

-- Целостность данных обеспециваем тригерам
-- При удадение папки нужно удалить все дочерние папки и документы
drop trigger if exists delete_doc;
create trigger delete_doc after delete on doc
begin
    delete from doc_value_text where doc_id = old.id;
    delete from doc_value_number where doc_id = old.id;
    delete from doc_value_blob where doc_id = old.id;
end;

-- ----------------------------------------------------------------------------
-- Таблица пакок, где хрониться документ
-- внутри папики может храниться другая папка
create table if not exists folder(
    id text not null,
    parent_id text,
    name text,       -- Наименование папки
    descript text    -- Описание папки
);

drop index if exists uq_folder_id;
drop index if exists uq_folder_name;
create unique index if not exists uq_folder_id on folder(id);
create unique index if not exists uq_folder_name on folder(parent_id,name);


-- Целостность данных обеспециваем тригерам
-- При удадение папки нужно удалить все дочерние папки и документы
drop trigger if exists delete_folder;
create trigger delete_folder after delete on folder
begin
    delete from folder where parent_id = old.id;
    delete from doc where folder_id = old.id;
end;