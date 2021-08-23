create table if not exists documents (
  _id text primary key,        -- уникальный ключ документа
  created text not null,       -- дата и время создания докумнета
  changed text not null,       -- дата и время модификации докумета
  deleted integer default 0,   -- удаленный объект
  status integer default 0     -- статус
);

create unique index if not exists index_doc_id on documents(_id);
