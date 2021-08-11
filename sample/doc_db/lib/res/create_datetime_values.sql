-- Дата и время типы данных
create table if not exists datetime_values (
  _id_doc text not null,      -- ключ докумнета
  name text not null,         -- наименование ключа, можно на русском
  description text not null,  -- описание поля
  value text,                 -- значение
  constraint fk_documents
    foreign key (_id_doc)
    references documents(_id)
    on delete cascade
);

create index if not exists index_datetime_name on text_values(name);
create index if not exists index_datetime_description on text_values(description);
create index if not exists index_datetime_value on text_values(value);
