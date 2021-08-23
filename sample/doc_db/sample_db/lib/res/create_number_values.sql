-- Числовых типы данных
create table if not exists number_values (
  _id_doc text not null,   -- ключ докумнета
  name text not null,          -- наименование ключа, можно на русском
  description text not null,   -- описание поля
  value double,                -- значение
  constraint fk_documents
    foreign key (_id_doc)
    references documents(_id)
    on delete cascade
);

create index if not exists index_number_name on number_values(name);
create index if not exists index_number_description on number_values(description);
create index if not exists index_number_value on number_values(value);
