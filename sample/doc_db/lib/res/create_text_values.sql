-- Текстовые типы данных
create table if not exists text_values (
  _id_doc text not null,      -- ключ докумнета
  name text not null,         -- наименование ключа, можно на русском
  description text not null,  -- описание поля
  value text,                 -- значение
  constraint fk_documents
    foreign key (_id_doc)
    references documents(_id)
    on delete cascade
);

create index if not exists index_text_name on text_values(name);
create index if not exists index_text_description on text_values(description);
create index if not exists index_text_value on text_values(value);
