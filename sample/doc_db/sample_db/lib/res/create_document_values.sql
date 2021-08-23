create table if not exists document_values (
  _id_doc text not null,   -- ключ докумнета
  name text not null,          -- наименование ключа, можно на русском
  description text not null,   -- описание поля
  value text,                  -- значение
  type integer,                -- тип значение
  constraint fk_documents
    foreign key (_id_doc)
    references documents(_id)
    on delete cascade
);

create index if not exists index_doc_name on document_values(name);
create index if not exists index_doc_description on document_values(description);
create index if not exists index_doc_value on document_values(value);
