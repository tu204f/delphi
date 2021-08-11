-- create table if not exists value_type (
create table value_type (
  type integer not null,       -- тип данныех
  name text not null,          -- наименование типа данных
  description text not null    -- описание типа данных
);

create unique index if not exists index_value_type_type on value_type(type);
