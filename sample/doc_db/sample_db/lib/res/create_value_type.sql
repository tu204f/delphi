-- create table if not exists value_type (
create table value_type (
  type integer not null,       -- ��� �������
  name text not null,          -- ������������ ���� ������
  description text not null    -- �������� ���� ������
);

create unique index if not exists index_value_type_type on value_type(type);
