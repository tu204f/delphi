-- �������� �������� ������
create table if not exists blob_values (
  _id_doc text not null,      -- ���� ���������
  name text not null,         -- ������������ �����, ����� �� �������
  description text not null,  -- �������� ����
  value blob,                 -- �������� ������
  constraint fk_documents
    foreign key (_id_doc)
    references documents(_id)
    on delete cascade
);

create index if not exists index_datetime_name on text_values(name);
create index if not exists index_datetime_description on text_values(description);
