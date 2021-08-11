create table if not exists document_values (
  _id_doc text not null,   -- ���� ���������
  name text not null,          -- ������������ �����, ����� �� �������
  description text not null,   -- �������� ����
  value text,                  -- ��������
  type integer,                -- ��� ��������
  constraint fk_documents
    foreign key (_id_doc)
    references documents(_id)
    on delete cascade
);

create index if not exists index_doc_name on document_values(name);
create index if not exists index_doc_description on document_values(description);
create index if not exists index_doc_value on document_values(value);
