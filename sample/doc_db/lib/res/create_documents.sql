create table if not exists documents (
  _id text primary key,        -- ���������� ���� ���������
  created text not null,       -- ���� � ����� �������� ���������
  changed text not null,       -- ���� � ����� ����������� ��������
  deleted integer default 0,   -- ��������� ������
  status integer default 0     -- ������
);

create unique index if not exists index_doc_id on documents(_id);
