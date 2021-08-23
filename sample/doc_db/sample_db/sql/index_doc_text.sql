drop if exists ind_doc_value_txt_id;
drop if exists ind_doc_value_txt_value;
create index if not exists ind_doc_value_txt_id on doc_value_text(doc_id);
create index if not exists ind_doc_value_txt_value on doc_value_text(value);