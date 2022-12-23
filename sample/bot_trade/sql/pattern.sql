-- удаление таблицы
drop table if exists candels;

go
drop table if exists pattern;

go
drop table if exists pattern_structura;

go
drop table if exists pattern_candels;

-- создание таблицы количество свячей
go
create table if not exists candels(
    c_time  time,    -- время
    c_date  date,    -- дата
    c_open  double,  -- цена открытие 
    c_high  double,  -- максимальная цена
    c_low   double,  -- минимальная цена
    c_close double,  -- цена закрытие 
    c_vol   double   -- объем    
);

go
-- описание паттерна
create table if not exists pattern(
    name_pattern text,    -- на именование паттерна
    count_source integer, -- количество свячей, для опередление
    count_future integer  -- количество свячей для предсказания 
);

go
-- структура 
create table if not exists pattern_structura(
    p_id    integer -- какой свячи принадлежит pattern.rowid     
);

go
-- одна свяча шаблона
create table if not exists pattern_candels(
    s_id    integer, -- ключ структуры pattern_structura.rowid
    c_open  integer, -- цена открытие 
    c_high  integer, -- максимальная цена
    c_low   integer, -- минимальная цена
    c_close integer, -- цена закрытие 
    c_vol   integer  -- объем  
);
