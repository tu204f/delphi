-- ----------------------------------------------------------------------------
-- таблица хранит история
-- Для текстовых данных
create table if not exists candels(
    candel_id integer, -- id
    startTime text,    -- время
    date text,         -- стандартное дата
    time text,         -- стандартное время
    open double,       -- цена открытие
    high double,       -- максимальная цена
    low double,        -- минимальная цена
    close double,      -- цена закрытие
    volume double      -- объем цены
);
create index if not exists ind_candels_id on candels(candel_id);
