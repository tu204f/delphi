-- Здесь создается таблица базы данных

--drop table vector;
--go
--drop table vector_value;
--go
-- ----------------------------------------------------------------------------
-- Таблица векторов, (начальный вектор всегда еденица,
-- значение - цены закрытие)
create table if not exists vector(
  id text not null,        -- ключ вектора
  date text not null,      -- дата
  time text not null
);
create unique index if not exists uq_vector_id on vector(id);
create unique index if not exists uq_vector_datetime on vector(date,time);

-- ----------------------------------------------------------------------------
-- Таблица значений веторов
create table if not exists vector_value(
  vector_id text not null,     -- ключ принадлежности к вектору
  type_value integer not null, -- тип значения
                               -- 0 - определяет вектор (опеределяющий вектор)
                               -- 1 - определяет результат
  open float not null,         -- Цена открытие
  high float not null,         -- Максимальное занчение
  low float not null,          -- Минимальное значение
  close float not null,        -- Цена закрытие
  val float not null           -- Объем
);

-- ----------------------------------------------------------------------------
-- временая таблица используется для сортировки данных
create table if not exists vector_result(
  vector_id text not null, -- ветор сравнение
  length float,            -- длина вектора
  angle float              -- угол вектора
)
