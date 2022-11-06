with 
    t1 as (
        select
            "b".fio,
            "b".dt,
            "b".code,
            date_part('hour', "a".hour)::integer as hour,
            "a".info
        from "b" 
        left join "a" on "a".fio = "b".fio
        where "b".code = 1
    ),
    t2 as (
        select
            t1.fio,
            t1.dt
        from t1
    )

select * from t1;