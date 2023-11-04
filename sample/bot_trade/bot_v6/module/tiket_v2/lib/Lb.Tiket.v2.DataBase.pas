(******************************************************************************)
(* Сохроняем информация  *)
(******************************************************************************)
unit Lb.Tiket.v2.DataBase;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Tiket.v2.Life;

//procedure SetClearDeleteLife;
//procedure SetClearDropLife;
//procedure SetInsertLife(const ABuySell: Char; const AOpenValue, ACloseValue: Double);
//procedure SetUpdateLife(const ABuySell: String; const ATradeLeft: TTradeLeft);

implementation

uses
  Data.DB,
  Lb.DataModuleDB;

var
  localDataModuleDB: TDataModuleDB = nil;

function GetCreateTableSQL: String;
var
  xSQL: TStrings;
begin
  Result := '';
  xSQL := TStringList.Create;
  try
    with xSQL do
    begin
      Clear;
      Add('CREATE TABLE if not exists vir_trd(');
      Add('	_ID INTEGER PRIMARY KEY AUTOINCREMENT,');
      Add(' BuySell TEXT,');                          // Напровление
      Add('	OpenValue REAL DEFAULT 0,');              // Открытие позиции
      Add('	CloseValue REAL DEFAULT 0,');             // Закрытие позиции
      Add(' SumValue REAL DEFAULT 0,');               // Сумма открытия
      Add(' FullSumValue REAL DEFAULT 0,');           // Польня сумма отрктия
      Add(' TradeCount INTEGER  DEFAULT 0');
      Add(');');
    end;
    Result := xSQL.Text;
  finally
    FreeAndNil(xSQL);
  end;
end;

function GetDataModuleDB: TDataModuleDB;
var
  xSQL: String;
begin
  if not Assigned(localDataModuleDB) then
  begin
    localDataModuleDB := TDataModuleDB.Create(nil);
    localDataModuleDB.DefaultConnection('virtual_trade.db');
    xSQL := GetCreateTableSQL;
    localDataModuleDB.GetExecSQL(xSQL);
  end;
  Result := localDataModuleDB;
end;

procedure SetClearDeleteLife;
var
  xSQL: String;
begin
  xSQL := 'delete from vir_trd';
  GetDataModuleDB.GetExecSQL(xSQL);
end;

procedure SetClearDropLife;
var
  xSQL: String;
begin
  xSQL := 'drop table if exists vir_trd';
  GetDataModuleDB.GetExecSQL(xSQL);
  xSQL := GetCreateTableSQL;
  localDataModuleDB.GetExecSQL(xSQL);
end;

procedure SetUpdateLife(const ABuySell: String; const ATradeLeft: TTradeLeft);
var
  xSQL: TStrings;
begin
  xSQL := TStringList.Create;
  try
    with xSQL do
    begin
      Add('update vir_trd set');
      Add('SumValue = :SumValue,');
      Add('FullSumValue = :FullSumValue,');
      Add('TradeCount = :TradeCount');
      Add('where');
      Add('BuySell = :BuySell and');
      Add('OpenValue = :OpenValue and');
      Add('CloseValue = :CloseValue');
    end;

    GetDataModuleDB.GetExecSQL(
      xSQL.Text,
      [
        ATradeLeft.SumValue,
        ATradeLeft.FullSumValue,
        ATradeLeft.Trades.Count,
        ABuySell,
        ATradeLeft.OpenValue,
        ATradeLeft.CloseValue
      ],
      [ftFloat,ftFloat,ftInteger,ftString,ftFloat,ftFloat]
    );

  finally
    FreeAndNil(xSQL);
  end;
end;

procedure SetInsertLife(const ABuySell: Char; const AOpenValue, ACloseValue: Double);
var
  xSQL: String;
begin
  xSQL := 'insert into vir_trd(BuySell,OpenValue,CloseValue)';
  xSQL := xSQL + ' values (?,?,?)';

  GetDataModuleDB.GetExecSQL(xSQL,
    [ABuySell,AOpenValue,ACloseValue],
    [ftString,ftFloat,ftFloat]
  );
end;



initialization

finalization
  if Assigned(localDataModuleDB) then
    FreeAndNil(localDataModuleDB);

end.
