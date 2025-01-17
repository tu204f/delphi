unit Lb.Journal.DB;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Data.DB,
  Lb.SysUtils,
  Lb.DataModuleDB,
  Lb.Journal.Trading.V2,
  FMX.Dialogs;

type
  TJournalDataBase = record
  public
    class function GetDB: TDataModuleDB; static;
    class function GetCreateKey(const ACount: Integer = 20): String; static;
    class function InsertJournalPosition(AJournalPosition: TJournalPosition): Integer; static;
    class procedure InsertJournalCandel(ATradeID: Integer; ACandel: TCandel); static;
    class procedure InsertJournalCandels(ATradeID: Integer; ACandels: TCandelList); static;
    class function InsertJournalTrade(APositionID: Integer; ATrade: TJournalTrade): Integer; static;
    class procedure InsertJournalTrades(APositionID: Integer; ATrades: TJournalTradeList); static;
    class procedure InsertJournalConditionParams(APositionID: Integer; AJournalPosition: TJournalPosition); static;
  public
    class procedure SaveJournalPositionDB(AJournalPosition: TJournalPosition); static;
  end;


implementation

var
  localDB: TDataModuleDB = nil;

{ TJournalDataBase }

class function TJournalDataBase.GetDB: TDataModuleDB;
var
  xPathDB, xFileNameDB: String;
  xDataSet: TDataSet;
begin
  if not Assigned(localDB) then
  begin
    localDB := TDataModuleDB.Create(nil);
    xPathDB := ExtractFilePath(ParamStr(0));
    xFileNameDB := xPathDB  + 'journal.db';
    localDB.DefaultConnection(xFileNameDB);
  end;
  Result := localDB;
end;


class function TJournalDataBase.GetCreateKey(const ACount: Integer = 20): String;
const
  TO_CHAR = '0123456789abcdefghijklmnopqrstuvwxyz';
var
  i: Integer;
  xS: String;
begin
  xS := '';
  for i := 0 to 4 do
    xS := xS + TO_CHAR[Random(36) + 1];
  xS := xS + '-';
  for i := 0 to 9 do
    xS := xS + TO_CHAR[Random(36) + 1];
  xS := xS + '-';
  for i := 0 to ACount - 13 do
    xS := xS + TO_CHAR[Random(36) + 1];
  Result := xS;
end;

class function TJournalDataBase.InsertJournalPosition(AJournalPosition: TJournalPosition): Integer;
var
  xSQL, xKey: String;
  xDateTime: TDateTime;
begin
  Result := -1;
  xDateTime := GetNewDateTime;
  xSQL := 'insert into position(journal_key,time) values(:journal_key,:time)';
  xKey := TJournalDataBase.GetCreateKey;
  TJournalDataBase.GetDB.GetCommandSQL(xSQL,[xKey,xDateTime],[ftString,ftFloat]);
  Result := TJournalDataBase.GetDB.GetExecSQLScalar('select last_insert_rowid() as id');
end;


class procedure TJournalDataBase.InsertJournalCandel(ATradeID: Integer; ACandel: TCandel);
var
  xSQL: String;
begin
  xSQL := 'insert into candel(trade_id,time,open,high,low,close,vol) values(:trade_id,:time,:open,:high,:low,:close,:vol)';
  TJournalDataBase.GetDB.GetExecSQL(xSQL,
    [ATradeID,ACandel.Time,ACandel.Open,ACandel.High,ACandel.Low,ACandel.Close,ACandel.Vol],
    [ftInteger,ftFloat,ftFloat,ftFloat,ftFloat,ftFloat,ftFloat]
  );
end;

class procedure TJournalDataBase.InsertJournalCandels(ATradeID: Integer; ACandels: TCandelList);
begin
  for var xCandel in ACandels do
    TJournalDataBase.InsertJournalCandel(ATradeID, xCandel);
end;

class procedure TJournalDataBase.InsertJournalConditionParams(APositionID: Integer; AJournalPosition: TJournalPosition);
var
  xSQL: String;
begin
  for var xC in AJournalPosition.ConditionParams do
  begin
    xSQL := 'insert into profit(position_id,profit,RSI,AveragRSI,ART) ' +
     'values(:position_id,:profit,:RSI,:AveragRSI,:ART)';
    TJournalDataBase.GetDB.GetExecSQL(
      xSQL,
      [APositionID,xC.Profit,xC.RSI, xC.AveragRSI,xC.ART],
      [ftInteger,  ftFloat,  ftFloat,ftFloat,     ftFloat]
    );
  end;
end;

class function TJournalDataBase.InsertJournalTrade(APositionID: Integer; ATrade: TJournalTrade): Integer;
var
  xSQL: String;
begin
  Result := -1;
  xSQL := 'insert into trade(position_id,time,price,qty,side) values(:position_id,:time,:price,:qty,:side)';
  TJournalDataBase.GetDB.GetCommandSQL(xSQL,
    [APositionID,ATrade.Time,ATrade.Price,ATrade.Qty,Integer(ATrade.Side)],
    [ftInteger,ftFloat,ftFloat,ftFloat,ftInteger]);
  Result := TJournalDataBase.GetDB.GetExecSQLScalar('select last_insert_rowid() as id');
end;

class procedure TJournalDataBase.InsertJournalTrades(APositionID: Integer; ATrades: TJournalTradeList);
begin
  for var xTrade in ATrades do
  begin
    var xTradeID := TJournalDataBase.InsertJournalTrade(APositionID,xTrade);
    TJournalDataBase.InsertJournalCandels(xTradeID,xTrade.Candels);
  end;
end;

class procedure TJournalDataBase.SaveJournalPositionDB(AJournalPosition: TJournalPosition);
var
  xID: Integer;
begin
  xID := TJournalDataBase.InsertJournalPosition(AJournalPosition);
  TJournalDataBase.InsertJournalTrades(xID,AJournalPosition.Trades);
  TJournalDataBase.InsertJournalConditionParams(xID,AJournalPosition);
end;

initialization
  {todo: Реализовать возможность создание базы данных}

finalization
  FreeAndNil(localDB);

end.
