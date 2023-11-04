(******************************************************************************)
(* Сортировка и выборка лучьщих
(******************************************************************************)
unit Lb.Sort;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections,
  Lb.Trader;

type
  TInfoTrader = record
    TraderID: Integer;
    ProfitResult: Double;
    Age: Integer;
  end;
  TInfoTraderList = TList<TInfoTrader>;

procedure SetInsetrTraders(const ATraders: TTraderManList);
procedure SetSeleted(AInfoTrades: TInfoTraderList);

implementation

uses
  Data.DB,
  Lb.DataModuleDB;

var
  localDB: TDataModuleDB = nil;

function GetCreateDB: TDataModuleDB;
var
  xSQL: String;
begin
  if not Assigned(localDB) then
  begin
    localDB := TDataModuleDB.Create(nil);
    localDB.DefaultConnection;
    xSQL :=
      'create table if not exists trader(' + sLineBreak +
      ' TraderID int,' + sLineBreak +
      ' ProfitResult double,' + sLineBreak +
      ' Age int' + sLineBreak +
      ');';
    localDB.GetExecSQL(xSQL);
  end;
  Result := localDB;
end;

procedure SetDeleteTraders;
var
  xSQL: String;
begin
  xSQL := 'delete from trader';
  GetCreateDB.GetExecSQL(xSQL);
end;

procedure SetInsertTrader(const ATrader: TTraderMan);
var
  xSQL: String;
begin
  xSQL := 'insert into trader(TraderID,ProfitResult,Age)' + sLineBreak +
          'values (:TraderID, :ProfitResult, :Age)';
  GetCreateDB.GetExecSQL(xSQL,
    [
      ATrader.ID,
      ATrader.Transaction.ProfitResult,
      ATrader.Age
    ]);
end;

procedure SetInsetrTraders(const ATraders: TTraderManList);
begin
  SetDeleteTraders;
  for var xTrader in ATraders do
    SetInsertTrader(xTrader);
end;


procedure SetSeleted(AInfoTrades: TInfoTraderList);
var
  xDataSet: TDataSet;
  xInfoTrader: TInfoTrader;
  xSQL: String;
begin
  AInfoTrades.Clear;

  xSQL :=
    'select * from trader' + sLineBreak +
    'where ProfitResult > 0' + sLineBreak +
    'order by Age desc, ProfitResult desc';

  xDataSet := GetCreateDB.GetSelectCreateDataSet(xSQL);
  if Assigned(xDataSet) then
  begin
    xDataSet.First;
    while not xDataSet.Eof do
    begin
      xInfoTrader.TraderID := xDataSet.FieldByName('TraderID').AsInteger;
      xInfoTrader.ProfitResult := xDataSet.FieldByName('ProfitResult').AsFloat;
      xInfoTrader.Age := xDataSet.FieldByName('Age').AsInteger;
      AInfoTrades.Add(xInfoTrader);
      xDataSet.Next;
    end;
    FreeAndNil(xDataSet);
  end;
end;


initialization

finalization
  if Assigned(localDB) then
    FreeAndNil(localDB);

end.
