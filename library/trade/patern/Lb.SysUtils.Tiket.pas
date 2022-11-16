unit Lb.SysUtils.Tiket;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TTiket = record
    Date: TDateTime;
    Time: TDateTime;
    Price: Double;
    Vol: Integer;
  public
    constructor Create(ADate, ATime: TDateTime; APrice: Double; AVol: Integer); overload;
    constructor Create(ATiket: TTiket); overload;
    constructor Cretae(AValue: String); overload;
  end;


  TMemoryTiket = class(TStringList)
  private
    //function GetTiket(Index: Integer): Double;
    //procedure SetTiket(Index: Integer; const Value: Double);
  public

    //property Tiket[Index: Integer]: Double read GetTiket write SetTiket;
  end;

implementation

{ TTiket }

constructor TTiket.Create(ADate, ATime: TDateTime; APrice: Double; AVol: Integer);
begin
  Date := ADate;
  Time := ATime;
  Price:= APrice;
  Vol  := AVol;
end;

constructor TTiket.Create(ATiket: TTiket);
begin
  Self.Create(ATiket.Date,ATiket.Time,ATiket.Price,ATiket.Vol);
end;

constructor TTiket.Cretae(AValue: String);

  function GetStr(AStrings: TStrings; AIndex: Integer): String;
  begin
    Result := '';
    if not Assigned(AStrings) then
      Exit;
    if (AIndex >= 0) and (AIndex < AStrings.Count) then
      Result := AStrings[AIndex];
  end;


var
  xStr: TStrings;
begin
  //<TICKER>;<DATE>;<TIME>;<LAST>;<VOL>
  //SBER;20221111;095933;137.900000000;80
  //<TICKER>;<DATE>;<TIME>;<LAST>;<VOL>
  //SPFB.SBRF-12.22;20221111;090000;13760.000000000;1

  xStr := TStringList.Create;
  try
    xStr.Delimiter := ';';
    xStr.DelimitedText := AValue;




  finally
    FreeAndNil(xStr);
  end;
end;

end.
