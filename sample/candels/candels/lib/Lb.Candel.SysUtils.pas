unit Lb.Candel.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ///<summary>Тип цены</summary>
  TTypePrice = (tpOpen,tpHigh,tpLow,tpClose);
  ///<summary>Свеча</summary>
  TCandel = record
    Date: TDateTime;
    Time: TDateTime;
    Open: Double;
    High: Double;
    Low: Double;
    Close: Double;
    Vol: Double;
    Status: Integer;
  public
    constructor Create(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double; AStatus: Integer); overload;
    constructor CreateCandel(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double); overload;
    constructor CreateCandel(ACandel: TCandel); overload;
    constructor CreateVector(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double); overload;
    constructor CreateVector(ACandel: TCandel); overload;
  end;
  TCandelList = TList<TCandel>;

/// <summary>С равниваем два числа</summary>
function GetSameValue(const AValue1, AValue2: Double; const AEpsilon: Integer): Boolean;

implementation

uses
  System.Math;

function GetSameValue(const AValue1, AValue2: Double; const AEpsilon: Integer): Boolean;
var
  xEpsilon: Double;
  //xValue,
  xLowValue, xHighValue: Double;
begin
  Result := False;
  if (AEpsilon > 0)  and (AEpsilon < 100) then
  begin
    xLowValue := Min(AValue1,AValue2);
    xHighValue := Max(AValue1,AValue2);
    xEpsilon := (xHighValue - xLowValue)/xHighValue;
    Result := AEpsilon > (xEpsilon * 100);
  end;
end;

{ TCandel }

constructor TCandel.Create(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double; AStatus: Integer);
begin
  with Self do
  begin
    Date := ADate;
    Time := ATime;
    Open := AOpen;
    High := AHigh;
    Low := ALow;
    Close := AClose;
    Vol := AVol;
    Status := AStatus;
  end;
end;

constructor TCandel.CreateCandel(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double);
begin
  Self.Create(ADate,ATime,AOpen,AHigh,ALow,AClose,AVol,0);
end;

constructor TCandel.CreateVector(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double);
begin
  Self.Create(ADate,ATime,AOpen,AHigh,ALow,AClose,AVol,1);
end;

constructor TCandel.CreateCandel(ACandel: TCandel);
begin
  Self.CreateCandel(ACandel.Date,ACandel.Time,ACandel.Open,ACandel.High,ACandel.Low,ACandel.Close,ACandel.Close);
end;

constructor TCandel.CreateVector(ACandel: TCandel);
begin
  Self.CreateVector(ACandel.Date,ACandel.Time,ACandel.Open,ACandel.High,ACandel.Low,ACandel.Close,ACandel.Close);
end;

end.
