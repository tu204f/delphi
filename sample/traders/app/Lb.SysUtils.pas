unit Lb.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

const
  TRADES_PATH = 'd:\work\git\delphi\sample\traders\traders.csv';
  TRADE_PATH  = 'd:\work\git\delphi\sample\traders\source\';

type
  TInfoUser = record
    Name: String;
    Count: Integer;
    Profit: Double;
    URL: String;
  private
    function GetID: String;
  public
    property ID: String read GetID;
  end;
  TInfoUserList = TList<TInfoUser>;

  ///<summary>Трейдер</summary>
  TInfoUsers = class(TInfoUserList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure NameTraders(const ANames: TStrings);
    procedure Load(const AFileName: String = TRADES_PATH);
  end;

  // 2022-10-24 18:36:00.000;PHOR;1;6220.00000
  // 2022-10-24 18:36:00.000;PHOR;40;6225.00000

  ///<summary>Дата и время</summary>
  TDT = record
    Year: Integer;
    Month: Integer;
    Day: Integer;
    Hour: Integer;
    Minutes: Integer;
    Seconds: Integer;
    Milliseconds: Integer;
  private
    procedure SetText(const Value: String);
  public
    property Value: String write SetText;
  end;

  ///<summary>Сделка</summary>
  TMarketTrade = record
    DateTime: TDT;
    Tiker: String;      // Наименование инстурмента
    Quantity: Integer;  // Количество - Объем
    Price: Double;      // Цена
  end;

  ///<summary>Списка сделка</summary>
  TMarketTradeList = TList<TMarketTrade>;

  ///<summary>Тикер инструмент</summary>
  TTiket = class(TObject)
    Name: String;
    Count: Integer;
  end;
  TTiketList = TObjectList<TTiket>;

  ///<summary>Список котировак</summary>
  TTikets = class(TTiketList)
  protected
    function CreateTiket: TTiket;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const AName: String): Integer;
    procedure SetTiket(const AName: String);
  end;

  ///<summary>Сделки - одной плащадке</summary>
  TMarketTrades = class(TMarketTradeList)
  private
    FTikers: TTikets;
  protected
    procedure Load(const AFileName: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LaodFileName(const AFileName: String);
    procedure LoadID(const ID: String);
    procedure LoadNonClearID(const ID: String);
    property Tikers: TTikets read FTikers;
    procedure SetNameTikers(const ANames: TStrings);
  end;


implementation

uses
  System.DateUtils;

function Field(const AFeild: Integer; const ACols: TStrings): String; inline;
begin
  Result := '';
  if (AFeild >= 0) and (AFeild < ACols.Count) then
    Result := ACols[AFeild];
end;

{ TInfoUser }

function TInfoUser.GetID: String;
var
  xPosition: Integer;
begin
  // https://investor.moex.com/trader2022?user=312371
  xPosition := Pos('?user',URL) + 6;
  Result := Copy(URL,xPosition,256);
end;

{ TInfoUsers }

constructor TInfoUsers.Create;
begin
  inherited Create;

end;

destructor TInfoUsers.Destroy;
begin

  inherited;
end;

procedure TInfoUsers.Load(const AFileName: String);

  //Участник;Сделок;Доходность, %;URL
  //deprivator;47142;398,8;https://investor.moex.com/trader2022?user=312371
  function _InfoTradeToStr(const S: String): TInfoUser;
  var
    xStr: TStrings;
  begin
    xStr := TStringList.Create;
    try
      xStr.Delimiter := ';';
      xStr.DelimitedText := S;
      with Result do
      begin
        Name   := Field(0,xStr);
        Count  := StrToIntDef(Field(1,xStr),0);
        Profit := StrToFloatDef(Field(2,xStr),0);
        URL    := Field(3,xStr);
      end;
    finally
      FreeAndNil(xStr);
    end;
  end;

var
  xS: String;
  xStr: TStrings;
  xInfoTrade: TInfoUser;
begin
  xStr := TStringList.Create;
  try
    xStr.LoadFromFile(AFileName);
    xStr.Delete(0); // Удалем строку наименование полей
    for xS in xStr do
    begin
      xInfoTrade := _InfoTradeToStr(xS);
      Self.Add(xInfoTrade);
    end;
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TInfoUsers.NameTraders(const ANames: TStrings);
var
  xInfoTrade: TInfoUser;
begin
  ANames.Clear;
  for xInfoTrade in Self do
    ANames.Add(xInfoTrade.Name);
end;

{ TDT }

procedure TDT.SetText(const Value: String);

  procedure _SetFieldValue(const AIndex: Integer; S: String);
  begin
    case AIndex of
      0: Year         := StrToIntDef(S,0);
      1: Month        := StrToIntDef(S,0);
      2: Day          := StrToIntDef(S,0);
      3: Hour         := StrToIntDef(S,0);
      4: Minutes      := StrToIntDef(S,0);
      5: Seconds      := StrToIntDef(S,0);
      6: Milliseconds := StrToIntDef(S,0);
    end;
  end;

var
  xC: Char;
  tmpS: String;
  xIndField: Integer;
begin
  //2022-10-24 18:36:00.000
  tmpS := '';
  xIndField := 0;
  for xC in Value do
  begin
    if CharInSet(xC,['-',' ',':','.']) then
    begin
      _SetFieldValue(xIndField,tmpS);
      tmpS := '';
      Inc(xIndField);
    end
    else
      tmpS := tmpS + xC;
  end;
  _SetFieldValue(xIndField,tmpS);
end;

{ TTikets }

constructor TTikets.Create;
begin
  inherited;
end;

destructor TTikets.Destroy;
begin

  inherited;
end;

function TTikets.CreateTiket: TTiket;
var
  xTiket: TTiket;
begin
  xTiket := TTiket.Create;

  xTiket.Name := '';
  xTiket.Count := 0;

  Self.Add(xTiket);

  Result := xTiket;
end;

function TTikets.IndexOf(const AName: String): Integer;
var
  xTiket: TTiket;
  i, iCount: Integer;
begin
  Result := -1;
  iCount := Self.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTiket := Self.Items[i];
      if SameText(xTiket.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

procedure TTikets.SetTiket(const AName: String);
var
  xTiket: TTiket;
  xIndex: Integer;
begin
  xIndex := IndexOf(AName);
  if xIndex >= 0 then
    xTiket := Self.Items[xIndex]
  else
  begin
    xTiket := CreateTiket;
    xTiket.Name := AName;
  end;
  xTiket.Count := xTiket.Count + 1;
end;

{ TMarketTrades }

constructor TMarketTrades.Create;
begin
  inherited Create;
  FTikers := TTikets.Create;
end;

destructor TMarketTrades.Destroy;
begin
  FreeAndNil(FTikers);
  inherited;
end;

procedure TMarketTrades.Load(const AFileName: String);

  procedure _Delimiter(const Value: String; const ASource: TStrings);
  var
    xC: Char;
    tmpS: String;
  begin
    tmpS := '';
    for xC in Value do
    begin
      if xC = ';' then
      begin
        if not tmpS.IsEmpty then
          ASource.Add(tmpS);
        tmpS := '';
      end
      else
        tmpS := tmpS + xC;
    end;
    if not tmpS.IsEmpty then
      ASource.Add(tmpS);
  end;

  //2022-11-25 17:52:00.000;TRMK;-100;76.76000
  //2022-11-30 10:55:00.000;TRMK;100;78.50000
  function _TradeToStr(const S: String): TMarketTrade;
  var
    xStr: TStrings;
  begin
    xStr := TStringList.Create;
    try
      _Delimiter(S,xStr);
      with Result do
      begin
        DateTime.Value := Field(0,xStr);
        Tiker    := Field(1,xStr);
        Quantity := StrToIntDef(Field(2,xStr),0);
        Price    := StrToFloatDef(Field(3,xStr),0);
      end;
      FTikers.SetTiket(Result.Tiker);
    finally
      FreeAndNil(xStr);
    end;
  end;

var
  xS: String;
  xStr: TStrings;
  xTrade: TMarketTrade;
begin
  if not FileExists(AFileName) then
    Exit;
  xStr := TStringList.Create;
  try
    xStr.LoadFromFile(AFileName);
    xStr.Delete(0); // Удалем строку наименование полей
    for xS in xStr do
    begin
      xTrade := _TradeToStr(xS);
      Self.Add(xTrade);
    end;
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TMarketTrades.LaodFileName(const AFileName: String);
begin
  Clear;
  FTikers.Clear;
  Load(AFileName);
end;

procedure TMarketTrades.LoadID(const ID: String);
var
  xPath: String;
begin
  Clear;
  FTikers.Clear;
  LoadNonClearID(ID);
end;

procedure TMarketTrades.LoadNonClearID(const ID: String);
var
  xPath: String;
begin
  xPath := 'd:\work\git\delphi\sample\traders\source\';
  Load(xPath + '1_' + ID + '.csv');
  Load(xPath + '2_' + ID + '.csv');
  Load(xPath + '3_' + ID + '.csv');
end;

procedure TMarketTrades.SetNameTikers(const ANames: TStrings);
var
  xS: String;
begin
  ANames.Clear;
  for var xTiket in FTikers do
  begin
    xS := xTiket.Name + '[' + xTiket.Count.ToString + ']';
    ANames.Add(xS);
  end;
end;

end.
