unit Lb.Table.CSV;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections,
  Lb.SysUtils;

type
  TTableCSV = class(TObject)
  public type
    TPosition = record
      OpenTime: TDateTime;
      OpenPrice: Double;
      CloseTime: TDateTime;
      ClosePrice: Double;
      Qty: Double;
      Side: TTypeBuySell;
      StopLoss: Double;
      TakeProfit: Double;
      Profit: Double;
      TypeTrade: TTypeTrade;
      RSI: Double;
      MaRSI: Double;
    public
      procedure SetParserStr(const AValue: String);
    end;
  private
    FSources: TStrings;
    function GetRowCount: Integer;
    function GetPositions(Index: Integer): TTableCSV.TPosition;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Sources: TStrings read FSources;
    property Positions[Index: Integer]: TTableCSV.TPosition read GetPositions;
    property RowCount: Integer read GetRowCount;
  end;

implementation

{ TTableCSV.TPosition }

procedure TTableCSV.TPosition.SetParserStr(const AValue: String);

  procedure _Paraser(AValue: String; AStr: TStrings);
  var
    xC: Char;
    xS: String;
  begin
    xS := '';
    for xC in AValue do
    begin
      if xC = ';' then
      begin
        if not xS.IsEmpty then
        begin
          AStr.Add(xS);
          xS := '';
        end;
      end
      else
        xS := xS + xC;
    end;
    if not xS.IsEmpty then
      AStr.Add(xS);
  end;

  function _GetToStr(AIndex: Integer; AStr: TStrings): String;
  begin
    Result := '';
    if (AIndex >= 0) and (AIndex < AStr.Count) then
      Result := AStr[AIndex];
  end;

var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    _Paraser(AValue,xStr);

    OpenTime  := StrToDateTimeDef(_GetToStr(1,xStr),0);
    OpenPrice := StrToFloatDef(_GetToStr(2,xStr),0);
    CloseTime := StrToDateTimeDef(_GetToStr(3,xStr),0);
    ClosePrice:= StrToFloatDef(_GetToStr(4,xStr),0);
    Qty       := StrToFloatDef(_GetToStr(5,xStr),0);
    Side      := GetSiseToStr(_GetToStr(6,xStr));
    StopLoss  := StrToFloatDef(_GetToStr(7,xStr),0);
    TakeProfit:= StrToFloatDef(_GetToStr(8,xStr),0);
    Profit    := StrToFloatDef(_GetToStr(9,xStr),0);
    TypeTrade := GetTypeTradeToStr(_GetToStr(10,xStr));
    RSI       := StrToFloatDef(_GetToStr(11,xStr),0);
    MaRSI     := StrToFloatDef(_GetToStr(12,xStr),0);

  finally
    FreeAndNil(xStr);
  end;
end;

{ TTableCSV }

constructor TTableCSV.Create;
begin
  FSources := TStringList.Create;
end;

destructor TTableCSV.Destroy;
begin
  FreeAndNil(FSources);
  inherited;
end;

function TTableCSV.GetPositions(Index: Integer): TTableCSV.TPosition;
var
  xPosition: TTableCSV.TPosition;
begin
  xPosition.SetParserStr(FSources[Index]);
  Result := xPosition;
end;

function TTableCSV.GetRowCount: Integer;
begin
  Result := FSources.Count;
end;

end.
