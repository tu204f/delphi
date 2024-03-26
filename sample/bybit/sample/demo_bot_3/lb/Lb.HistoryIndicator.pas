unit Lb.HistoryIndicator;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.Indicator;

type
  ///<summary>Исторические данные, с индикатором</summary>
  THistoryIndicator = class(TObject)
  private
    FRSI: TRSI;
    FCurrentCandel: TCandelObject;
    FBybitKline: TBybitKline;
    procedure BybitKlineOnEventEndLoading(Sender: TObject);
  private
    FSymbol: String;
    FCategory: TTypeCategory;
    FInterval: TTypeInterval;
    FLimit: Integer;
    function GetPeriod: Integer;
    function GetAvgPeriod: Integer;
    procedure SetPeriod(const Value: Integer);
    procedure SetAvgPeriod(const Value: Integer);
    function GetCandels: TCandelObjectList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function UpDate: Boolean;
    property RSI: TRSI read FRSI;
    property Period: Integer read GetPeriod write SetPeriod;
    property AvgPeriod: Integer read GetAvgPeriod write SetAvgPeriod;
    property Candels: TCandelObjectList read GetCandels;
    property CurrentCandel: TCandelObject read FCurrentCandel write FCurrentCandel;
  public
    property Symbol: String read FSymbol write FSymbol;
    property Category: TTypeCategory read FCategory write FCategory;
    property Interval: TTypeInterval read FInterval write FInterval;
  public
    property BybitKline: TBybitKline read FBybitKline;
  end;

implementation

{ THistoryIndicator }

constructor THistoryIndicator.Create;
begin
  FSymbol := '';
  FCategory := TTypeCategory.tcLinear;
  FInterval := TTypeInterval.ti_5;

  FCurrentCandel := nil;

  FRSI := TRSI.Create;
  FRSI.Period := 14;
  FRSI.AvgPeriod := 3;

  FBybitKline := TBybitKline.Create;
  FBybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;
end;

destructor THistoryIndicator.Destroy;
begin
  FreeAndNil(FBybitKline);
  FreeAndNil(FRSI);
  inherited;
end;

function THistoryIndicator.GetPeriod: Integer;
begin
  Result := FRSI.Period;
end;

procedure THistoryIndicator.SetPeriod(const Value: Integer);
begin
  FLimit := Value * 10;
  if FLimit < 150 then
    FLimit := 150;
  FRSI.Period := Value;
end;

function THistoryIndicator.GetAvgPeriod: Integer;
begin
  Result := FRSI.AvgPeriod;
end;

function THistoryIndicator.GetCandels: TCandelObjectList;
begin
  Result := FBybitKline.CandelObjects;
end;

procedure THistoryIndicator.SetAvgPeriod(const Value: Integer);
begin
  FRSI.AvgPeriod := Value;
end;

procedure THistoryIndicator.BybitKlineOnEventEndLoading(Sender: TObject);
var
  iCount: Integer;
begin
  iCount := FBybitKline.CandelObjects.Count;
  if iCount > 0 then
  begin
    FRSI.SetCandels(FBybitKline.CandelObjects);
    FCurrentCandel := FBybitKline.CandelObjects[0];
  end
  else
    FCurrentCandel := nil;
end;

function THistoryIndicator.UpDate: Boolean;
begin
  Result := True;
  try
    FBybitKline.Category := FCategory;
    FBybitKline.Symbol   := FSymbol;
    FBybitKline.Interval := FInterval;
    FBybitKline.Limit    := FLimit;
    FBybitKline.Selected;
  except
    Result := False;
  end;
end;

end.
