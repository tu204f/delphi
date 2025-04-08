unit Lb.Pair;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Defaults,
  System.Generics.Collections,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.Bybit.Tickers;

type
  TDoubleList = TList<Double>;

  TRatePair = class(TObject)
  private
    FQtyF: Double;
    FQtyS: Double;
    FValues: TDoubleList;
    FSecurityF: TBybitKline;
    FSecurityS: TBybitKline;
    procedure SecurityFOnEventEndLoading(Sender: TObject);
    procedure SecuritySOnEventEndLoading(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start(ASymbolF, ASymbolS: String; AQtyS, AQtyF: Double);
    procedure Stop;
    procedure CompiledSecurity;
  end;

implementation

{ TRatePair }

constructor TRatePair.Create;
begin
  // Инструмент - эмитации срочного рынка
  FSecurityF := TBybitKline.Create;
  FSecurityF.OnEventEndLoading := SecurityFOnEventEndLoading;

  // Инструмент - эмитации спот рынка
  FSecurityS := TBybitKline.Create;
  FSecurityS.OnEventEndLoading := SecuritySOnEventEndLoading;

  FValues := TDoubleList.Create;
end;

destructor TRatePair.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FSecurityF);
  FreeAndNil(FSecurityS);
  inherited;
end;

procedure TRatePair.SecurityFOnEventEndLoading(Sender: TObject);
begin
  // Загрузка работы системы
end;

procedure TRatePair.SecuritySOnEventEndLoading(Sender: TObject);
begin
  // Загрузка работы системы
end;

procedure TRatePair.Start(ASymbolF, ASymbolS: String; AQtyS, AQtyF: Double);
begin
  FSecurityF.Symbol := ASymbolF;
  FSecurityF.Interval := TTypeInterval.ti_1;
  FSecurityF.Start(100);

  FSecurityS.Symbol := ASymbolS;
  FSecurityS.Interval := TTypeInterval.ti_1;
  FSecurityS.Start(100);
end;

procedure TRatePair.Stop;
begin
  FSecurityF.Stop;
  FSecurityS.Stop;
end;

procedure TRatePair.CompiledSecurity;

  procedure _SetCompiled;
  var
    i, iCount: Integer;
    xRate, xPriceF, xPriceS: Double;
    xCandelF, xCandelS: TCandelObject;
  begin
    FValues.Clear;
    iCount := FSecurityF.CandelObjects.Count;
    for i := 0 to iCount - 1 do
    begin
      xCandelF := FSecurityF.CandelObjects[i];
      xCandelS := FSecurityS.CandelObjects[i];
      xPriceF := xCandelF.Close;
      xPriceS := xCandelS.Close;
      xRate := xPriceF * FQtyF - xPriceS * FQtyS;
      FValues.Add(xRate);
    end;
  end;

begin
  if FSecurityF.CandelObjects.Count > 0 then
    if FSecurityF.CandelObjects.Count = FSecurityS.CandelObjects.Count then
      _SetCompiled;
end;


end.
