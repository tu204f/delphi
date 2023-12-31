(******************************************************************************)
(* Источник данных для построение графика                                     *)
(******************************************************************************)
unit Lb.Bybit.Candels;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.SysUtils.Candel,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline;

type
  ///<summary>Получения списка свечей</summary>
  TBybitCandels = class(TObject)
  private
    FOnChange: TNotifyEvent;
    FOnNewCandel: TNotifyEvent;
    FBybitKlineOne: TBybitKline;
    FBybitKlineAll: TBybitKline;
  private
    FOldStartTime: String;
    FSources: TCandels;
  protected
    procedure BybitKlineOneEventMessage(Sender: TObject);
    procedure BybitKlineAllEventMessage(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start(
      const ASymbol: String;
      const ALimit: Integer;
      const ACategory: TTypeCategory;
      AInterval: TTypeInterval
    );
    procedure Stop;
    property OnChange: TNotifyEvent write FOnChange;
    property OnNewCandel: TNotifyEvent write FOnNewCandel;
    property Sources: TCandels read FSources;
  end;

implementation

{ TBybitCandels }

constructor TBybitCandels.Create;
begin
  FOldStartTime := '';
  FBybitKlineOne := TBybitKline.Create;
  FBybitKlineOne.OnEventEndLoading := BybitKlineOneEventMessage;
  FBybitKlineOne.IntervalSleep := 1;

  FBybitKlineAll := TBybitKline.Create;
  FBybitKlineAll.OnEventEndLoading := BybitKlineAllEventMessage;
  FBybitKlineOne.IntervalSleep := 100;

  FSources := TCandels.Create;
end;

destructor TBybitCandels.Destroy;
begin
  FreeAndNil(FSources);
  FreeAndNil(FBybitKlineAll);
  FreeAndNil(FBybitKlineOne);
  inherited;
end;

procedure TBybitCandels.Start(const ASymbol: String; const ALimit: Integer;
  const ACategory: TTypeCategory; AInterval: TTypeInterval);
begin
  FBybitKlineAll.Category := ACategory;
  FBybitKlineAll.Symbol := ASymbol;
  FBybitKlineAll.Interval := AInterval;
  FBybitKlineAll.Limit := ALimit;
  FBybitKlineAll.Selected(5000);

  FBybitKlineOne.Category := ACategory;
  FBybitKlineOne.Symbol := ASymbol;
  FBybitKlineOne.Interval := AInterval;
  FBybitKlineOne.Limit := 1;
  FBybitKlineOne.Selected(50);
end;

procedure TBybitCandels.Stop;
begin
  FBybitKlineAll.Stop;
  FBybitKlineOne.Stop;
end;

procedure TBybitCandels.BybitKlineOneEventMessage(Sender: TObject);
var
  iCount: Integer;
  xBybitCandel: TCandelObject;
  xBybitCandels: TCandelObjectList;
  xOldDecimalSeparator: Char;
  xValue: TDateTime;
  xStartTime: String;
begin
  if FSources.Count = 0 then
    Exit;
  xBybitCandels := TCandelObjectList.Create;
  try
    SetLinearObjects(FBybitKlineAll.ListJson, xBybitCandels);
    iCount := xBybitCandels.Count;
    if iCount > 0 then
    begin
      xOldDecimalSeparator := FormatSettings.DecimalSeparator;
      FormatSettings.DecimalSeparator := '.';
      xBybitCandel := xBybitCandels[0];
      xStartTime := xBybitCandel.startTime;
      xValue := UnixToDateTime(Round(StrToUInt64Def(xStartTime,0)/1000));
      FSources.Items[0] :=
        TCandel.Create(
          xValue,                             // Дата
          xValue,                             // Время
          xBybitCandel.openPrice.ToDouble,
          xBybitCandel.highPrice.ToDouble,
          xBybitCandel.lowPrice.ToDouble,
          xBybitCandel.closePrice.ToDouble,
          xBybitCandel.volume.ToDouble
        );
      FormatSettings.DecimalSeparator := xOldDecimalSeparator;
    end;
  finally
    FreeAndNil(xBybitCandels);
  end;

  // *******************************************
  // Событие когда приходит новый бар
  if not SameText(FOldStartTime,xStartTime) then
  begin
    if Assigned(FOnNewCandel) then
      FOnNewCandel(Self);
    FOldStartTime := xStartTime;
  end;

  // *******************************************
  // СОбытие обновление
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBybitCandels.BybitKlineAllEventMessage(Sender: TObject);
var
  i, iCount: Integer;
  xBybitCandel: TCandelObject;
  xBybitCandels: TCandelObjectList;
  xOldDecimalSeparator: Char;
  xValue: TDateTime;
begin
  FSources.Clear;
  xBybitCandels := TCandelObjectList.Create;
  try
    SetLinearObjects(FBybitKlineAll.ListJson, xBybitCandels);
    iCount := xBybitCandels.Count;
    if iCount > 0 then
    begin
      xOldDecimalSeparator := FormatSettings.DecimalSeparator;
      FormatSettings.DecimalSeparator := '.';
      for i := 0 to iCount - 1 do
      begin
        xBybitCandel := xBybitCandels[i];
        xValue := UnixToDateTime(Round(StrToUInt64Def(xBybitCandel.startTime,0)/1000));
        FSources.Add(
          TCandel.Create(
            xValue,
            xValue,
            xBybitCandel.openPrice.ToDouble,
            xBybitCandel.highPrice.ToDouble,
            xBybitCandel.lowPrice.ToDouble,
            xBybitCandel.closePrice.ToDouble,
            xBybitCandel.volume.ToDouble
          )
        );
      end;
      FormatSettings.DecimalSeparator := xOldDecimalSeparator;
    end;
  finally
    FreeAndNil(xBybitCandels);
  end;
end;

end.
