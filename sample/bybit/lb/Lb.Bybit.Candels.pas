(******************************************************************************)
(* �������� ������ ��� ���������� �������                                     *)
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
    procedure BybitKlineOneEventMessage(ASender: TObject; AMessage: String);
    procedure BybitKlineAllEventMessage(ASender: TObject; AMessage: String);
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

uses
  Lb.Logger;

{ TBybitCandels }

constructor TBybitCandels.Create;
begin
  FOldStartTime := '';
  FBybitKlineOne := TBybitKline.Create;
  FBybitKlineOne.OnEventMessage := BybitKlineOneEventMessage;
  FBybitKlineOne.IntervalSleep := 1;

  FBybitKlineAll := TBybitKline.Create;
  FBybitKlineAll.OnEventMessage := BybitKlineAllEventMessage;
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
  FBybitKlineAll.Start(True);

  FBybitKlineOne.Category := ACategory;
  FBybitKlineOne.Symbol := ASymbol;
  FBybitKlineOne.Interval := AInterval;
  FBybitKlineOne.Limit := 1;
  FBybitKlineOne.Start(True);
end;

procedure TBybitCandels.Stop;
begin
  FBybitKlineAll.Stop;
  FBybitKlineOne.Stop;
end;

procedure TBybitCandels.BybitKlineOneEventMessage(ASender: TObject; AMessage: String);
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
          xValue,                             // ����
          xValue,                             // �����
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
  // ������� ����� �������� ����� ���
  if not SameText(FOldStartTime,xStartTime) then
  begin
    if Assigned(FOnNewCandel) then
      FOnNewCandel(Self);
    FOldStartTime := xStartTime;
  end;

  // *******************************************
  // ������� ����������
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBybitCandels.BybitKlineAllEventMessage(ASender: TObject; AMessage: String);
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
