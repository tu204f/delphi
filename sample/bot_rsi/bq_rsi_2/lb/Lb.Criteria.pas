unit Lb.Criteria;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Level;

type
  TManagerCriteria = class;
  TEventOnSendTrade = procedure(ASender: TObject; ASide: TTypeBuySell; AQty: Double) of object;

  ///<summary>
  /// Критерий открытие позиции
  ///</summary>
  TCriteria = class(TObject)
  private
    FIsActive: Boolean;
    FIsReActive: Boolean;
    FActiveLevel: TOneEventLevel;
    FReActiveLevel: TOneEventLevel;
    FQty: Double;
    FSide: TTypeBuySell;
  private
    FOnChange: TNotifyEvent;
    FManagerCriteria: TManagerCriteria;
    procedure ActiveLevelOnIntersection(Sender: TObject);
    procedure ReActiveLevelOnIntersection(Sender: TObject);
    procedure SetSide(const Value: TTypeBuySell);
    procedure SetActiveLevelSide;
  protected
    procedure DoChange;
    procedure DoSendTrade(const ASide: TTypeBuySell; const AQty: Double);
  public
    constructor Create(const AManagerCriteria: TManagerCriteria); virtual;
    destructor Destroy; override;
    ///<summary>Обновление значение RSI</summary>
    procedure SetUpDateValue(const AValueRSI: Double);
    ///<summary>Активация критерия</summary>
    property ActiveLevel: TOneEventLevel read FActiveLevel;
    ///<summary>Значение для реактивация критерия</summary>
    property ReActiveLevel: TOneEventLevel read FReActiveLevel;
    ///<summary>Критерий активен</summary>
    property IsActive: Boolean read FIsActive write FIsActive;
    ///<summary>Перезапустить критерий</summary>
    property IsReActive: Boolean read FIsReActive write FIsReActive;
    ///<summary>Количество — критерия</summary>
    property Qty: Double read FQty write FQty;
    ///<summary>Направление критерия</summary>
    property Side: TTypeBuySell read FSide write SetSide;
    property OnChange: TNotifyEvent write FOnChange;
  end;

  ///<summary>
  /// Список критериев, на основание которых открывается позиция;
  ///</summary>
  TCriteriaList = TObjectList<TCriteria>;

  ///<summary>Менеджер критериев</summary>
  TManagerCriteria = class(TCriteriaList)
  private
    FSide: TTypeBuySell;
    FOnSendTrade: TEventOnSendTrade;
    procedure SetSide(const Value: TTypeBuySell);
  protected
    procedure DoSendTrade(const ASide: TTypeBuySell; const AQty: Double);
  public
    ///<summary>Добавить кретерий</summary>
    function AddCriteria(const ARSI, AActiveRSI, AQty: Double): Integer;
    ///<summary>Обновить значение критерия</summary>
    procedure UpDateCriteria(const AIndex: Integer; const ARSI, AActiveRSI, AQty: Double);
    ///<summary>Напровление критерия</summary>
    property Side: TTypeBuySell read FSide write SetSide;
  public
    procedure SetCreateCriteria(const AValueFrom, AValueTo, AStep, AReActiveValue, AQty: Double);
    procedure SetUpDateValue(const AValueRSI: Double);
    property OnSendTrade: TEventOnSendTrade write FOnSendTrade;
  end;

implementation

{ TCriteria }

constructor TCriteria.Create(const AManagerCriteria: TManagerCriteria);
begin
  FManagerCriteria := AManagerCriteria;

  FActiveLevel := TOneEventLevel.Create;
  FActiveLevel.OnIntersectionLevel := ActiveLevelOnIntersection;

  FReActiveLevel := TOneEventLevel.Create;
  FReActiveLevel.OnIntersectionLevel := ReActiveLevelOnIntersection;
end;

destructor TCriteria.Destroy;
begin
  FreeAndNil(FReActiveLevel);
  FreeAndNil(FActiveLevel);
  inherited;
end;


procedure TCriteria.SetActiveLevelSide;
begin
  case FSide of
    TTypeBuySell.tsBuy: begin
      FActiveLevel.IsRepeat := False;
      FActiveLevel.WorkLevel := TIntersectionLevel.tlDownUp;

      FReActiveLevel.IsRepeat := False;
      FReActiveLevel.WorkLevel := TIntersectionLevel.tlUpDown;
    end;
    TTypeBuySell.tsSell: begin
      FActiveLevel.IsRepeat := False;
      FActiveLevel.WorkLevel := TIntersectionLevel.tlUpDown;

      FReActiveLevel.IsRepeat := False;
      FReActiveLevel.WorkLevel := TIntersectionLevel.tlDownUp;
    end;
  end;
  DoChange;
end;

procedure TCriteria.SetSide(const Value: TTypeBuySell);
begin
  FSide := Value;
  SetActiveLevelSide;
end;

procedure TCriteria.ActiveLevelOnIntersection(Sender: TObject);
begin
  if FIsActive then
  begin
    DoSendTrade(FSide, FQty);
    FIsActive := False;
    DoChange;
  end;
end;

procedure TCriteria.ReActiveLevelOnIntersection(Sender: TObject);
begin
  if FIsReActive then
  begin
    FIsActive := True;
    SetActiveLevelSide;
  end;
end;

procedure TCriteria.SetUpDateValue(const AValueRSI: Double);

  procedure _ActiveLevelSetUpDate(const AValue: Double);
  begin
    if IsActive then
      FActiveLevel.SetUpDate(AValue);
  end;

  procedure _ReActiveLevelSetUpDate(const AValue: Double);
  begin
    if not IsActive and FIsReActive then
      ReActiveLevel.SetUpDate(AValue);
  end;

begin
  _ActiveLevelSetUpDate(AValueRSI);
  _ReActiveLevelSetUpDate(AValueRSI);
end;

procedure TCriteria.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCriteria.DoSendTrade(const ASide: TTypeBuySell; const AQty: Double);
begin
  if Assigned(FManagerCriteria) then
    FManagerCriteria.DoSendTrade(ASide,AQty);
end;

{ TManagerCriteria }

function TManagerCriteria.AddCriteria(const ARSI, AActiveRSI, AQty: Double): Integer;
var
  xCriteria: TCriteria;
begin
  xCriteria := TCriteria.Create(Self);
  xCriteria.Qty := AQty;
  xCriteria.ActiveLevel.Value := ARSI;
  xCriteria.ReActiveLevel.Value := AActiveRSI;
  xCriteria.Side := FSide;
  Result := Self.Add(xCriteria);
end;

procedure TManagerCriteria.UpDateCriteria(const AIndex: Integer; const ARSI,
  AActiveRSI, AQty: Double);
var
  xCriteria: TCriteria;
begin
  xCriteria := Self.Items[AIndex];
  xCriteria.Qty := AQty;
  xCriteria.ActiveLevel.Value := ARSI;
  xCriteria.ReActiveLevel.Value := AActiveRSI;
end;

procedure TManagerCriteria.SetSide(const Value: TTypeBuySell);
begin
  FSide := Value;
  for var xC in Self do
    xC.Side := Value;
end;

procedure TManagerCriteria.SetUpDateValue(const AValueRSI: Double);
begin
  for var xC in Self do
    xC.SetUpDateValue(AValueRSI);
end;

procedure TManagerCriteria.DoSendTrade(const ASide: TTypeBuySell; const AQty: Double);
begin
  if Assigned(FOnSendTrade) then
    FOnSendTrade(Self,ASide,AQty);
end;

procedure TManagerCriteria.SetCreateCriteria(const AValueFrom, AValueTo, AStep,
  AReActiveValue, AQty: Double);

  function _IsTo(AValueTo, AValue: Double): Boolean;
  begin
    case FSide of
      tsBuy: Result := AValue > AValueTo;
      tsSell: Result := AValue < AValueTo;
    else
      Result := False;
    end;
  end;

var
  xValue, xReValue: Double;
begin
  Clear;
  xValue := AValueFrom;
  while _IsTo(AValueTo,xValue) do
  begin
    case FSide of
      tsBuy: xReValue := xValue + AReActiveValue;
      tsSell: xReValue := xValue - AReActiveValue;
    else
      xReValue := xValue;
    end;

    if xReValue > 100 then
      xReValue := 100;
    if xReValue < 0 then
      xReValue := 0;

    AddCriteria(xValue,xReValue,AQty);
    case FSide of
      tsBuy: xValue := xValue - AStep;
      tsSell: xValue := xValue + AStep;
    else
      Break;
    end;

  end;
end;


end.
