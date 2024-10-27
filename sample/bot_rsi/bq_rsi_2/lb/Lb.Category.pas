unit Lb.Category;

interface

{$I debug.inc}

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
  TManagerCategory = class;
  TEventOnSendTrade = procedure(ASender: TObject; ASide: TTypeBuySell; AQty: Double) of object;

  ///<summary>
  /// �������� �������� �������
  ///</summary>
  TCategory = class(TObject)
  private
    FIsActive: Boolean;
    FIsReActive: Boolean;
    FActiveLevel: TOneEventLevel;
    FReActiveLevel: TOneEventLevel;
    FQty: Double;
    FSide: TTypeBuySell;
  private
    FOnChange: TNotifyEvent;
    FManagerCategory: TManagerCategory;
    procedure ActiveLevelOnIntersection(Sender: TObject);
    procedure ReActiveLevelOnIntersection(Sender: TObject);
    procedure SetSide(const Value: TTypeBuySell);
    procedure SetActiveLevelSide;
  protected
    procedure DoChange;
    procedure DoSendTrade(const ASide: TTypeBuySell; const AQty: Double);
  public
    constructor Create(const AManagerCategory: TManagerCategory); virtual;
    destructor Destroy; override;
    ///<summary>���������� �������� RSI</summary>
    procedure SetUpDateValue(const AValueRSI: Double);
    ///<summary>��������� ��������</summary>
    property ActiveLevel: TOneEventLevel read FActiveLevel;
    ///<summary>�������� ��� ����������� ��������</summary>
    property ReActiveLevel: TOneEventLevel read FReActiveLevel;
    ///<summary>�������� �������</summary>
    property IsActive: Boolean read FIsActive write FIsActive;
    ///<summary>������������� ��������</summary>
    property IsReActive: Boolean read FIsReActive write FIsReActive;
    ///<summary>���������� � ��������</summary>
    property Qty: Double read FQty write FQty;
    ///<summary>����������� ��������</summary>
    property Side: TTypeBuySell read FSide write SetSide;
    property OnChange: TNotifyEvent write FOnChange;
  end;

  ///<summary>
  /// ������ ���������, �� ��������� ������� ����������� �������;
  ///</summary>
  TCategoryList = TObjectList<TCategory>;

  ///<summary>�������� ���������</summary>
  TManagerCategory= class(TCategoryList)
  private
    FSide: TTypeBuySell;
    FOnSendTrade: TEventOnSendTrade;
    procedure SetSide(const Value: TTypeBuySell);
  protected
    procedure DoSendTrade(const ASide: TTypeBuySell; const AQty: Double);
  public
    ///<summary>�������� ��������</summary>
    function AddCategory(const ARSI, AActiveRSI, AQty: Double): Integer;
    ///<summary>�������� �������� ��������</summary>
    procedure UpDateCategory(const AIndex: Integer; const ARSI, AActiveRSI, AQty: Double);
    ///<summary>����������� ��������</summary>
    property Side: TTypeBuySell read FSide write SetSide;
  public
    procedure SetCreateCriteria(const AValueFrom, AValueTo, AStep, AReActiveValue, AQty: Double);
    procedure SetUpDateValue(const AValueRSI: Double);
    property OnSendTrade: TEventOnSendTrade write FOnSendTrade;
  end;

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

{ TCategory }

constructor TCategory.Create(const AManagerCategory: TManagerCategory);
begin
  FManagerCategory := AManagerCategory;

  FActiveLevel := TOneEventLevel.Create;
  FActiveLevel.OnIntersectionLevel := ActiveLevelOnIntersection;

  FReActiveLevel := TOneEventLevel.Create;
  FReActiveLevel.OnIntersectionLevel := ReActiveLevelOnIntersection;
end;

destructor TCategory.Destroy;
begin
  FreeAndNil(FReActiveLevel);
  FreeAndNil(FActiveLevel);
  inherited;
end;


procedure TCategory.SetActiveLevelSide;
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

procedure TCategory.SetSide(const Value: TTypeBuySell);
begin
  FSide := Value;
  SetActiveLevelSide;
end;

procedure TCategory.ActiveLevelOnIntersection(Sender: TObject);
begin
{$IFDEF DBG_CATEGORY_LEVEL}
  TLogger.Log('TCategory.ActiveLevelOnIntersection:');
{$ENDIF}
  if FIsActive then
  begin
    DoSendTrade(FSide, FQty);
    FIsActive := False;
    DoChange;
  end;
end;

procedure TCategory.ReActiveLevelOnIntersection(Sender: TObject);
begin
{$IFDEF DBG_CATEGORY_LEVEL}
  TLogger.Log('TCategory.ReActiveLevelOnIntersection:');
{$ENDIF}
  if FIsReActive then
  begin
    FIsActive := True;
    SetActiveLevelSide;
  end;
end;

procedure TCategory.SetUpDateValue(const AValueRSI: Double);

  procedure _ActiveLevelSetUpDate(const AValue: Double);
  begin
    {$IFDEF DBG_CATEGORY}
      TLogger.Log('_ActiveLevelSetUpDate');
    {$ENDIF}
    if IsActive then
      FActiveLevel.SetUpDate(AValue);
  end;

  procedure _ReActiveLevelSetUpDate(const AValue: Double);
  begin
    {$IFDEF DBG_CATEGORY}
      TLogger.Log('_ReActiveLevelSetUpDate');
    {$ENDIF}
    if not IsActive and FIsReActive then
      ReActiveLevel.SetUpDate(AValue);
  end;

begin
{$IFDEF DBG_CATEGORY}
  TLogger.Log('TCategory.SetUpDateValue:');
{$ENDIF}
  _ActiveLevelSetUpDate(AValueRSI);
  _ReActiveLevelSetUpDate(AValueRSI);
end;

procedure TCategory.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCategory.DoSendTrade(const ASide: TTypeBuySell; const AQty: Double);
begin
{$IFDEF DBG_CATEGORY_SEND}
  TLogger.Log('TCategory.DoSendTrade:');
  TLogger.LogTreeText(3,'Side: ' + GetStrToSide(ASide));
  TLogger.LogTreeText(3,'AQty: ' + AQty.ToString);
{$ENDIF}
  if Assigned(FManagerCategory) then
    FManagerCategory.DoSendTrade(ASide,AQty);
end;

{ TManagerCategory }

function TManagerCategory.AddCategory(const ARSI, AActiveRSI, AQty: Double): Integer;
var
  xCategory: TCategory;
begin
  xCategory := TCategory.Create(Self);
  xCategory.Qty := AQty;
  xCategory.ActiveLevel.Value := ARSI;
  xCategory.ReActiveLevel.Value := AActiveRSI;
  xCategory.Side := FSide;

  xCategory.IsActive := True;
  xCategory.IsReActive := True;

  Result := Self.Add(xCategory);
end;

procedure TManagerCategory.UpDateCategory(const AIndex: Integer; const ARSI,
  AActiveRSI, AQty: Double);
var
  xCategory: TCategory;
begin
  xCategory := Self.Items[AIndex];
  xCategory.Qty := AQty;
  xCategory.ActiveLevel.Value := ARSI;
  xCategory.ReActiveLevel.Value := AActiveRSI;
end;

procedure TManagerCategory.SetSide(const Value: TTypeBuySell);
begin
  FSide := Value;
  for var xC in Self do
    xC.Side := Value;
end;

procedure TManagerCategory.SetUpDateValue(const AValueRSI: Double);
begin
{$IFDEF DBG_CATEGORY}
  TLogger.Log('TManagerCategory.SetUpDateValue: ValueRSI = ' + AValueRSI.ToString);
{$ENDIF}
  for var xC in Self do
    xC.SetUpDateValue(AValueRSI);
end;

procedure TManagerCategory.DoSendTrade(const ASide: TTypeBuySell; const AQty: Double);
begin
{$IFDEF DBG_MANAGER_SEND}
  TLogger.Log('TManagerCategory.DoSendTrade:');
{$ENDIF}
  if Assigned(FOnSendTrade) then
    FOnSendTrade(Self,ASide,AQty);
end;

procedure TManagerCategory.SetCreateCriteria(const AValueFrom, AValueTo, AStep,
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

    AddCategory(xValue,xReValue,AQty);
    case FSide of
      tsBuy: xValue := xValue - AStep;
      tsSell: xValue := xValue + AStep;
    else
      Break;
    end;

  end;
end;


end.
